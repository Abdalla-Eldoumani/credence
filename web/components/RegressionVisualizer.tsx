"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import { type BandRenderer, makeBandRenderer, resolveCssVar } from "@/lib/canvas";
import { engineBaseUrl } from "@/lib/config";
import {
  type Method,
  type RunRequest,
  type Subscription,
  type Summary,
  startRegressionRun,
  subscribeRun,
} from "@/lib/stream";
import styles from "./RegressionVisualizer.module.css";

// The data domain the plot covers. The slope prior is Normal(0,3) and x spans a
// few units either side of the origin, so y of +/-12 over x of +/-6 frames the
// plausible lines without clipping the band. Points drag within this domain.
const X_MIN = -6;
const X_MAX = 6;
const Y_MIN = -12;
const Y_MAX = 12;

// The CSS pixel size of the canvas coordinate space. The canvas backing store is
// scaled by devicePixelRatio for crisp lines; these are the logical dimensions.
const VIEW_W = 720;
const VIEW_H = 460;
const MARGIN = { top: 16, right: 16, bottom: 32, left: 40 };
const PLOT_W = VIEW_W - MARGIN.left - MARGIN.right;
const PLOT_H = VIEW_H - MARGIN.top - MARGIN.bottom;

// Data point radius (design system: 3px to 4px) and the touch hit target (>=44px).
const POINT_RADIUS = 4;
const HIT_RADIUS = 22;

// The keyboard nudge for the active point: one arrow press moves it half a data
// unit, small enough for precise placement across the 12-by-24 data domain.
const KEY_STEP = 0.5;

// The keyboard focus ring radius for the active point, drawn on Canvas in
// --focus-ring. Sits just outside the point so the dot stays visible inside it.
const FOCUS_RING_RADIUS = POINT_RADIUS + 4;

// The engine grows the run length batch by batch; ask for enough to converge.
const REQUESTED_SAMPLES = 20000;

// A noisy linear dataset for the one-click first-launch affordance: points near
// the line y = 1.6 x with small Gaussian-looking jitter, so a run pulls the band
// onto a clear positive slope (edge case first launch).
const SAMPLE_POINTS: [number, number][] = [
  [-5, -7.6],
  [-4, -6.9],
  [-3, -4.4],
  [-2, -3.5],
  [-1, -1.2],
  [1, 1.9],
  [2, 3.0],
  [3, 5.1],
  [4, 6.2],
  [5, 8.3],
];

type VizState = "empty" | "streaming" | "converged" | "reconnecting" | "error";

type RegressionVisualizerProps = {
  method: Method;
  // The observed points; dragging a point lifts state up via onPointsChange.
  points: [number, number][];
  onPointsChange: (points: [number, number][]) => void;
  // Bumped by the controls to (re)start a run. 0 means no run requested yet.
  runToken: number;
  // Optional taps on the single stream subscription so siblings (the traceplot and
  // diagnostics) can mount without opening a second EventSource. onRunStart fires
  // when a fresh run begins (reset point), onBatchDraws carries each slope
  // subsample, onSummary carries each running summary, onRunDone fires on the
  // terminal event so the diagnostics can flag a not-converged run.
  onRunStart?: () => void;
  onBatchDraws?: (draws: number[]) => void;
  onSummary?: (summary: Summary) => void;
  onRunDone?: () => void;
};

const toPixelX = (dataX: number): number =>
  MARGIN.left + ((dataX - X_MIN) / (X_MAX - X_MIN)) * PLOT_W;
const toPixelY = (dataY: number): number =>
  MARGIN.top + (1 - (dataY - Y_MIN) / (Y_MAX - Y_MIN)) * PLOT_H;
const toDataX = (pixelX: number): number =>
  X_MIN + ((pixelX - MARGIN.left) / PLOT_W) * (X_MAX - X_MIN);
const toDataY = (pixelY: number): number =>
  Y_MIN + (1 - (pixelY - MARGIN.top) / PLOT_H) * (Y_MAX - Y_MIN);

const clamp = (value: number, lo: number, hi: number): number => Math.max(lo, Math.min(hi, value));

export function RegressionVisualizer({
  method,
  points,
  onPointsChange,
  runToken,
  onRunStart,
  onBatchDraws,
  onSummary,
  onRunDone,
}: RegressionVisualizerProps) {
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const rendererRef = useRef<BandRenderer | null>(null);
  const subscriptionRef = useRef<Subscription | null>(null);
  const reducedMotionRef = useRef(false);

  // The stream-tap callbacks captured in refs so the run effect (gated on runToken)
  // can call the latest without listing them as deps and re-subscribing mid-run.
  const onRunStartRef = useRef(onRunStart);
  onRunStartRef.current = onRunStart;
  const onBatchDrawsRef = useRef(onBatchDraws);
  onBatchDrawsRef.current = onBatchDraws;
  const onSummaryRef = useRef(onSummary);
  onSummaryRef.current = onSummary;
  const onRunDoneRef = useRef(onRunDone);
  onRunDoneRef.current = onRunDone;

  const [state, setState] = useState<VizState>("empty");
  const [meanReadout, setMeanReadout] = useState<number | null>(null);
  const [batchReadout, setBatchReadout] = useState<number | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

  // The keyboard-active point index. -1 means no point is active. Kept in state so
  // a change repaints the focus ring and announces via aria-label, and mirrored in
  // a ref the Canvas overlay reads each frame without a stale closure.
  const [activeIndex, setActiveIndex] = useState(-1);
  const activeIndexRef = useRef(activeIndex);
  activeIndexRef.current = activeIndex;

  // Whether the canvas holds keyboard focus, so the active-point ring shows only
  // while focused (matching the CSS :focus-visible outline on the canvas itself).
  const focusedRef = useRef(false);

  // The latest points are read by the render loop's overlay without re-creating
  // the renderer; a ref keeps the drawn dots in sync with drags frame to frame.
  const pointsRef = useRef(points);
  pointsRef.current = points;

  // Whether any batch arrived this run, so onDone can mark convergence vs empty.
  const sawDataRef = useRef(false);

  // Draw the gridlines and axes under the band. Reads token colors from the canvas
  // computed style so nothing is hardcoded.
  const drawGrid = useCallback((ctx: CanvasRenderingContext2D) => {
    const canvas = ctx.canvas;
    const gridColor = resolveCssVar(canvas, "--viz-grid");
    const axisColor = resolveCssVar(canvas, "--viz-axis");

    ctx.strokeStyle = gridColor;
    ctx.lineWidth = 1;
    ctx.beginPath();
    // Vertical gridlines every 2 data-x.
    for (let x = X_MIN; x <= X_MAX; x += 2) {
      const px = Math.round(toPixelX(x)) + 0.5;
      ctx.moveTo(px, MARGIN.top);
      ctx.lineTo(px, MARGIN.top + PLOT_H);
    }
    // Horizontal gridlines every 4 data-y.
    for (let y = Y_MIN; y <= Y_MAX; y += 4) {
      const py = Math.round(toPixelY(y)) + 0.5;
      ctx.moveTo(MARGIN.left, py);
      ctx.lineTo(MARGIN.left + PLOT_W, py);
    }
    ctx.stroke();

    // The x and y axes through the origin, a touch stronger than the grid.
    ctx.strokeStyle = axisColor;
    ctx.beginPath();
    const originX = Math.round(toPixelX(0)) + 0.5;
    const originY = Math.round(toPixelY(0)) + 0.5;
    ctx.moveTo(originX, MARGIN.top);
    ctx.lineTo(originX, MARGIN.top + PLOT_H);
    ctx.moveTo(MARGIN.left, originY);
    ctx.lineTo(MARGIN.left + PLOT_W, originY);
    ctx.stroke();
  }, []);

  // Draw the observed data points on top of the band so they stay legible over the
  // wash. Reads --viz-data from the canvas computed style.
  const drawPoints = useCallback((ctx: CanvasRenderingContext2D) => {
    const canvas = ctx.canvas;
    const dataColor = resolveCssVar(canvas, "--viz-data");
    ctx.fillStyle = dataColor;
    const current = pointsRef.current;
    for (const [x, y] of current) {
      const px = toPixelX(clamp(x, X_MIN, X_MAX));
      const py = toPixelY(clamp(y, Y_MIN, Y_MAX));
      ctx.beginPath();
      ctx.arc(px, py, POINT_RADIUS, 0, Math.PI * 2);
      ctx.fill();
    }

    // Ring the keyboard-active point in --focus-ring while the canvas is focused,
    // so keyboard users see which point the arrow keys will move (design system:
    // a visible focus state on every interactive element).
    const active = activeIndexRef.current;
    if (focusedRef.current && active >= 0 && active < current.length) {
      const [ax, ay] = current[active];
      const px = toPixelX(clamp(ax, X_MIN, X_MAX));
      const py = toPixelY(clamp(ay, Y_MIN, Y_MAX));
      ctx.strokeStyle = resolveCssVar(canvas, "--focus-ring");
      ctx.lineWidth = 2;
      ctx.beginPath();
      ctx.arc(px, py, FOCUS_RING_RADIUS, 0, Math.PI * 2);
      ctx.stroke();
    }
  }, []);

  // Size the canvas backing store to the device pixel ratio and scale the context
  // so all drawing is in CSS pixels. Returns the 2D context, or null if absent.
  const setupCanvas = useCallback((): CanvasRenderingContext2D | null => {
    const canvas = canvasRef.current;
    if (!canvas) return null;
    const ctx = canvas.getContext("2d");
    if (!ctx) return null;
    const dpr = window.devicePixelRatio || 1;
    canvas.width = Math.round(VIEW_W * dpr);
    canvas.height = Math.round(VIEW_H * dpr);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    return ctx;
  }, []);

  // Build the band renderer once on mount (and rebuild if the canvas node swaps).
  // The renderer draws grid (underlay), band (capped lines), and points (overlay)
  // each frame; the slope set is fed from the stream, the points from the ref.
  useEffect(() => {
    const ctx = setupCanvas();
    if (!ctx) return;

    const renderer = makeBandRenderer(ctx, {
      toPixelX,
      toPixelY,
      xDomain: () => [X_MIN, X_MAX],
      styleSource: () => canvasRef.current,
      drawUnderlay: drawGrid,
      drawOverlay: drawPoints,
      reducedMotion: reducedMotionRef.current,
    });
    rendererRef.current = renderer;
    // Initial paint: grid + points, no band yet (the empty state shows the data
    // and axes with a calm prompt to start a run).
    renderer.redraw();

    return () => {
      renderer.stop();
      rendererRef.current = null;
    };
  }, [setupCanvas, drawGrid, drawPoints]);

  // Repaint when the points change off-stream (add, clear, sample, or a drag that
  // lifts new state) so the dots track without waiting for a batch, and when the
  // keyboard-active point changes so the focus ring moves with it. Drags also
  // redraw imperatively in the move handler for immediacy; this covers the rest.
  // points and activeIndex are the intended triggers even though redraw reads the
  // refs, so the deps are deliberate, not the body's free variables biome looks for.
  // biome-ignore lint/correctness/useExhaustiveDependencies: points and activeIndex are the repaint triggers; redraw reads the refs by design
  useEffect(() => {
    rendererRef.current?.redraw();
  }, [points, activeIndex]);

  // Keep the active index in range as the list shrinks (clear, or fewer points
  // after a reload), so the ring never references a removed point.
  useEffect(() => {
    if (activeIndex >= points.length) {
      setActiveIndex(points.length > 0 ? points.length - 1 : -1);
    }
  }, [points.length, activeIndex]);

  // Track prefers-reduced-motion and forward it to the renderer.
  useEffect(() => {
    if (typeof window === "undefined" || !window.matchMedia) return;
    const query = window.matchMedia("(prefers-reduced-motion: reduce)");
    const apply = () => {
      reducedMotionRef.current = query.matches;
      rendererRef.current?.setReducedMotion(query.matches);
    };
    apply();
    query.addEventListener("change", apply);
    return () => query.removeEventListener("change", apply);
  }, []);

  const teardown = useCallback(() => {
    subscriptionRef.current?.close();
    subscriptionRef.current = null;
  }, []);

  // Start (or restart) a run when the controls bump runToken. The run captures the
  // current method and points at the bump; depending on them here would restart the
  // stream on every drag, which the runToken gate exists to prevent.
  // biome-ignore lint/correctness/useExhaustiveDependencies: runToken gates the run; method/points are captured intentionally, not tracked
  useEffect(() => {
    if (runToken === 0) return;

    let cancelled = false;
    sawDataRef.current = false;
    setErrorMessage(null);
    setState("streaming");
    // Reset the sibling readouts (traceplot, diagnostics) for the fresh run.
    onRunStartRef.current?.();

    const request: RunRequest = {
      method,
      dataset: { tag: "Regression", points: pointsRef.current },
      samples: REQUESTED_SAMPLES,
    };

    teardown();

    const begin = async () => {
      let runId: string;
      try {
        runId = await startRegressionRun(engineBaseUrl, request);
      } catch (error) {
        if (cancelled) return;
        setErrorMessage(error instanceof Error ? error.message : "could not start the run");
        setState("error");
        return;
      }
      if (cancelled) return;

      subscriptionRef.current = subscribeRun(engineBaseUrl, runId, {
        onSummary: (summary) => {
          if (cancelled) return;
          setMeanReadout(summary.mean);
          setBatchReadout(summary.batchIndex);
          onSummaryRef.current?.(summary);
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onBatch: (batch) => {
          if (cancelled) return;
          // Each batch is a subsample of slope draws; the band eases onto them.
          sawDataRef.current = true;
          rendererRef.current?.setSlopes(batch.draws);
          onBatchDrawsRef.current?.(batch.draws);
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onDone: () => {
          if (cancelled) return;
          setState(sawDataRef.current ? "converged" : "empty");
          onRunDoneRef.current?.();
        },
        onReconnecting: () => {
          if (cancelled) return;
          // Preserve the band; just flag the state (edge case SSE drop).
          setState("reconnecting");
        },
        onError: (error) => {
          if (cancelled) return;
          setErrorMessage(error instanceof Error ? error.message : "the stream dropped");
          setState("error");
        },
      });
    };

    void begin();

    return () => {
      cancelled = true;
      teardown();
    };
  }, [runToken]);

  // Pointer drag: hit-test the nearest point within the 44px target on pointer
  // down, then move it with the pointer until release. Dragging updates the lifted
  // points but never restarts the stream (the runToken gate owns that).
  const draggingRef = useRef<number | null>(null);

  const pointerPos = useCallback((event: React.PointerEvent<HTMLCanvasElement>) => {
    const canvas = canvasRef.current;
    if (!canvas) return { x: 0, y: 0 };
    const rect = canvas.getBoundingClientRect();
    // Map the client coordinates back through the CSS-pixel viewBox (the canvas is
    // scaled to fit its container, so divide by the displayed-to-logical ratio).
    const scaleX = VIEW_W / rect.width;
    const scaleY = VIEW_H / rect.height;
    return {
      x: (event.clientX - rect.left) * scaleX,
      y: (event.clientY - rect.top) * scaleY,
    };
  }, []);

  const onPointerDown = useCallback(
    (event: React.PointerEvent<HTMLCanvasElement>) => {
      const { x, y } = pointerPos(event);
      let nearest = -1;
      let nearestDist = HIT_RADIUS;
      const current = pointsRef.current;
      for (let i = 0; i < current.length; i++) {
        const px = toPixelX(clamp(current[i][0], X_MIN, X_MAX));
        const py = toPixelY(clamp(current[i][1], Y_MIN, Y_MAX));
        const dist = Math.hypot(px - x, py - y);
        if (dist <= nearestDist) {
          nearest = i;
          nearestDist = dist;
        }
      }
      if (nearest >= 0) {
        draggingRef.current = nearest;
        // The grabbed point becomes the keyboard-active one, so a pointer user can
        // hand off to the arrow keys on the same point without re-selecting.
        setActiveIndex(nearest);
        event.currentTarget.setPointerCapture(event.pointerId);
      }
    },
    [pointerPos],
  );

  const onPointerMove = useCallback(
    (event: React.PointerEvent<HTMLCanvasElement>) => {
      const index = draggingRef.current;
      if (index === null) return;
      const { x, y } = pointerPos(event);
      const dataX = clamp(toDataX(x), X_MIN, X_MAX);
      const dataY = clamp(toDataY(y), Y_MIN, Y_MAX);
      const next = pointsRef.current.map((p, i): [number, number] =>
        i === index ? [dataX, dataY] : p,
      );
      onPointsChange(next);
      rendererRef.current?.redraw();
    },
    [pointerPos, onPointsChange],
  );

  const endDrag = useCallback((event: React.PointerEvent<HTMLCanvasElement>) => {
    if (draggingRef.current === null) return;
    draggingRef.current = null;
    if (event.currentTarget.hasPointerCapture(event.pointerId)) {
      event.currentTarget.releasePointerCapture(event.pointerId);
    }
  }, []);

  // Keyboard focus on the canvas: show the active-point ring, defaulting to the
  // first point so arrow keys act immediately. Repaint to draw the ring.
  const onFocus = useCallback(() => {
    focusedRef.current = true;
    setActiveIndex((prev) => {
      if (prev >= 0 && prev < pointsRef.current.length) return prev;
      return pointsRef.current.length > 0 ? 0 : -1;
    });
    rendererRef.current?.redraw();
  }, []);

  const onBlur = useCallback(() => {
    focusedRef.current = false;
    // Repaint to clear the focus ring now that the canvas is unfocused.
    rendererRef.current?.redraw();
  }, []);

  // Keyboard path for moving points: arrows nudge the active point by KEY_STEP data
  // units (left/right on x, up/down on y), [ and ] cycle which point is active. This
  // mirrors the pointer drag for keyboard-only users without restarting the stream
  // (the runToken gate owns that), and never traps other keys.
  const onKeyDown = useCallback(
    (event: React.KeyboardEvent<HTMLCanvasElement>) => {
      const current = pointsRef.current;
      if (current.length === 0) return;

      // Cycle the active point with the bracket keys, wrapping at the ends.
      if (event.key === "[" || event.key === "]") {
        event.preventDefault();
        setActiveIndex((prev) => {
          const base = prev >= 0 ? prev : 0;
          const delta = event.key === "]" ? 1 : -1;
          return (base + delta + current.length) % current.length;
        });
        return;
      }

      let dx = 0;
      let dy = 0;
      if (event.key === "ArrowLeft") dx = -KEY_STEP;
      else if (event.key === "ArrowRight") dx = KEY_STEP;
      else if (event.key === "ArrowUp") dy = KEY_STEP;
      else if (event.key === "ArrowDown") dy = -KEY_STEP;
      else return;

      event.preventDefault();
      const index = activeIndexRef.current >= 0 ? activeIndexRef.current : 0;
      if (index >= current.length) return;
      const [x, y] = current[index];
      const nextX = clamp(x + dx, X_MIN, X_MAX);
      const nextY = clamp(y + dy, Y_MIN, Y_MAX);
      if (nextX === x && nextY === y) return;
      const next = current.map((p, i): [number, number] => (i === index ? [nextX, nextY] : p));
      // Ensure the moved point stays active even if focus arrived without one set.
      if (activeIndexRef.current !== index) setActiveIndex(index);
      onPointsChange(next);
      rendererRef.current?.redraw();
    },
    [onPointsChange],
  );

  const retry = useCallback(() => {
    sawDataRef.current = false;
    setErrorMessage(null);
    setState("streaming");
    onRunStartRef.current?.();
    const request: RunRequest = {
      method,
      dataset: { tag: "Regression", points: pointsRef.current },
      samples: REQUESTED_SAMPLES,
    };
    void (async () => {
      let runId: string;
      try {
        runId = await startRegressionRun(engineBaseUrl, request);
      } catch (error) {
        setErrorMessage(error instanceof Error ? error.message : "could not start the run");
        setState("error");
        return;
      }
      teardown();
      subscriptionRef.current = subscribeRun(engineBaseUrl, runId, {
        onSummary: (summary) => {
          setMeanReadout(summary.mean);
          setBatchReadout(summary.batchIndex);
          onSummaryRef.current?.(summary);
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onBatch: (batch) => {
          sawDataRef.current = true;
          rendererRef.current?.setSlopes(batch.draws);
          onBatchDrawsRef.current?.(batch.draws);
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onDone: () => {
          setState(sawDataRef.current ? "converged" : "empty");
          onRunDoneRef.current?.();
        },
        onReconnecting: () => setState("reconnecting"),
        onError: (error) => {
          setErrorMessage(error instanceof Error ? error.message : "the stream dropped");
          setState("error");
        },
      });
    })();
  }, [method, teardown]);

  // Describe the widget and the active point so screen readers announce both the
  // keyboard contract and the live coordinates as arrow keys move the point.
  const activePoint = activeIndex >= 0 && activeIndex < points.length ? points[activeIndex] : null;
  const ariaLabel = activePoint
    ? `Regression data points, ${points.length} total. Point ${activeIndex + 1} active at x ${activePoint[0].toFixed(1)}, y ${activePoint[1].toFixed(1)}. Arrow keys move it; bracket keys change which point is active.`
    : `Regression data points, ${points.length} total. Press arrow keys to move a point; bracket keys change which point is active.`;

  return (
    <div className={styles.root} data-state={state}>
      <figure className={`${styles.figure} ${state === "reconnecting" ? styles.reconnecting : ""}`}>
        {/* role=application: the canvas owns arrow keys to reposition points, so
            assistive tech must pass keystrokes through rather than browse. A Canvas
            plot has no semantic element to use instead. */}
        {/* biome-ignore lint/a11y/noInteractiveElementToNoninteractiveRole: a Canvas point editor needs role=application so arrow keys reach it */}
        <canvas
          ref={canvasRef}
          className={styles.canvas}
          style={{ aspectRatio: `${VIEW_W} / ${VIEW_H}` }}
          tabIndex={0}
          role="application"
          aria-label={ariaLabel}
          onFocus={onFocus}
          onBlur={onBlur}
          onKeyDown={onKeyDown}
          onPointerDown={onPointerDown}
          onPointerMove={onPointerMove}
          onPointerUp={endDrag}
          onPointerCancel={endDrag}
        />
      </figure>

      <div className={styles.readout}>
        <div className={styles.stat}>
          <span className={styles.statLabel}>slope mean</span>
          <span className={styles.statValue}>
            {meanReadout !== null ? meanReadout.toFixed(4) : "--"}
          </span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>points</span>
          <span className={styles.statValue}>{points.length.toString().padStart(2, "0")}</span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>batch</span>
          <span className={styles.statValue}>
            {batchReadout !== null ? batchReadout.toString().padStart(2, "0") : "--"}
          </span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>state</span>
          <span
            className={`${styles.statValue} ${state === "reconnecting" ? styles.statusAccent : ""}`}
          >
            {state}
          </span>
        </div>
      </div>

      {state === "empty" && (
        <div className={styles.banner}>
          <span className={styles.bannerText}>
            Drag the points to shape the data, then start a run to watch the band of plausible lines
            collapse onto the trend.
          </span>
        </div>
      )}

      {state === "reconnecting" && (
        <div className={styles.banner}>
          <span className={`${styles.bannerText} ${styles.statusAccent}`}>
            Reconnecting to the engine. The current band is preserved.
          </span>
        </div>
      )}

      {state === "error" && (
        <div className={styles.banner}>
          <span className={styles.bannerText}>{errorMessage ?? "The engine is unreachable."}</span>
          <button type="button" className={styles.retry} onClick={retry}>
            retry
          </button>
        </div>
      )}
    </div>
  );
}

export { SAMPLE_POINTS };
