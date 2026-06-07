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
  // subsample, onSummary carries each running summary.
  onRunStart?: () => void;
  onBatchDraws?: (draws: number[]) => void;
  onSummary?: (summary: Summary) => void;
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

  const [state, setState] = useState<VizState>("empty");
  const [meanReadout, setMeanReadout] = useState<number | null>(null);
  const [batchReadout, setBatchReadout] = useState<number | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

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
    for (const [x, y] of pointsRef.current) {
      const px = toPixelX(clamp(x, X_MIN, X_MAX));
      const py = toPixelY(clamp(y, Y_MIN, Y_MAX));
      ctx.beginPath();
      ctx.arc(px, py, POINT_RADIUS, 0, Math.PI * 2);
      ctx.fill();
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
  // lifts new state) so the dots track without waiting for a batch. Drags also
  // redraw imperatively in the move handler for immediacy; this covers the rest.
  // points is the intended trigger even though redraw reads pointsRef, so the dep
  // is deliberate, not the body's free variable biome looks for.
  // biome-ignore lint/correctness/useExhaustiveDependencies: points is the repaint trigger; redraw reads the ref by design
  useEffect(() => {
    rendererRef.current?.redraw();
  }, [points]);

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
      try {
        await startRegressionRun(engineBaseUrl, request);
      } catch (error) {
        if (cancelled) return;
        setErrorMessage(error instanceof Error ? error.message : "could not start the run");
        setState("error");
        return;
      }
      if (cancelled) return;

      subscriptionRef.current = subscribeRun(engineBaseUrl, {
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
      try {
        await startRegressionRun(engineBaseUrl, request);
      } catch (error) {
        setErrorMessage(error instanceof Error ? error.message : "could not start the run");
        setState("error");
        return;
      }
      teardown();
      subscriptionRef.current = subscribeRun(engineBaseUrl, {
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
        onDone: () => setState(sawDataRef.current ? "converged" : "empty"),
        onReconnecting: () => setState("reconnecting"),
        onError: (error) => {
          setErrorMessage(error instanceof Error ? error.message : "the stream dropped");
          setState("error");
        },
      });
    })();
  }, [method, teardown]);

  return (
    <div className={styles.root} data-state={state}>
      <figure className={`${styles.figure} ${state === "reconnecting" ? styles.reconnecting : ""}`}>
        <canvas
          ref={canvasRef}
          className={styles.canvas}
          style={{ aspectRatio: `${VIEW_W} / ${VIEW_H}` }}
          aria-label="Regression band of plausible lines over the dragged data points"
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
