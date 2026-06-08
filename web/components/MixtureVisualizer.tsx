"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import { resolveCssVar } from "@/lib/canvas";
import { engineBaseUrl } from "@/lib/config";
import {
  type Method,
  type RunRequest,
  type Subscription,
  type Summary,
  startMixtureRun,
  subscribeRun,
} from "@/lib/stream";
import styles from "./MixtureVisualizer.module.css";

// The position axis the data and clusters span, in data units. The server's broad
// Normal(0,10) prior and the well-separated planted means sit inside +/-10, so this
// window brackets the synthetic data and the recovered means without clipping.
const POS_MIN = -10;
const POS_MAX = 10;

// The CSS pixel size of the canvas coordinate space; the backing store is scaled by
// devicePixelRatio for crisp dots. These are the logical dimensions.
const VIEW_W = 720;
const VIEW_H = 460;
const MARGIN = { top: 16, right: 16, bottom: 32, left: 40 };
const PLOT_W = VIEW_W - MARGIN.left - MARGIN.right;
const PLOT_H = VIEW_H - MARGIN.top - MARGIN.bottom;

// Data point radius (design tokens: 3px to 4px).
const DATA_RADIUS = 3.5;

// The per-frame work cap: the most data points drawn in one frame. The synthetic set
// is small, but capping keeps the frame bounded however many points a dataset carries
// (the same per-frame contract the cloud honors).
const MAX_DATA_POINTS = 300;

// The posterior interpolation window from the design tokens (180ms to 240ms); the
// midpoint, matching the other tweens so every morph reads the same.
const DURATION_MS = 210;

// The vertical lanes for the data points (jittered below the axis line so the row
// reads as a strip) and the height the recovered-mean and planted ticks span.
const DATA_BAND_TOP = MARGIN.top + PLOT_H * 0.55;
const DATA_BAND_BOTTOM = MARGIN.top + PLOT_H * 0.92;
const TICK_TOP = MARGIN.top + PLOT_H * 0.08;
const TICK_BOTTOM = MARGIN.top + PLOT_H * 0.5;

// The engine grows the run length batch by batch; ask for enough that the recovered
// means concentrate sharply on the planted clusters (capped server-side).
const REQUESTED_SAMPLES = 20000;

type VizState = "empty" | "streaming" | "converged" | "reconnecting" | "error";

type MixtureVisualizerProps = {
  method: Method;
  // The synthetic data points to cluster. Drawn in --viz-data along the axis; also the
  // dataset POSTed to /run/mixture.
  points: number[];
  // The planted (true) cluster means, marked crisp in --viz-analytic so the eye sees
  // the recovered means land on them. The server fixes the same planted clusters.
  plantedClusters: number[];
  // Bumped by the controls to (re)start a run. 0 means no run requested yet.
  runToken: number;
  // Optional taps on the single stream subscription so siblings (the diagnostics) can
  // mount without opening a second EventSource.
  onRunStart?: () => void;
  onSummary?: (summary: Summary) => void;
  onRunDone?: () => void;
};

const toPixelX = (pos: number): number =>
  MARGIN.left + ((pos - POS_MIN) / (POS_MAX - POS_MIN)) * PLOT_W;

const clamp = (value: number, lo: number, hi: number): number => Math.max(lo, Math.min(hi, value));

// A cheap deterministic [0,1) hash of an integer, so each data point keeps a stable
// vertical lane across frames (the strip does not shimmer).
const laneFraction = (i: number): number => {
  const s = Math.sin((i + 1) * 78.233) * 43758.5453;
  return s - Math.floor(s);
};

// Ease the recovered means toward their targets over the settle window. cubic-bezier(
// 0.22,1,0.36,1) is close to 1-(1-t)^3 over [0,1]; the marks need the same settling
// feel as the SVG tween, not exact curve parity, so this stays a pure function.
const easeSettle = (t: number): number => {
  const c = t < 0 ? 0 : t > 1 ? 1 : t;
  return 1 - (1 - c) * (1 - c) * (1 - c);
};

// Thin a list to at most k entries, preserving order and span (the same striding the
// engine uses), so the data-point draw count stays under the per-frame cap.
const thin = (values: number[], k: number): number[] => {
  if (values.length <= k) return values.slice();
  const out = new Array<number>(k);
  const stride = values.length / k;
  for (let i = 0; i < k; i++) {
    out[i] = values[Math.min(values.length - 1, Math.floor(i * stride))];
  }
  return out;
};

export function MixtureVisualizer({
  method,
  points,
  plantedClusters,
  runToken,
  onRunStart,
  onSummary,
  onRunDone,
}: MixtureVisualizerProps) {
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const subscriptionRef = useRef<Subscription | null>(null);
  const reducedMotionRef = useRef(false);
  const rafRef = useRef<number | null>(null);

  // The stream-tap callbacks captured in refs so the run effect (gated on runToken)
  // calls the latest without re-subscribing mid-run.
  const onRunStartRef = useRef(onRunStart);
  onRunStartRef.current = onRunStart;
  const onSummaryRef = useRef(onSummary);
  onSummaryRef.current = onSummary;
  const onRunDoneRef = useRef(onRunDone);
  onRunDoneRef.current = onRunDone;

  // The data points and planted clusters read by the paint loop without re-creating
  // it; refs keep the marks in sync if the page changes them.
  const pointsRef = useRef<number[]>(thin(points, MAX_DATA_POINTS));
  pointsRef.current = thin(points, MAX_DATA_POINTS);
  const plantedRef = useRef(plantedClusters);
  plantedRef.current = plantedClusters;

  const [state, setState] = useState<VizState>("empty");
  const [recoveredReadout, setRecoveredReadout] = useState<number[] | null>(null);
  const [batchReadout, setBatchReadout] = useState<number | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

  // Whether any batch arrived this run, so onDone marks convergence vs empty.
  const sawDataRef = useRef(false);

  // The eased and target recovered-mean sets, plus the tween clock. The marks ease
  // from the previous frame toward the latest batch over DURATION_MS, so they visibly
  // glide onto the planted clusters; reduced motion jumps per batch.
  const currentRef = useRef<number[]>([]);
  const startRef = useRef<number[]>([]);
  const targetRef = useRef<number[]>([]);
  const tweenStartRef = useRef(0);

  // Size the canvas backing store to the device pixel ratio and scale the context so
  // drawing is in CSS pixels. Returns the 2D context, or null if absent.
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

  // Paint one frame: clear, grid, the planted-cluster marks, the synthetic data, then
  // the recovered means at their eased x on top. Every color resolves through
  // getComputedStyle (resolveCssVar), so no hex is hardcoded; a missing token degrades
  // to transparent, not a crash.
  const paint = useCallback((recovered: number[]) => {
    const canvas = canvasRef.current;
    const ctx = canvas?.getContext("2d");
    if (!canvas || !ctx) return;

    ctx.save();
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.restore();

    // Grid: a faint vertical rule every 2 position units and a baseline axis.
    const gridColor = resolveCssVar(canvas, "--viz-grid");
    const axisColor = resolveCssVar(canvas, "--viz-axis");
    ctx.strokeStyle = gridColor;
    ctx.lineWidth = 1;
    ctx.beginPath();
    for (let p = POS_MIN; p <= POS_MAX; p += 2) {
      const px = Math.round(toPixelX(p)) + 0.5;
      ctx.moveTo(px, MARGIN.top);
      ctx.lineTo(px, MARGIN.top + PLOT_H);
    }
    ctx.stroke();
    ctx.strokeStyle = axisColor;
    ctx.beginPath();
    const baseY = Math.round(MARGIN.top + PLOT_H) + 0.5;
    ctx.moveTo(MARGIN.left, baseY);
    ctx.lineTo(MARGIN.left + PLOT_W, baseY);
    ctx.stroke();

    // The planted clusters: crisp --viz-analytic ticks the recovered means land on,
    // drawn before the data so the data dots read over them.
    const analyticColor = resolveCssVar(canvas, "--viz-analytic");
    ctx.strokeStyle = analyticColor;
    ctx.lineWidth = 2;
    ctx.beginPath();
    for (const cluster of plantedRef.current) {
      if (!Number.isFinite(cluster)) continue;
      const cx = Math.round(toPixelX(clamp(cluster, POS_MIN, POS_MAX))) + 0.5;
      ctx.moveTo(cx, TICK_TOP);
      ctx.lineTo(cx, TICK_BOTTOM);
    }
    ctx.stroke();

    // The synthetic data points: --viz-data dots in a jittered strip below the axis.
    const dataColor = resolveCssVar(canvas, "--viz-data");
    ctx.fillStyle = dataColor;
    const pts = pointsRef.current;
    for (let i = 0; i < pts.length; i++) {
      const value = pts[i];
      if (!Number.isFinite(value)) continue;
      const px = toPixelX(clamp(value, POS_MIN, POS_MAX));
      const py = DATA_BAND_TOP + laneFraction(i) * (DATA_BAND_BOTTOM - DATA_BAND_TOP);
      ctx.beginPath();
      ctx.arc(px, py, DATA_RADIUS, 0, Math.PI * 2);
      ctx.fill();
    }

    // The recovered cluster means: --viz-posterior lines with a cap dot, the
    // approximation concentrating onto the planted ticks as the run proceeds.
    const posteriorColor = resolveCssVar(canvas, "--viz-posterior");
    ctx.strokeStyle = posteriorColor;
    ctx.fillStyle = posteriorColor;
    ctx.lineWidth = 2;
    for (let i = 0; i < recovered.length; i++) {
      const value = recovered[i];
      if (!Number.isFinite(value)) continue;
      const rx = Math.round(toPixelX(clamp(value, POS_MIN, POS_MAX))) + 0.5;
      ctx.beginPath();
      ctx.moveTo(rx, TICK_TOP);
      ctx.lineTo(rx, TICK_BOTTOM);
      ctx.stroke();
      ctx.beginPath();
      ctx.arc(rx - 0.5, TICK_TOP, 3.5, 0, Math.PI * 2);
      ctx.fill();
    }
  }, []);

  const stopRaf = useCallback(() => {
    if (rafRef.current !== null) {
      cancelAnimationFrame(rafRef.current);
      rafRef.current = null;
    }
  }, []);

  // The frame loop: ease each recovered mean from its start toward its target and
  // paint. Settles (stops requesting frames) once the tween completes. Reduced motion
  // jumps straight to the target and paints once (discrete redraw per batch).
  const frame = useCallback(
    (now: number) => {
      rafRef.current = null;
      const target = targetRef.current;
      const start = startRef.current;
      const xs = new Array<number>(target.length);

      if (reducedMotionRef.current) {
        for (let i = 0; i < target.length; i++) xs[i] = target[i];
        currentRef.current = xs;
        paint(xs);
        return;
      }

      const eased = easeSettle((now - tweenStartRef.current) / DURATION_MS);
      for (let i = 0; i < target.length; i++) {
        const from = i < start.length ? start[i] : target[i];
        xs[i] = from + (target[i] - from) * eased;
      }
      currentRef.current = xs;
      paint(xs);

      if (eased < 1) {
        rafRef.current = requestAnimationFrame(frame);
      }
    },
    [paint],
  );

  // Hand a new batch of recovered means to the marks: re-anchor the eased start at the
  // current eased positions so a burst keeps gliding instead of snapping, set the new
  // target, and kick the loop.
  const setBatch = useCallback(
    (draws: number[]) => {
      const next = draws.filter((d) => Number.isFinite(d));
      const prev = currentRef.current;
      const start = new Array<number>(next.length);
      for (let i = 0; i < next.length; i++) {
        start[i] = i < prev.length ? prev[i] : next[i];
      }
      startRef.current = start;
      targetRef.current = next;
      tweenStartRef.current = performance.now();
      if (rafRef.current === null) {
        rafRef.current = requestAnimationFrame(frame);
      }
    },
    [frame],
  );

  // Build the canvas and paint the empty state (grid + data + planted marks) on mount.
  useEffect(() => {
    const ctx = setupCanvas();
    if (!ctx) return;
    paint([]);
    return () => {
      stopRaf();
    };
  }, [setupCanvas, paint, stopRaf]);

  // Track prefers-reduced-motion; on entering reduced motion, settle immediately.
  useEffect(() => {
    if (typeof window === "undefined" || !window.matchMedia) return;
    const query = window.matchMedia("(prefers-reduced-motion: reduce)");
    const apply = () => {
      reducedMotionRef.current = query.matches;
      if (query.matches && targetRef.current.length > 0 && rafRef.current === null) {
        rafRef.current = requestAnimationFrame(frame);
      }
    };
    apply();
    query.addEventListener("change", apply);
    return () => query.removeEventListener("change", apply);
  }, [frame]);

  const teardown = useCallback(() => {
    subscriptionRef.current?.close();
    subscriptionRef.current = null;
  }, []);

  // Open the single stream subscription for a run, threading the runId. Shared by the
  // run effect and the retry path so neither opens a second EventSource.
  const openStream = useCallback(
    (runId: string, cancelledRef: { current: boolean }) => {
      teardown();
      subscriptionRef.current = subscribeRun(engineBaseUrl, runId, {
        onSummary: (summary) => {
          if (cancelledRef.current) return;
          setBatchReadout(summary.batchIndex);
          onSummaryRef.current?.(summary);
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onBatch: (batch) => {
          if (cancelledRef.current) return;
          sawDataRef.current = true;
          // The draws are the recovered (sorted) cluster means; there are k of them
          // and they carry no per-draw weight (the per-cluster concentration is the
          // story, not opacity), so they go straight to the marks.
          setBatch(batch.draws);
          setRecoveredReadout(batch.draws.filter((d) => Number.isFinite(d)));
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onDone: () => {
          if (cancelledRef.current) return;
          setState(sawDataRef.current ? "converged" : "empty");
          onRunDoneRef.current?.();
        },
        onReconnecting: () => {
          if (cancelledRef.current) return;
          setState("reconnecting");
        },
        onError: (error) => {
          if (cancelledRef.current) return;
          setErrorMessage(error instanceof Error ? error.message : "the stream dropped");
          setState("error");
        },
      });
    },
    [teardown, setBatch],
  );

  // Start (or restart) a run when the controls bump runToken. The run captures the
  // current method and points at the bump; depending on them here would restart the
  // stream on every keystroke, which the runToken gate exists to prevent.
  // biome-ignore lint/correctness/useExhaustiveDependencies: runToken gates the run; method/points are captured intentionally, not tracked
  useEffect(() => {
    if (runToken === 0) return;

    const cancelledRef = { current: false };
    sawDataRef.current = false;
    setErrorMessage(null);
    setRecoveredReadout(null);
    setState("streaming");
    onRunStartRef.current?.();

    const request: RunRequest = {
      method,
      dataset: { tag: "Mixture", mixturePoints: points },
      samples: REQUESTED_SAMPLES,
    };

    const begin = async () => {
      let runId: string;
      try {
        runId = await startMixtureRun(engineBaseUrl, request);
      } catch (error) {
        if (cancelledRef.current) return;
        setErrorMessage(error instanceof Error ? error.message : "could not start the run");
        setState("error");
        return;
      }
      if (cancelledRef.current) return;
      openStream(runId, cancelledRef);
    };

    void begin();

    return () => {
      cancelledRef.current = true;
      teardown();
    };
  }, [runToken]);

  const retry = useCallback(() => {
    sawDataRef.current = false;
    setErrorMessage(null);
    setRecoveredReadout(null);
    setState("streaming");
    onRunStartRef.current?.();
    const cancelledRef = { current: false };
    const request: RunRequest = {
      method,
      dataset: { tag: "Mixture", mixturePoints: points },
      samples: REQUESTED_SAMPLES,
    };
    void (async () => {
      let runId: string;
      try {
        runId = await startMixtureRun(engineBaseUrl, request);
      } catch (error) {
        setErrorMessage(error instanceof Error ? error.message : "could not start the run");
        setState("error");
        return;
      }
      openStream(runId, cancelledRef);
    })();
  }, [method, points, openStream]);

  const recoveredLabel =
    recoveredReadout && recoveredReadout.length > 0
      ? recoveredReadout.map((m) => m.toFixed(2)).join(", ")
      : "--";

  return (
    <div className={styles.root} data-state={state}>
      <figure className={`${styles.figure} ${state === "reconnecting" ? styles.reconnecting : ""}`}>
        <canvas
          ref={canvasRef}
          className={styles.canvas}
          style={{ aspectRatio: `${VIEW_W} / ${VIEW_H}` }}
          role="img"
          aria-label={`Synthetic data with recovered cluster means landing on the planted clusters at ${plantedClusters
            .map((c) => c.toFixed(1))
            .join(", ")}. ${
            recoveredReadout ? `Recovered means ${recoveredLabel}.` : "No run yet."
          }`}
        />
      </figure>

      <div className={styles.readout}>
        <div className={styles.stat}>
          <span className={styles.statLabel}>recovered means</span>
          <span className={styles.statValue}>{recoveredLabel}</span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>planted</span>
          <span className={styles.statValue}>
            {plantedClusters.map((c) => c.toFixed(1)).join(", ")}
          </span>
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
            Start a run to watch the recovered cluster means concentrate onto the planted clusters,
            marked by the pale ticks, over the synthetic data.
          </span>
        </div>
      )}

      {state === "reconnecting" && (
        <div className={styles.banner}>
          <span className={`${styles.bannerText} ${styles.statusAccent}`}>
            Reconnecting to the engine. The current clusters are preserved.
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
