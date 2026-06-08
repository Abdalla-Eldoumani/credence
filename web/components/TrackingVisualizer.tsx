"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import { resolveCssVar } from "@/lib/canvas";
import { engineBaseUrl } from "@/lib/config";
import {
  type Method,
  type RunRequest,
  type Subscription,
  type Summary,
  startTrackingRun,
  subscribeRun,
} from "@/lib/stream";
import styles from "./TrackingVisualizer.module.css";

// The position axis the cloud spans, in target-position units. The walk starts at 0
// with step noise trackQ = 0.6 and is observed with noise trackR = 1.0, so a run of a
// few dozen steps wanders inside roughly +/-10; this window brackets the trajectory
// the sample data plants without clipping the cloud at the ends.
const POS_MIN = -10;
const POS_MAX = 10;

// The CSS pixel size of the canvas coordinate space; the backing store is scaled by
// devicePixelRatio for crisp dots. These are the logical dimensions.
const VIEW_W = 720;
const VIEW_H = 460;
const MARGIN = { top: 16, right: 16, bottom: 32, left: 40 };
const PLOT_W = VIEW_W - MARGIN.left - MARGIN.right;
const PLOT_H = VIEW_H - MARGIN.top - MARGIN.bottom;

// Particle radius (design tokens: 2px to 3px).
const PARTICLE_RADIUS = 2.5;

// The vertical band the particles jitter across, centered in the plot, so a dense
// cluster of filtering states reads as a vertical wash rather than a single line.
const JITTER_TOP = MARGIN.top + PLOT_H * 0.2;
const JITTER_BOTTOM = MARGIN.top + PLOT_H * 0.8;

// The per-frame work cap: the most particles drawn in one frame. The cloud is a
// density, so a few hundred dots already read as the full spread; capping the count
// keeps every frame bounded however many particles a batch carries.
const MAX_PARTICLES = 300;

// The opacity-by-weight range from the design tokens: 0.25 at low weight to 0.9 at high.
const ALPHA_LOW = 0.25;
const ALPHA_HIGH = 0.9;
// The uniform mid alpha used when there are no weights, so the cloud still reads as a
// density without a per-particle weight to scale by. The tracking server always sends
// weights, but the fallback keeps the renderer total.
const ALPHA_UNIFORM = 0.55;

// The posterior interpolation window from the design tokens (180ms to 240ms); the
// midpoint, matching the band and histogram tweens so every morph reads the same.
const DURATION_MS = 210;

// The engine grows the run length batch by batch; ask for enough that the filter
// walks the whole observation sequence (one batch per step, capped server-side).
const REQUESTED_SAMPLES = 20000;

type VizState = "empty" | "streaming" | "converged" | "reconnecting" | "error";

type TrackingVisualizerProps = {
  method: Method;
  // The noisy observation sequence the filter tracks. The visualizer also reads it to
  // mark the latest observation (the freshest read of the hidden target) as the batch
  // index advances, the only true-position proxy available client-side: the planted
  // trajectory is generated server-side under a fixed seed and never crosses the wire.
  observations: number[];
  // Bumped by the controls to (re)start a run. 0 means no run requested yet.
  runToken: number;
  // Optional taps on the single stream subscription so siblings (the diagnostics) can
  // mount without opening a second EventSource.
  onRunStart?: () => void;
  onBatchDraws?: (draws: number[]) => void;
  onSummary?: (summary: Summary) => void;
  onRunDone?: () => void;
};

const toPixelX = (pos: number): number =>
  MARGIN.left + ((pos - POS_MIN) / (POS_MAX - POS_MIN)) * PLOT_W;

const clamp = (value: number, lo: number, hi: number): number => Math.max(lo, Math.min(hi, value));

// A cheap deterministic [0,1) hash of an integer, so each particle index keeps a
// stable vertical lane across frames (the y-jitter does not shimmer while x eases).
const laneFraction = (i: number): number => {
  const s = Math.sin((i + 1) * 12.9898) * 43758.5453;
  return s - Math.floor(s);
};

// Ease the cloud's x toward its target over the settle window. cubic-bezier(0.22,1,
// 0.36,1) is close to 1-(1-t)^3 over [0,1]; the cloud needs the same settling feel as
// the SVG tween, not exact curve parity, so this stays a pure module-scope function.
const easeSettle = (t: number): number => {
  const c = t < 0 ? 0 : t > 1 ? 1 : t;
  return 1 - (1 - c) * (1 - c) * (1 - c);
};

// One particle the renderer tweens: a target x-position (in data units), a fixed
// vertical lane, and an alpha already scaled from its weight.
type Particle = { x: number; alpha: number; lane: number };

// Build the capped particle set from a batch. Draws are sorted so the eased tween is a
// contraction of order statistics (position i eases toward the new i-th sorted draw),
// which makes the cloud's spread move smoothly as the filtering distribution shifts
// along the trajectory rather than flickering. Weights ride along the sort so each
// particle's opacity stays paired with its state. Alpha is scaled within the frame
// relative to the heaviest weight, so the cloud reads as a density whatever the
// (normalized, hence tiny) weight magnitudes are; with no weights every particle takes
// the uniform mid alpha.
const prepareParticles = (draws: number[], weights: number[] | null | undefined): Particle[] => {
  const paired: { x: number; w: number }[] = [];
  for (let i = 0; i < draws.length; i++) {
    const x = draws[i];
    if (!Number.isFinite(x)) continue;
    const w = weights && Number.isFinite(weights[i]) ? weights[i] : Number.NaN;
    paired.push({ x, w });
  }
  paired.sort((a, b) => a.x - b.x);

  // Thin to the cap, striding to keep the full span rather than the first N.
  const thinned: { x: number; w: number }[] = [];
  if (paired.length <= MAX_PARTICLES) {
    thinned.push(...paired);
  } else {
    const stride = paired.length / MAX_PARTICLES;
    for (let i = 0; i < MAX_PARTICLES; i++) {
      thinned.push(paired[Math.min(paired.length - 1, Math.floor(i * stride))]);
    }
  }

  const hasWeights = weights != null && thinned.some((p) => Number.isFinite(p.w));
  const maxW = hasWeights ? Math.max(...thinned.map((p) => (Number.isFinite(p.w) ? p.w : 0))) : 0;

  return thinned.map((p, i) => {
    let alpha = ALPHA_UNIFORM;
    if (hasWeights && maxW > 0) {
      const scaled = Number.isFinite(p.w) ? p.w / maxW : 0;
      alpha = ALPHA_LOW + (ALPHA_HIGH - ALPHA_LOW) * clamp(scaled, 0, 1);
    }
    return { x: p.x, alpha, lane: laneFraction(i) };
  });
};

export function TrackingVisualizer({
  method,
  observations,
  runToken,
  onRunStart,
  onBatchDraws,
  onSummary,
  onRunDone,
}: TrackingVisualizerProps) {
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const subscriptionRef = useRef<Subscription | null>(null);
  const reducedMotionRef = useRef(false);
  const rafRef = useRef<number | null>(null);

  // The stream-tap callbacks captured in refs so the run effect (gated on runToken)
  // calls the latest without re-subscribing mid-run.
  const onRunStartRef = useRef(onRunStart);
  onRunStartRef.current = onRunStart;
  const onBatchDrawsRef = useRef(onBatchDraws);
  onBatchDrawsRef.current = onBatchDraws;
  const onSummaryRef = useRef(onSummary);
  onSummaryRef.current = onSummary;
  const onRunDoneRef = useRef(onRunDone);
  onRunDoneRef.current = onRunDone;

  // The observation sequence read by the paint loop without re-creating it; a ref
  // keeps the marks in sync if the page changes the planted observations.
  const observationsRef = useRef(observations);
  observationsRef.current = observations;

  // The estimate (filtering mean) and the latest-observation index for the marks,
  // held in refs so the paint loop reads the freshest values each frame without
  // re-binding. The estimate is the --viz-posterior line; the observation at the
  // current step is the --viz-analytic mark the cloud is tracking.
  const estimateRef = useRef<number | null>(null);
  const obsIndexRef = useRef<number | null>(null);

  const [state, setState] = useState<VizState>("empty");
  const [meanReadout, setMeanReadout] = useState<number | null>(null);
  const [batchReadout, setBatchReadout] = useState<number | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

  // Whether any batch arrived this run, so onDone marks convergence vs empty.
  const sawDataRef = useRef(false);

  // The eased and target particle sets, plus the tween clock. The cloud eases its
  // x-positions from the previous frame toward the latest batch over DURATION_MS, so
  // the population slides along the trajectory; reduced motion jumps per batch.
  const currentRef = useRef<Particle[]>([]);
  const startRef = useRef<number[]>([]);
  const targetRef = useRef<Particle[]>([]);
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

  // Paint one frame: clear, grid, the particles at their eased x, then the estimate
  // and the latest-observation marks on top. Every color resolves through
  // getComputedStyle (resolveCssVar), so no hex is hardcoded; a missing token degrades
  // to transparent, not a crash.
  const paint = useCallback((xs: number[]) => {
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

    // The particles. --viz-particle resolves to a hex with no alpha, so the alpha is
    // applied through globalAlpha per dot (the opacity-by-weight channel).
    const particleColor = resolveCssVar(canvas, "--viz-particle");
    ctx.fillStyle = particleColor;
    const particles = currentRef.current;
    for (let i = 0; i < particles.length; i++) {
      const px = toPixelX(clamp(xs[i], POS_MIN, POS_MAX));
      const py = JITTER_TOP + particles[i].lane * (JITTER_BOTTOM - JITTER_TOP);
      ctx.globalAlpha = particles[i].alpha;
      ctx.beginPath();
      ctx.arc(px, py, PARTICLE_RADIUS, 0, Math.PI * 2);
      ctx.fill();
    }
    ctx.globalAlpha = 1;

    // The estimate: the filtering mean as a crisp --viz-posterior vertical line, the
    // approximation the cloud centers on.
    const estimate = estimateRef.current;
    if (estimate !== null && Number.isFinite(estimate)) {
      const ex = Math.round(toPixelX(clamp(estimate, POS_MIN, POS_MAX))) + 0.5;
      ctx.strokeStyle = resolveCssVar(canvas, "--viz-posterior");
      ctx.lineWidth = 2;
      ctx.beginPath();
      ctx.moveTo(ex, MARGIN.top);
      ctx.lineTo(ex, MARGIN.top + PLOT_H);
      ctx.stroke();
    }

    // The latest observation: the freshest noisy read of the moving target, marked in
    // --viz-analytic on top as a tick plus a dot, so the eye watches the cloud and the
    // estimate track the moving read down the axis.
    const obsIndex = obsIndexRef.current;
    const obs = observationsRef.current;
    if (obsIndex !== null && obsIndex >= 0 && obsIndex < obs.length) {
      const value = obs[obsIndex];
      if (Number.isFinite(value)) {
        const ax = Math.round(toPixelX(clamp(value, POS_MIN, POS_MAX))) + 0.5;
        const analyticColor = resolveCssVar(canvas, "--viz-analytic");
        ctx.strokeStyle = analyticColor;
        ctx.fillStyle = analyticColor;
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(ax, MARGIN.top);
        ctx.lineTo(ax, MARGIN.top + PLOT_H);
        ctx.stroke();
        ctx.beginPath();
        ctx.arc(ax - 0.5, MARGIN.top + PLOT_H * 0.5, 3.5, 0, Math.PI * 2);
        ctx.fill();
      }
    }
  }, []);

  const stopRaf = useCallback(() => {
    if (rafRef.current !== null) {
      cancelAnimationFrame(rafRef.current);
      rafRef.current = null;
    }
  }, []);

  // The frame loop: ease each particle's x from its start toward its target and paint.
  // Settles (stops requesting frames) once the tween completes. Reduced motion jumps
  // straight to the target and paints once (discrete redraw per batch).
  const frame = useCallback(
    (now: number) => {
      rafRef.current = null;
      const target = targetRef.current;
      const start = startRef.current;
      const xs = new Array<number>(target.length);

      if (reducedMotionRef.current) {
        for (let i = 0; i < target.length; i++) xs[i] = target[i].x;
        currentRef.current = target;
        paint(xs);
        return;
      }

      const eased = easeSettle((now - tweenStartRef.current) / DURATION_MS);
      for (let i = 0; i < target.length; i++) {
        const from = i < start.length ? start[i] : target[i].x;
        xs[i] = from + (target[i].x - from) * eased;
      }
      currentRef.current = target;
      paint(xs);

      if (eased < 1) {
        rafRef.current = requestAnimationFrame(frame);
      }
    },
    [paint],
  );

  // Hand a new batch to the cloud: re-anchor the eased start at the current eased
  // positions so a burst keeps sliding instead of snapping, set the new target, and
  // kick the loop.
  const setBatch = useCallback(
    (draws: number[], weights: number[] | null | undefined) => {
      const next = prepareParticles(draws, weights);
      const prevXs = currentRef.current.map((p) => p.x);
      const start = new Array<number>(next.length);
      for (let i = 0; i < next.length; i++) {
        start[i] = i < prevXs.length ? prevXs[i] : next[i].x;
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

  // Build the canvas and paint the empty state (grid only) on mount.
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
          // The filtering mean is the estimate mark; the batch index is the current
          // step along the trajectory, so the latest observation tracks it.
          estimateRef.current = summary.mean;
          obsIndexRef.current = summary.batchIndex;
          setMeanReadout(summary.mean);
          setBatchReadout(summary.batchIndex);
          onSummaryRef.current?.(summary);
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onBatch: (batch) => {
          if (cancelledRef.current) return;
          sawDataRef.current = true;
          obsIndexRef.current = batch.batchIndex;
          setBatch(batch.draws, batch.weights);
          onBatchDrawsRef.current?.(batch.draws);
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
  // current method and observations at the bump; depending on them here would restart
  // the stream on every keystroke, which the runToken gate exists to prevent.
  // biome-ignore lint/correctness/useExhaustiveDependencies: runToken gates the run; method/observations are captured intentionally, not tracked
  useEffect(() => {
    if (runToken === 0) return;

    const cancelledRef = { current: false };
    sawDataRef.current = false;
    estimateRef.current = null;
    obsIndexRef.current = null;
    setErrorMessage(null);
    setState("streaming");
    onRunStartRef.current?.();

    const request: RunRequest = {
      method,
      dataset: { tag: "Tracking", observations },
      samples: REQUESTED_SAMPLES,
    };

    const begin = async () => {
      let runId: string;
      try {
        runId = await startTrackingRun(engineBaseUrl, request);
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
    estimateRef.current = null;
    obsIndexRef.current = null;
    setErrorMessage(null);
    setState("streaming");
    onRunStartRef.current?.();
    const cancelledRef = { current: false };
    const request: RunRequest = {
      method,
      dataset: { tag: "Tracking", observations },
      samples: REQUESTED_SAMPLES,
    };
    void (async () => {
      let runId: string;
      try {
        runId = await startTrackingRun(engineBaseUrl, request);
      } catch (error) {
        setErrorMessage(error instanceof Error ? error.message : "could not start the run");
        setState("error");
        return;
      }
      openStream(runId, cancelledRef);
    })();
  }, [method, observations, openStream]);

  return (
    <div className={styles.root} data-state={state}>
      <figure className={`${styles.figure} ${state === "reconnecting" ? styles.reconnecting : ""}`}>
        <canvas
          ref={canvasRef}
          className={styles.canvas}
          style={{ aspectRatio: `${VIEW_W} / ${VIEW_H}` }}
          role="img"
          aria-label={`Particle cloud tracking a moving target. ${
            meanReadout !== null
              ? `Filtering estimate ${meanReadout.toFixed(2)} at step ${(batchReadout ?? 0) + 1}.`
              : "No run yet."
          }`}
        />
      </figure>

      <div className={styles.readout}>
        <div className={styles.stat}>
          <span className={styles.statLabel}>estimate</span>
          <span className={styles.statValue}>
            {meanReadout !== null ? meanReadout.toFixed(3) : "--"}
          </span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>latest read</span>
          <span className={styles.statValue}>
            {batchReadout !== null && batchReadout < observations.length
              ? observations[batchReadout].toFixed(2)
              : "--"}
          </span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>step</span>
          <span className={styles.statValue}>
            {batchReadout !== null
              ? `${(batchReadout + 1).toString().padStart(2, "0")}/${observations.length
                  .toString()
                  .padStart(2, "0")}`
              : "--"}
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
            Start a run to watch the particle cloud follow the moving target, the pale mark tracking
            the latest read as the estimate locks on.
          </span>
        </div>
      )}

      {state === "reconnecting" && (
        <div className={styles.banner}>
          <span className={`${styles.bannerText} ${styles.statusAccent}`}>
            Reconnecting to the engine. The current cloud is preserved.
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
