"use client";

import { scaleLinear } from "d3-scale";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { betaBars, betaMean } from "@/lib/beta";
import { makeTween, type Tween } from "@/lib/clock";
import { engineBaseUrl } from "@/lib/config";
import { type RunRequest, type Subscription, startRun, subscribeRun } from "@/lib/stream";
import styles from "./CoinVisualizer.module.css";

// Bins for the posterior histogram over theta in [0, 1]. Forty bars read as a
// smooth shape while staying coarse enough that the streamed empirical histogram
// fills in visibly as samples accumulate.
const BIN_COUNT = 40;

// SVG coordinate space (a viewBox keeps it responsive). Generous left and bottom
// margins leave room for the mono axis labels.
const VIEW_W = 720;
const VIEW_H = 420;
const MARGIN = { top: 16, right: 16, bottom: 36, left: 44 };
const PLOT_W = VIEW_W - MARGIN.left - MARGIN.right;
const PLOT_H = VIEW_H - MARGIN.top - MARGIN.bottom;

// The engine grows the run length batch by batch; ask for enough to converge.
const REQUESTED_SAMPLES = 20000;

type VizState = "empty" | "streaming" | "converged" | "reconnecting" | "error";

type CoinVisualizerProps = {
  method: RunRequest["method"];
  flips: boolean[];
  priorA: number;
  priorB: number;
  // Bumped by the controls to (re)start a run. 0 means no run requested yet.
  runToken: number;
};

// The analytic posterior bars (a normalized density over the bins) so the overlay
// path and the empty-state prior share betaPdf as their single source of truth.
function densityPath(
  bars: number[],
  x: (theta: number) => number,
  y: (density: number) => number,
): string {
  const width = 1 / bars.length;
  let path = "";
  for (let i = 0; i < bars.length; i++) {
    const theta = (i + 0.5) * width;
    const command = i === 0 ? "M" : "L";
    path += `${command}${x(theta).toFixed(2)},${y(bars[i]).toFixed(2)} `;
  }
  return path.trim();
}

export function CoinVisualizer({ method, flips, priorA, priorB, runToken }: CoinVisualizerProps) {
  const heads = useMemo(() => flips.filter(Boolean).length, [flips]);
  const tails = flips.length - heads;

  // The exact posterior the approximation converges toward. Fixed for a given set
  // of flips; the overlay draws this, and the y-axis scales to include its peak.
  const posteriorA = priorA + heads;
  const posteriorB = priorB + tails;

  // The prior shown as the starting posterior in the empty state.
  const priorBars = useMemo(() => betaBars(priorA, priorB, BIN_COUNT), [priorA, priorB]);

  // The analytic posterior density on the bin grid (for the overlay path).
  const analyticBars = useMemo(
    () => betaBars(posteriorA, posteriorB, BIN_COUNT),
    [posteriorA, posteriorB],
  );

  const [bars, setBars] = useState<number[]>(priorBars);
  const [state, setState] = useState<VizState>("empty");
  const [hasData, setHasData] = useState(false);
  const [meanReadout, setMeanReadout] = useState<number | null>(null);
  const [batchReadout, setBatchReadout] = useState<number | null>(null);
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

  const tweenRef = useRef<Tween | null>(null);
  const subscriptionRef = useRef<Subscription | null>(null);
  // Running histogram of streamed draws (counts per bin), accumulated across
  // batches so the empirical posterior fills in as the approximation converges.
  const histogramRef = useRef<number[]>(new Array(BIN_COUNT).fill(0));
  const totalDrawsRef = useRef(0);
  const reducedMotionRef = useRef(false);
  // The latest batch index seen, so onDone can mark convergence.
  const sawDataRef = useRef(false);

  // y-axis scales to the taller of the analytic peak and the current bars, so the
  // exact curve always fits and the approximation has headroom to grow into it.
  const yMax = useMemo(() => {
    const analyticPeak = Math.max(...analyticBars, ...priorBars, 0.001);
    return analyticPeak * 1.15;
  }, [analyticBars, priorBars]);

  const xScale = useMemo(() => scaleLinear().domain([0, 1]).range([0, PLOT_W]), []);
  const yScale = useMemo(() => scaleLinear().domain([0, yMax]).range([PLOT_H, 0]), [yMax]);

  // Normalize the running histogram to a density (area 1) so its scale matches the
  // analytic overlay. Empty bins stay 0; the bars morph toward this each batch.
  const histogramDensity = useCallback((): number[] => {
    const counts = histogramRef.current;
    const total = totalDrawsRef.current;
    const binWidth = 1 / BIN_COUNT;
    if (total === 0) return new Array(BIN_COUNT).fill(0);
    return counts.map((count) => count / (total * binWidth));
  }, []);

  // Set up the tween once and keep it across runs; the component drives onFrame to
  // repaint the bars from the client clock. priorBars is read only to seed the
  // initial buffer; the tween owns its own state after, so this runs once on mount
  // and must not re-create the tween when priorBars identity changes.
  // biome-ignore lint/correctness/useExhaustiveDependencies: seed-once on mount; the tween owns its state afterward
  useEffect(() => {
    const tween = makeTween(priorBars, (values) => {
      // Copy out so React sees a new array reference each frame.
      setBars(values.slice());
    });
    tweenRef.current = tween;
    return () => {
      tween.stop();
      tweenRef.current = null;
    };
  }, []);

  // Track prefers-reduced-motion and forward it to the tween.
  useEffect(() => {
    if (typeof window === "undefined" || !window.matchMedia) return;
    const query = window.matchMedia("(prefers-reduced-motion: reduce)");
    const apply = () => {
      reducedMotionRef.current = query.matches;
      tweenRef.current?.setReducedMotion(query.matches);
    };
    apply();
    query.addEventListener("change", apply);
    return () => query.removeEventListener("change", apply);
  }, []);

  const teardown = useCallback(() => {
    subscriptionRef.current?.close();
    subscriptionRef.current = null;
  }, []);

  // Start (or restart) a run whenever the controls bump runToken. The run captures
  // the current method and flips at the moment of the bump; depending on them here
  // would restart the stream on every flip toggle mid-edit, which the runToken gate
  // exists to prevent (the user edits freely, then presses start/restart).
  // biome-ignore lint/correctness/useExhaustiveDependencies: runToken gates the run; method/flips are captured intentionally, not tracked
  useEffect(() => {
    if (runToken === 0) return;

    let cancelled = false;
    // Reset the accumulators for the fresh run.
    histogramRef.current = new Array(BIN_COUNT).fill(0);
    totalDrawsRef.current = 0;
    sawDataRef.current = false;
    setErrorMessage(null);
    setState("streaming");

    const request: RunRequest = {
      method,
      dataset: { tag: "Coin", flips },
      samples: REQUESTED_SAMPLES,
    };

    const accumulate = (draws: number[]) => {
      const counts = histogramRef.current;
      for (const draw of draws) {
        if (draw <= 0 || draw >= 1 || Number.isNaN(draw)) continue;
        const bin = Math.min(BIN_COUNT - 1, Math.floor(draw * BIN_COUNT));
        counts[bin] += 1;
        totalDrawsRef.current += 1;
      }
      sawDataRef.current = true;
      setHasData(true);
      tweenRef.current?.setTarget(histogramDensity());
    };

    teardown();

    const begin = async () => {
      let runId: string;
      try {
        runId = await startRun(engineBaseUrl, request);
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
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onBatch: (batch) => {
          if (cancelled) return;
          accumulate(batch.draws);
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onDone: () => {
          if (cancelled) return;
          setState(sawDataRef.current ? "converged" : "empty");
        },
        onReconnecting: () => {
          if (cancelled) return;
          // Preserve the current bars; just flag the state (edge case SSE drop).
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

  // The retry button re-issues the current run by tearing down and re-subscribing.
  const retry = useCallback(() => {
    histogramRef.current = new Array(BIN_COUNT).fill(0);
    totalDrawsRef.current = 0;
    sawDataRef.current = false;
    setErrorMessage(null);
    setState("streaming");
    const request: RunRequest = {
      method,
      dataset: { tag: "Coin", flips },
      samples: REQUESTED_SAMPLES,
    };
    void (async () => {
      let runId: string;
      try {
        runId = await startRun(engineBaseUrl, request);
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
          setState((prev) => (prev === "reconnecting" ? "streaming" : prev));
        },
        onBatch: (batch) => {
          const counts = histogramRef.current;
          for (const draw of batch.draws) {
            if (draw <= 0 || draw >= 1 || Number.isNaN(draw)) continue;
            const bin = Math.min(BIN_COUNT - 1, Math.floor(draw * BIN_COUNT));
            counts[bin] += 1;
            totalDrawsRef.current += 1;
          }
          sawDataRef.current = true;
          setHasData(true);
          tweenRef.current?.setTarget(histogramDensity());
        },
        onDone: () => setState(sawDataRef.current ? "converged" : "empty"),
        onReconnecting: () => setState("reconnecting"),
        onError: (error) => {
          setErrorMessage(error instanceof Error ? error.message : "the stream dropped");
          setState("error");
        },
      });
    })();
  }, [method, flips, histogramDensity, teardown]);

  // Gridlines at quarters of theta and of density.
  const xTicks = [0, 0.25, 0.5, 0.75, 1];
  const yTicks = yScale.ticks(4);

  const barWidth = PLOT_W / BIN_COUNT;
  const analyticPath = densityPath(analyticBars, xScale, yScale);
  const analyticMean = betaMean(posteriorA, posteriorB);

  return (
    <div className={styles.root} data-state={state}>
      <figure className={`${styles.figure} ${state === "reconnecting" ? styles.reconnecting : ""}`}>
        <svg
          className={styles.svg}
          viewBox={`0 0 ${VIEW_W} ${VIEW_H}`}
          role="img"
          aria-label="Beta posterior over the coin bias"
          preserveAspectRatio="xMidYMid meet"
        >
          <g transform={`translate(${MARGIN.left},${MARGIN.top})`}>
            {/* Horizontal gridlines + density axis labels */}
            {yTicks.map((tick) => (
              <g key={`y-${tick}`} transform={`translate(0,${yScale(tick)})`}>
                <line className={styles.gridLine} x1={0} x2={PLOT_W} y1={0} y2={0} />
                <text className={styles.axisLabel} x={-8} y={4} textAnchor="end">
                  {tick.toFixed(1)}
                </text>
              </g>
            ))}

            {/* Vertical gridlines + theta axis labels */}
            {xTicks.map((tick) => (
              <g key={`x-${tick}`} transform={`translate(${xScale(tick)},0)`}>
                <line className={styles.gridLine} x1={0} x2={0} y1={0} y2={PLOT_H} />
                <text className={styles.axisLabel} x={0} y={PLOT_H + 20} textAnchor="middle">
                  {tick.toFixed(2)}
                </text>
              </g>
            ))}

            <text className={styles.axisTitle} x={PLOT_W} y={PLOT_H + 20} textAnchor="end">
              theta
            </text>

            {/* Posterior histogram: the streamed approximation. */}
            {bars.map((height, i) => {
              const top = yScale(Math.max(0, height));
              const h = Math.max(0, PLOT_H - top);
              return (
                <rect
                  // Bins are positional and fixed in count.
                  // biome-ignore lint/suspicious/noArrayIndexKey: fixed bin grid
                  key={i}
                  className={styles.bar}
                  x={i * barWidth + 1}
                  y={top}
                  width={Math.max(0, barWidth - 2)}
                  height={h}
                />
              );
            })}

            {/* Analytic Beta curve on top, fading in once when data first exists. */}
            <path
              className={`${styles.analytic} ${hasData ? styles.analyticVisible : ""}`}
              d={analyticPath}
            />
          </g>
        </svg>
      </figure>

      <div className={styles.readout}>
        <div className={styles.stat}>
          <span className={styles.statLabel}>posterior mean</span>
          <span className={styles.statValue}>
            {meanReadout !== null ? meanReadout.toFixed(4) : analyticMean.toFixed(4)}
          </span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>exact mean</span>
          <span className={styles.statValue}>{analyticMean.toFixed(4)}</span>
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
            Showing the prior as the starting posterior. Start a run to watch it converge.
          </span>
        </div>
      )}

      {state === "reconnecting" && (
        <div className={styles.banner}>
          <span className={`${styles.bannerText} ${styles.statusAccent}`}>
            Reconnecting to the engine. The current posterior is preserved.
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
