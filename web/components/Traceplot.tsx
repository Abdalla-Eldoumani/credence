"use client";

import { forwardRef, useEffect, useImperativeHandle, useRef } from "react";
import styles from "./Traceplot.module.css";

// An SVG traceplot of the latent value over sample index. The load-bearing
// contract (a hardening note) is the windowed buffer: only the most recent WINDOW
// samples are kept, so the path's d-string is bounded however long the run, and
// it is rebuilt only when new samples ARRIVE (a few times, paced by the stream),
// never every animation frame. The per-frame work is a transform-only scroll (a
// cheap, composited translate), so a long run stays smooth. Under
// prefers-reduced-motion the window steps without the continuous glide.

// The SVG coordinate space (a viewBox keeps it responsive).
const VIEW_W = 720;
const VIEW_H = 200;
const MARGIN = { top: 12, right: 12, bottom: 24, left: 40 };
const PLOT_W = VIEW_W - MARGIN.left - MARGIN.right;
const PLOT_H = VIEW_H - MARGIN.top - MARGIN.bottom;

// The window: the most recent WINDOW samples are shown. This bounds the d-string,
// so the path never grows without limit; older samples scroll off the left.
const WINDOW = 240;

// Horizontal spacing between successive samples, in plot pixels.
const STEP_X = PLOT_W / WINDOW;

// The default y-domain (the slope's plausible range under the Normal(0,3) prior).
const Y_MIN = -8;
const Y_MAX = 8;

export type TraceplotHandle = {
  // Append samples to the trace (called from the page's single stream handler).
  push: (values: number[]) => void;
  // Clear the trace for a fresh run.
  reset: () => void;
};

type TraceplotProps = {
  // The latent's display range, defaulting to the wide slope range. The coin page
  // can pass [0, 1] for theta so the trace fills the band.
  yDomain?: [number, number];
  label?: string;
};

export const Traceplot = forwardRef<TraceplotHandle, TraceplotProps>(function Traceplot(
  { yDomain = [Y_MIN, Y_MAX], label = "slope" }: TraceplotProps,
  ref,
) {
  const pathRef = useRef<SVGPathElement | null>(null);
  // The group the trace lives in; translated by a sub-step fraction each frame so
  // the window glides between discrete shifts instead of jumping a whole STEP_X.
  const groupRef = useRef<SVGGElement | null>(null);
  const reducedMotionRef = useRef(false);

  const [yMin, yMax] = yDomain;
  // Stable refs to the domain so the rAF loop reads the latest without re-binding.
  const yMinRef = useRef(yMin);
  const yMaxRef = useRef(yMax);
  yMinRef.current = yMin;
  yMaxRef.current = yMax;

  // The windowed ring of the last WINDOW sample values (most recent last). Bounded,
  // so the rebuilt d-string is bounded too.
  const bufRef = useRef<number[]>([]);
  // Samples that arrived since the last flush, drained on the client clock so a
  // burst paints once per frame, not once per sample.
  const pendingRef = useRef<number[]>([]);
  // The number of samples that have scrolled off the left edge since the window
  // filled, used as the integer scroll position; the fractional remainder is eased
  // so the glide is continuous.
  const scrolledRef = useRef(0);
  const easedScrollRef = useRef(0);
  const rafRef = useRef<number | null>(null);

  const toPixelY = (value: number): number =>
    MARGIN.top + (1 - (value - yMinRef.current) / (yMaxRef.current - yMinRef.current)) * PLOT_H;

  // Rebuild the bounded d-string from the ring. Called only on flush (when samples
  // arrive), not per frame. x is the point's index in the window, so the path is
  // drawn at fixed local coordinates and the group transform does the scrolling.
  const rebuildPath = () => {
    const buf = bufRef.current;
    let d = "";
    for (let i = 0; i < buf.length; i++) {
      const x = i * STEP_X;
      const y = toPixelY(buf[i]);
      d += i === 0 ? `M${x.toFixed(2)},${y.toFixed(2)}` : ` L${x.toFixed(2)},${y.toFixed(2)}`;
    }
    pathRef.current?.setAttribute("d", d);
  };

  // Drain pending samples into the ring, evicting the oldest past WINDOW and
  // counting each eviction as one scroll step. Rebuilds the path once after the
  // drain. This is the only place the d-string changes.
  const flush = useRef<() => void>(() => {});
  flush.current = () => {
    const pending = pendingRef.current;
    if (pending.length === 0) return;
    const buf = bufRef.current;
    for (const value of pending) {
      buf.push(value);
      if (buf.length > WINDOW) {
        buf.shift();
        scrolledRef.current += 1;
      }
    }
    pendingRef.current = [];
    rebuildPath();
  };

  // The client-clock loop: ease the fractional scroll toward the integer scroll
  // position and translate the group by the remainder, so the window glides as
  // points drop off rather than stepping a whole sample. Decoupled from arrival
  // (edge case slow network). Reduced motion snaps to the step.
  useEffect(() => {
    const frame = () => {
      flush.current();
      const target = scrolledRef.current;
      if (reducedMotionRef.current) {
        easedScrollRef.current = target;
      } else {
        easedScrollRef.current += (target - easedScrollRef.current) * 0.18;
        if (Math.abs(target - easedScrollRef.current) < 0.001) easedScrollRef.current = target;
      }
      // Translate left by the eased fraction of a step beyond the last whole shift,
      // so the path slides smoothly. The whole shifts are already reflected by the
      // ring eviction; only the in-between fraction is animated here.
      const fraction = target - easedScrollRef.current;
      groupRef.current?.setAttribute("transform", `translate(${(fraction * STEP_X).toFixed(2)},0)`);
      rafRef.current = requestAnimationFrame(frame);
    };
    rafRef.current = requestAnimationFrame(frame);
    return () => {
      if (rafRef.current !== null) cancelAnimationFrame(rafRef.current);
    };
  }, []);

  // Track prefers-reduced-motion.
  useEffect(() => {
    if (typeof window === "undefined" || !window.matchMedia) return;
    const query = window.matchMedia("(prefers-reduced-motion: reduce)");
    const apply = () => {
      reducedMotionRef.current = query.matches;
    };
    apply();
    query.addEventListener("change", apply);
    return () => query.removeEventListener("change", apply);
  }, []);

  useImperativeHandle(
    ref,
    () => ({
      push(values: number[]) {
        for (const value of values) {
          if (Number.isFinite(value)) pendingRef.current.push(value);
        }
      },
      reset() {
        bufRef.current = [];
        pendingRef.current = [];
        scrolledRef.current = 0;
        easedScrollRef.current = 0;
        pathRef.current?.setAttribute("d", "");
        groupRef.current?.setAttribute("transform", "translate(0,0)");
      },
    }),
    [],
  );

  // Static y gridlines + labels (the x-axis scrolls, so it carries no fixed ticks).
  const yTicks = [yMin, (yMin + yMax) / 2, yMax];

  return (
    <figure className={styles.figure}>
      <figcaption className={styles.caption}>traceplot</figcaption>
      <svg
        className={styles.svg}
        viewBox={`0 0 ${VIEW_W} ${VIEW_H}`}
        role="img"
        aria-label={`Traceplot of the ${label} over sample index`}
        preserveAspectRatio="xMidYMid meet"
      >
        {/* Static frame: y gridlines and labels, drawn once and not scrolled. */}
        <g transform={`translate(${MARGIN.left},0)`}>
          {yTicks.map((tick) => (
            <g key={`y-${tick}`} transform={`translate(0,${toPixelY(tick)})`}>
              <line className={styles.gridLine} x1={0} x2={PLOT_W} y1={0} y2={0} />
              <text className={styles.axisLabel} x={-8} y={4} textAnchor="end">
                {tick.toFixed(1)}
              </text>
            </g>
          ))}
          <text className={styles.axisLabel} x={0} y={PLOT_H + 18} textAnchor="start">
            sample index
          </text>
        </g>

        {/* The scrolling trace. A clip keeps the path inside the plot as it slides. */}
        <clipPath id="trace-clip">
          <rect x={MARGIN.left} y={MARGIN.top} width={PLOT_W} height={PLOT_H} />
        </clipPath>
        <g transform={`translate(${MARGIN.left},0)`} clipPath="url(#trace-clip)">
          <g ref={groupRef} transform="translate(0,0)">
            <path ref={pathRef} className={styles.trace} d="" />
          </g>
        </g>
      </svg>
    </figure>
  );
});
