import { easeSettlePoints } from "@/lib/tokens";

// A Canvas draw driver for the regression band: the set of plausible lines
// y = w * x for slopes w drawn from the posterior. It paces redraws off the
// CLIENT clock (requestAnimationFrame), eases the rendered slopes toward each
// new posterior sample with the --ease-settle curve so the band CONTRACTS
// smoothly onto the trend, and draws AT MOST a bounded number of lines per
// frame so a large sample count never stalls the main thread (edge case
// streaming volume, a hardening note). Token colors come from getComputedStyle on
// the CSS variables, never a hardcoded hex.

// The posterior interpolation window from design system (180ms to 240ms); the
// midpoint, matching the histogram tween in lib/clock.ts so both morphs read as
// the same motion.
const DURATION_MS = 210;

// The per-frame work cap: the most lines drawn in a single frame. The band is a
// density, so a few hundred lines already read as a continuous wash; capping the
// count keeps each frame bounded however large the run's sample set grows. The
// rendered slopes are an evenly-thinned, order-sorted subset of the cloud, so a
// cap of 200 still spans the full spread.
const MAX_LINES_PER_FRAME = 200;

// Evaluate cubic-bezier(x1,y1,x2,y2) the way CSS does: solve for the parameter u
// where the x-component equals the elapsed fraction, then return the y-component.
// p0 and p3 are fixed at (0,0) and (1,1). Kept local to the renderer so the Canvas
// easing and the SVG tween (lib/clock.ts) stay the identical curve without a
// shared import surface that would couple their internals.
function bezier(p1x: number, p1y: number, p2x: number, p2y: number, x: number): number {
  if (x <= 0) return 0;
  if (x >= 1) return 1;

  const cx = 3 * p1x;
  const bx = 3 * (p2x - p1x) - cx;
  const ax = 1 - cx - bx;
  const sampleX = (u: number) => ((ax * u + bx) * u + cx) * u;
  const sampleDerivX = (u: number) => (3 * ax * u + 2 * bx) * u + cx;

  let u = x;
  for (let i = 0; i < 8; i++) {
    const dx = sampleX(u) - x;
    if (Math.abs(dx) < 1e-6) break;
    const d = sampleDerivX(u);
    if (Math.abs(d) < 1e-6) break;
    u -= dx / d;
  }
  if (u < 0 || u > 1) {
    let lo = 0;
    let hi = 1;
    u = x;
    for (let i = 0; i < 20; i++) {
      const xAt = sampleX(u);
      if (Math.abs(xAt - x) < 1e-6) break;
      if (xAt < x) lo = u;
      else hi = u;
      u = (lo + hi) / 2;
    }
  }

  const cy = 3 * p1y;
  const by = 3 * (p2y - p1y) - cy;
  const ay = 1 - cy - by;
  return ((ay * u + by) * u + cy) * u;
}

const [E1X, E1Y, E2X, E2Y] = easeSettlePoints;
const easeSettle = (x: number): number => bezier(E1X, E1Y, E2X, E2Y, x);

// Read a CSS variable as a concrete color string the Canvas can use for
// fillStyle/strokeStyle. Canvas cannot resolve var(--x) itself, so the value is
// pulled from the computed style of a live element (design system: resolve tokens
// via getComputedStyle, never hardcode hex). A blank result (SSR or detached node)
// falls back to transparent so a missing token degrades to no-paint, not a crash.
export function resolveCssVar(element: Element, name: string): string {
  const value = getComputedStyle(element).getPropertyValue(name).trim();
  return value === "" ? "transparent" : value;
}

// Evenly thin an array to at most k elements, preserving order and span (the same
// striding the engine uses to subsample draws). Used to cap the rendered line
// count without dropping the tails of the slope spread.
function thin(values: number[], k: number): number[] {
  if (values.length <= k) return values.slice();
  const out = new Array<number>(k);
  const stride = values.length / k;
  for (let i = 0; i < k; i++) {
    out[i] = values[Math.min(values.length - 1, Math.floor(i * stride))];
  }
  return out;
}

// Build the capped, order-sorted slope set the renderer tweens. Sorting makes the
// tween a contraction of order statistics: position i eases from its old value to
// the new i-th order statistic, so the cloud's SPREAD narrows smoothly as the
// posterior concentrates, which is the band collapsing onto the trend.
function prepareSlopes(slopes: number[]): number[] {
  const finite = slopes.filter((w) => Number.isFinite(w));
  finite.sort((a, b) => a - b);
  return thin(finite, MAX_LINES_PER_FRAME);
}

export type BandRendererOptions = {
  // Maps a data x to a canvas pixel x.
  toPixelX: (dataX: number) => number;
  // Maps a data y to a canvas pixel y.
  toPixelY: (dataY: number) => number;
  // The x-range (in data units) the lines span, so y = w * x is drawn edge to edge.
  xDomain: () => [number, number];
  // The element whose computed style carries the --viz-band token (the canvas or
  // its container); read each frame so a theme change is picked up.
  styleSource: () => Element | null;
  // Painted each frame after the clear and before the band, for anything that
  // belongs under the translucent lines (the grid). The caller controls the order.
  drawUnderlay?: (ctx: CanvasRenderingContext2D) => void;
  // Called after the band each frame, for anything that must sit on top (the
  // draggable data points, so they stay visible over the wash).
  drawOverlay?: (ctx: CanvasRenderingContext2D) => void;
  reducedMotion?: boolean;
};

export type BandRenderer = {
  // Update the posterior slope set the band eases toward. Re-anchors from the
  // current eased slopes so a burst of batches keeps contracting, not snapping.
  setSlopes: (slopes: number[]) => void;
  // Force an immediate repaint at the current eased slopes (on drag or resize),
  // without restarting the ease.
  redraw: () => void;
  setReducedMotion: (reduced: boolean) => void;
  // Stop the frame loop and release the rAF handle.
  stop: () => void;
};

/**
 * Create the capped Canvas band renderer. It owns a current and a target slope
 * array; each animation frame it eases current toward target over DURATION_MS
 * with --ease-settle and repaints, drawing at most MAX_LINES_PER_FRAME lines. The
 * cap is the load-bearing performance contract: the rendered line count is bounded
 * regardless of how many slopes the run produces. In reduced-motion it jumps to
 * the target on the next frame (discrete redraw) instead of easing.
 */
export function makeBandRenderer(
  ctx: CanvasRenderingContext2D,
  opts: BandRendererOptions,
): BandRenderer {
  let current: number[] = [];
  let start: number[] = [];
  let target: number[] = [];
  let startTime = 0;
  let reducedMotion = opts.reducedMotion ?? false;
  let rafId: number | null = null;

  // Grow or shrink the eased buffers to a new length, padding from the target so a
  // change in line count (e.g. the first batch after the empty state) does not
  // flash. New slots take the target value, so they fade in from their final place.
  const resize = (length: number) => {
    if (current.length === length) return;
    const pad = (arr: number[]) => {
      const next = new Array<number>(length);
      for (let i = 0; i < length; i++) {
        next[i] = i < arr.length ? arr[i] : (target[i] ?? 0);
      }
      return next;
    };
    current = pad(current);
    start = pad(start);
  };

  const paint = (slopes: number[]) => {
    const canvas = ctx.canvas;
    // Clear in device pixels; the caller sets the transform so 1 unit is 1 CSS px.
    ctx.save();
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.restore();

    opts.drawUnderlay?.(ctx);

    const source = opts.styleSource() ?? canvas;
    const bandColor = resolveCssVar(source, "--viz-band");
    const [x0, x1] = opts.xDomain();
    const px0 = opts.toPixelX(x0);
    const px1 = opts.toPixelX(x1);

    // The band is many translucent lines; --viz-band already carries the alpha, so
    // overlapping strokes accumulate into a denser wash where slopes agree (the
    // trend) and a thin haze in the tails. No stroke beyond that alpha (DESIGN
    // SYSTEM: band stroke none), so the lines are hairline.
    ctx.strokeStyle = bandColor;
    ctx.lineWidth = 1;
    ctx.beginPath();
    for (let i = 0; i < slopes.length; i++) {
      const w = slopes[i];
      ctx.moveTo(px0, opts.toPixelY(w * x0));
      ctx.lineTo(px1, opts.toPixelY(w * x1));
    }
    ctx.stroke();

    opts.drawOverlay?.(ctx);
  };

  const frame = (now: number) => {
    rafId = null;
    if (reducedMotion) {
      for (let i = 0; i < target.length; i++) current[i] = target[i];
      paint(current);
      return;
    }

    const elapsed = now - startTime;
    const t = Math.min(1, elapsed / DURATION_MS);
    const eased = easeSettle(t);
    for (let i = 0; i < target.length; i++) {
      current[i] = start[i] + (target[i] - start[i]) * eased;
    }
    paint(current);

    if (t < 1) {
      rafId = requestAnimationFrame(frame);
    }
  };

  const kick = () => {
    if (rafId !== null) return;
    rafId = requestAnimationFrame(frame);
  };

  return {
    setSlopes(slopes: number[]) {
      target = prepareSlopes(slopes);
      resize(target.length);
      if (reducedMotion) {
        startTime = 0;
        kick();
        return;
      }
      // Re-anchor the start at the current eased slopes and restart the client
      // clock, so a burst of batches keeps contracting instead of snapping.
      start = current.slice();
      startTime = performance.now();
      kick();
    },
    redraw() {
      paint(current);
    },
    setReducedMotion(reduced: boolean) {
      reducedMotion = reduced;
      if (reduced) kick();
    },
    stop() {
      if (rafId !== null) {
        cancelAnimationFrame(rafId);
        rafId = null;
      }
    },
  };
}
