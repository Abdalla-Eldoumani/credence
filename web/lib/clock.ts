import { easeSettlePoints } from "@/lib/tokens";

// A client-clock animation driver. It holds a current array of values and a target
// array and, each animation frame, eases the current toward the target over
// ~DURATION ms with the --ease-settle curve evaluated in JS. setTarget updates the
// target when a new summary arrives; the frame loop advances on the CLIENT clock
// (requestAnimationFrame), so a burst of arrivals re-aims the tween rather than
// snapping the bars. This decoupling of render pacing from network arrival is the
// load-bearing motion contract (edge case slow/bursty network).

// The posterior interpolation window from design system (180ms to 240ms); the
// midpoint keeps the morph in band.
const DURATION_MS = 210;

// Evaluate a cubic-bezier(x1, y1, x2, y2) easing the way CSS does: solve for the
// curve parameter u where the x-component equals the elapsed fraction, then return
// the y-component. p0 and p3 are fixed at (0,0) and (1,1).
function bezier(p1x: number, p1y: number, p2x: number, p2y: number, x: number): number {
  if (x <= 0) return 0;
  if (x >= 1) return 1;

  const cx = 3 * p1x;
  const bx = 3 * (p2x - p1x) - cx;
  const ax = 1 - cx - bx;
  const sampleX = (u: number) => ((ax * u + bx) * u + cx) * u;
  const sampleDerivX = (u: number) => (3 * ax * u + 2 * bx) * u + cx;

  // Newton-Raphson from the identity guess, then a bisection fallback for the flat
  // regions where the derivative is near zero.
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

export type TweenOptions = {
  // When true (prefers-reduced-motion), skip easing and jump straight to the
  // target on the next frame, so information is available without sustained motion.
  reducedMotion?: boolean;
};

export type Tween = {
  // Update the target the current values ease toward. Re-anchors from the current
  // (partially eased) values so a mid-flight retarget continues smoothly.
  setTarget: (next: number[]) => void;
  // The current eased values (a stable array reference, mutated in place).
  current: () => number[];
  // Toggle reduced-motion behavior at runtime (the matchMedia listener may flip it).
  setReducedMotion: (reduced: boolean) => void;
  // Stop the frame loop and release the rAF handle.
  stop: () => void;
};

/**
 * Create a client-clock tween over an array of values. onFrame is called with the
 * current values each animation frame while a tween is in flight (and once when it
 * settles), so the caller can repaint. The initial values seed both current and
 * target, so before any setTarget the array is steady.
 */
export function makeTween(
  initial: number[],
  onFrame: (values: number[]) => void,
  options: TweenOptions = {},
): Tween {
  let current = initial.slice();
  let start = initial.slice();
  let target = initial.slice();
  let startTime = 0;
  let animating = false;
  let reducedMotion = options.reducedMotion ?? false;
  let rafId: number | null = null;

  const resize = (length: number) => {
    // When the bin count changes (e.g. first data after the prior), grow or shrink
    // the buffers, padding new slots from the target so a resize does not flash.
    if (current.length === length) return;
    const pad = (arr: number[]) => {
      const next = new Array<number>(length);
      for (let i = 0; i < length; i++) {
        next[i] = i < arr.length ? arr[i] : 0;
      }
      return next;
    };
    current = pad(current);
    start = pad(start);
  };

  const frame = (now: number) => {
    rafId = null;
    if (reducedMotion) {
      for (let i = 0; i < target.length; i++) current[i] = target[i];
      animating = false;
      onFrame(current);
      return;
    }

    const elapsed = now - startTime;
    const t = Math.min(1, elapsed / DURATION_MS);
    const eased = easeSettle(t);
    for (let i = 0; i < target.length; i++) {
      current[i] = start[i] + (target[i] - start[i]) * eased;
    }
    onFrame(current);

    if (t < 1) {
      rafId = requestAnimationFrame(frame);
    } else {
      animating = false;
    }
  };

  const kick = () => {
    if (rafId !== null) return;
    rafId = requestAnimationFrame(frame);
  };

  return {
    setTarget(next: number[]) {
      resize(next.length);
      target = next.slice();
      if (reducedMotion) {
        // Discrete jump on the next frame.
        startTime = 0;
        animating = true;
        kick();
        return;
      }
      // Re-anchor the start at the current eased values and restart the client
      // clock, so a burst of retargets keeps morphing instead of snapping.
      start = current.slice();
      startTime = performance.now();
      animating = true;
      kick();
    },
    current() {
      return current;
    },
    setReducedMotion(reduced: boolean) {
      reducedMotion = reduced;
      if (reduced && animating) {
        // Settle immediately on the next frame.
        kick();
      }
    },
    stop() {
      if (rafId !== null) {
        cancelAnimationFrame(rafId);
        rafId = null;
      }
      animating = false;
    },
  };
}
