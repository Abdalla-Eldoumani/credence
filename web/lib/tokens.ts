/*
  The design tokens, surfaced to TS as CSS var()
  references. Components that pass colors to SVG attributes (fill, stroke) read
  them from here so no hex is ever hardcoded; everything resolves to the same
  CSS variables declared in app/globals.css.
*/

// Color
export const bg = "var(--bg)";
export const surface = "var(--surface)";
export const surfaceRaised = "var(--surface-raised)";
export const borderSubtle = "var(--border-subtle)";
export const border = "var(--border)";

export const text = "var(--text)";
export const textSecondary = "var(--text-secondary)";
export const textMuted = "var(--text-muted)";

export const accent = "var(--accent)";
export const accentStrong = "var(--accent-strong)";
export const accentMuted = "var(--accent-muted)";

export const focusRing = "var(--focus-ring)";

// Visualization color
export const vizPosterior = "var(--viz-posterior)";
export const vizPosteriorFill = "var(--viz-posterior-fill)";
export const vizAnalytic = "var(--viz-analytic)";
export const vizBand = "var(--viz-band)";
export const vizData = "var(--viz-data)";
export const vizGrid = "var(--viz-grid)";
export const vizAxis = "var(--viz-axis)";
export const vizTrace = "var(--viz-trace)";
export const vizParticle = "var(--viz-particle)";

// Typography
export const fontDisplay = "var(--font-display)";
export const fontBody = "var(--font-body)";
export const fontMono = "var(--font-mono)";

// Motion (the JS tween reads the curve control points; the value here is the
// CSS reference used on transition/animation properties in CSS Modules).
export const easeSettle = "var(--ease-settle)";
export const easeEntrance = "var(--ease-entrance)";
export const durMicro = "var(--dur-micro)";
export const durShort = "var(--dur-short)";
export const durMedium = "var(--dur-medium)";
export const durLong = "var(--dur-long)";

// The --ease-settle control points, in JS form, for the requestAnimationFrame
// tween in lib/clock.ts (CSS cannot drive an SVG attribute tween directly).
// Kept here so the JS easing and the CSS token stay the same curve.
export const easeSettlePoints: readonly [number, number, number, number] = [0.22, 1, 0.36, 1];
