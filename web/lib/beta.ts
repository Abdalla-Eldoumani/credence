// The analytic Beta(a, b) density, in log-space for stability. The coin posterior
// is Beta(priorA + heads, priorB + tails); both the histogram target and the
// crisp overlay curve are evaluated from these functions so they share one source
// of truth. Pure math, no design tokens.

// Lanczos coefficients (g = 7, n = 9), the standard set. lgamma via the Lanczos
// approximation with the reflection formula for x < 0.5, accurate to ~15 digits
// across the range the Beta density needs.
const LANCZOS_G = 7;
const LANCZOS_COEFFICIENTS = [
  0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
  -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6,
  1.5056327351493116e-7,
];

/** Natural log of the gamma function, stable for x > 0 via Lanczos. */
export function lgamma(x: number): number {
  if (x < 0.5) {
    // Reflection: Γ(x)Γ(1-x) = π / sin(πx), so lgamma(x) = ln(π/sin(πx)) - lgamma(1-x).
    return Math.log(Math.PI / Math.sin(Math.PI * x)) - lgamma(1 - x);
  }
  const z = x - 1;
  let a = LANCZOS_COEFFICIENTS[0];
  const t = z + LANCZOS_G + 0.5;
  for (let i = 1; i < LANCZOS_G + 2; i++) {
    a += LANCZOS_COEFFICIENTS[i] / (z + i);
  }
  return 0.5 * Math.log(2 * Math.PI) + (z + 0.5) * Math.log(t) - t + Math.log(a);
}

/** Log of the Beta function B(a, b) = Γ(a)Γ(b)/Γ(a+b). */
export function lbeta(a: number, b: number): number {
  return lgamma(a) + lgamma(b) - lgamma(a + b);
}

/**
 * Log Beta(a, b) density at x. Returns -Infinity outside the open interval (0, 1)
 * and at the boundaries when the corresponding shape pushes the density to zero,
 * so the linear-space pdf stays finite (0) rather than producing NaN.
 */
export function betaLogPdf(a: number, b: number, x: number): number {
  if (x <= 0 || x >= 1) {
    return Number.NEGATIVE_INFINITY;
  }
  return (a - 1) * Math.log(x) + (b - 1) * Math.log(1 - x) - lbeta(a, b);
}

/** Beta(a, b) density at x, finite on [0, 1] (0 at the endpoints for a, b > 1). */
export function betaPdf(a: number, b: number, x: number): number {
  const logPdf = betaLogPdf(a, b, x);
  return Number.isFinite(logPdf) ? Math.exp(logPdf) : 0;
}

/**
 * Analytic Beta(a, b) bar heights over theta in (0, 1), one value per bin
 * evaluated at the bin center. The histogram bars and the overlay both read from
 * here, so the approximation and the exact curve are sampled on the same grid.
 * Values are finite; endpoints use bin centers so a > 1, b > 1 densities never hit
 * the exact 0/1 singularity.
 */
export function betaBars(a: number, b: number, binCount: number): number[] {
  const bars = new Array<number>(binCount);
  const width = 1 / binCount;
  for (let i = 0; i < binCount; i++) {
    const center = (i + 0.5) * width;
    bars[i] = betaPdf(a, b, center);
  }
  return bars;
}

/** Mean of Beta(a, b) = a / (a + b). The posterior mean the engine converges to. */
export function betaMean(a: number, b: number): number {
  return a / (a + b);
}
