// Pure diagnostics helpers that MIRROR the engine's definitions, so the numbers
// the panel shows are the same quantities the server computes, not a plausible
// re-derivation (a hardening note). The engine supplies acceptanceRate and
// effectiveSampleSize directly on each Summary; these helpers compute the same
// values from raw weights for the cases where the panel derives them, and are
// pinned to hand-checked numbers by diagnostics.test.mjs so a wrong formula fails
// the test rather than rendering a smooth wrong figure.

/**
 * The importance-sampling effective sample size: 1 / sum(w_i^2) over the
 * NORMALIZED weights (which sum to 1). This is exactly the engine's definition
 * (Credence.Inference.Importance: normalizedLogWeights then 1/sum(w_i^2)). With N
 * equal weights of 1/N the sum of squares is N * (1/N)^2 = 1/N, so the ESS is N
 * (every sample fully counts); a weight vector dominated by one entry drives the
 * sum of squares toward 1 and the ESS toward 1 (one sample carries the mass).
 *
 * The input is expected already normalized (the engine normalizes in log-space
 * before sending). An empty vector has no samples, so the ESS is 0 rather than a
 * divide-by-zero. If the squares sum to 0 (all-zero weights, a degenerate input),
 * the ESS is 0 for the same reason.
 */
export function effectiveSampleSize(normalizedWeights: number[]): number {
  if (normalizedWeights.length === 0) return 0;
  let sumSquares = 0;
  for (const w of normalizedWeights) {
    sumSquares += w * w;
  }
  if (sumSquares <= 0) return 0;
  return 1 / sumSquares;
}

/**
 * The Metropolis-Hastings acceptance rate: accepted steps over proposed steps,
 * exactly the engine's accepts / n (Credence.Inference.MH.mhChain). With no steps
 * proposed the rate is 0 rather than a divide-by-zero. The result is clamped into
 * [0, 1] so a malformed pair can never render an out-of-range figure.
 */
export function acceptanceRate(accepts: number, total: number): number {
  if (total <= 0) return 0;
  const rate = accepts / total;
  return Math.max(0, Math.min(1, rate));
}

/**
 * Normalize raw (possibly unnormalized) non-negative weights to sum to 1, the
 * step the engine does in log-space before the ESS. Provided for when the panel
 * is handed raw weights rather than a precomputed ESS; an all-zero or empty input
 * returns an empty vector (there is nothing to normalize), which effectiveSampleSize
 * then reads as an ESS of 0.
 */
export function normalizeWeights(weights: number[]): number[] {
  let total = 0;
  for (const w of weights) {
    total += w;
  }
  if (total <= 0) return [];
  return weights.map((w) => w / total);
}
