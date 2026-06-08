// Hand-checked correctness oracle for the diagnostics helpers, run with Node's
// built-in test runner (node --test, no extra dependency). These pin the ESS and
// acceptance-rate formulas to numbers computed by hand, so a wrong formula fails
// here rather than rendering a smooth, plausible, wrong figure.
// Run from web/: node --test lib/diagnostics.test.ts

import assert from "node:assert/strict";
import { test } from "node:test";
import { acceptanceRate, effectiveSampleSize, normalizeWeights } from "./diagnostics.ts";

test("ESS of four equal weights is exactly 4", () => {
  // Each weight is 1/4; sum of squares is 4 * (1/4)^2 = 1/4; ESS = 1 / (1/4) = 4.
  const weights = [0.25, 0.25, 0.25, 0.25];
  assert.equal(effectiveSampleSize(weights), 4);
});

test("ESS of a skewed weight vector matches the hand-computed value", () => {
  // [0.7, 0.1, 0.1, 0.1]: sum of squares = 0.49 + 0.01 + 0.01 + 0.01 = 0.52;
  // ESS = 1 / 0.52 = 1.923076...; one heavy weight pulls the effective count down.
  const weights = [0.7, 0.1, 0.1, 0.1];
  const ess = effectiveSampleSize(weights);
  assert.ok(Math.abs(ess - 1.9230769230769231) < 1e-12, `expected ESS ~1.9230769, got ${ess}`);
});

test("ESS of a single unit weight is 1", () => {
  // One sample carrying all the mass: sum of squares = 1; ESS = 1.
  assert.equal(effectiveSampleSize([1]), 1);
});

test("ESS of an empty weight vector is 0, not a divide-by-zero", () => {
  assert.equal(effectiveSampleSize([]), 0);
});

test("ESS of all-zero weights is 0, not a divide-by-zero", () => {
  assert.equal(effectiveSampleSize([0, 0, 0]), 0);
});

test("acceptanceRate(3, 10) is 0.3", () => {
  assert.equal(acceptanceRate(3, 10), 0.3);
});

test("acceptanceRate with zero proposed steps is 0, not NaN", () => {
  assert.equal(acceptanceRate(0, 0), 0);
});

test("acceptanceRate clamps an out-of-range pair into [0, 1]", () => {
  assert.equal(acceptanceRate(12, 10), 1);
  assert.equal(acceptanceRate(-1, 10), 0);
});

test("normalizeWeights rescales raw weights to sum to 1", () => {
  // Raw [1, 1, 2] sums to 4, so normalized is [0.25, 0.25, 0.5]; feeding that to
  // the ESS gives 1 / (0.0625 + 0.0625 + 0.25) = 1 / 0.375 = 2.6666...
  const normalized = normalizeWeights([1, 1, 2]);
  assert.deepEqual(normalized, [0.25, 0.25, 0.5]);
  const ess = effectiveSampleSize(normalized);
  assert.ok(Math.abs(ess - 2.6666666666666665) < 1e-12, `expected ESS ~2.6667, got ${ess}`);
});

test("normalizeWeights of all-zero input is empty (ESS then reads 0)", () => {
  assert.deepEqual(normalizeWeights([0, 0]), []);
  assert.equal(effectiveSampleSize(normalizeWeights([0, 0])), 0);
});
