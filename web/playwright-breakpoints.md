# Responsive breakpoints to verify: 375 / 768 / 1440

The diagnostics panel and the method race must render cleanly at three widths with no
overflow or clipping, and the two-column layouts must collapse to a single stacked
column below 768px.

Drive a browser at each width with the engine up (`cabal run credence-server`) and the
dev server on `npm run dev`, and save screenshots to `web/.playwright-mcp/`.

## Matrix to capture

| Route           | 375 (mobile) | 768 (tablet) | 1440 (desktop) |
| --------------- | ------------ | ------------ | -------------- |
| `/coin`         | check        | check        | check          |
| `/regression`   | check        | check        | check          |
| `/localization` | check        | check        | check          |
| `/tracking`     | check        | check        | check          |
| `/mixture`      | check        | check        | check          |
| `/race`         | check        | check        | check          |

Screenshot path convention: `web/.playwright-mcp/<route>-<width>.png`
(for example `web/.playwright-mcp/race-375.png`).

What each capture confirms:

- 375: single column everywhere. The race stacks Importance over Metropolis-Hastings;
  the diagnostics stat grid wraps to one column; no horizontal scrollbar; the canvas
  cloud and SVG traceplot scale down to the column width.
- 768: the collapse boundary. At exactly 768px the `max-width: 768px` rules apply, so
  the LivePanel and the race are single-column; the diagnostics panel sits comfortably.
- 1440: two columns. The race shows Importance and Metropolis-Hastings side by side,
  each with its own cloud, diagnostics, and traceplot; the LivePanel shows controls
  left and the visualizer column right; content is capped at 1200px and centered.

## What the responsive CSS guarantees

- Every panel that renders at these widths carries a `@media (max-width: 768px)` rule:
  `LivePanel`, `MethodRace`, `Diagnostics`, `Traceplot`, `CloudVisualizer`,
  `RegressionVisualizer`, `CoinVisualizer`, and the gallery grid. Panel padding is
  `--space-6` desktop / `--space-4` small per the rule.
- The race grid is `grid-template-columns: repeat(2, minmax(0, 1fr))` collapsing to
  `minmax(0, 1fr)` at 768px; `minmax(0, ...)` plus `min-width: 0` on the column lets the
  720px-wide canvas shrink to the track instead of forcing overflow.
- No fixed pixel widths that would overflow a narrow column: the only `px` widths are the
  `max-width: 1200px` content cap (a ceiling, not a floor). The cloud canvas is
  `width: 100%; height: auto` with an `aspect-ratio`, the traceplot SVG is
  `width: 100%; height: auto` with a `viewBox`, and the diagnostics grid is
  `repeat(auto-fit, minmax(min(100%, 9rem), 1fr))` so a stat track never exceeds the panel.
- Diagnostics numeric readouts use `--font-mono` with `font-variant-numeric: tabular-nums`;
  `node --test lib/diagnostics.test.ts` passes (10/10), so the displayed figures are
  unchanged.
