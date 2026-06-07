# Breakpoint sweep: 375 / 768 / 1440

The diagnostics panel and the method race must render cleanly at three widths with no
overflow or clipping, and the two-column layouts must collapse to a single stacked
column below 768px (design system Layout, the spec).

The Playwright screenshot pass runs separately with the engine up in WSL
(`cabal run credence-server`) and the dev server on the host
(`npm run dev`), driving a browser at each width and saving to `web/.playwright-mcp/`.
This file records the matrix to capture and the programmatic checks already done.

## Matrix to capture (pending)

| Route          | 375 (mobile)      | 768 (tablet)       | 1440 (desktop)     |
| -------------- | ----------------- | ------------------ | ------------------ |
| `/coin`        | pending           | pending            | pending            |
| `/regression`  | pending           | pending            | pending            |
| `/localization`| pending           | pending            | pending            |
| `/race`        | pending           | pending            | pending            |

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

## Programmatic checks done (this run)

The responsive CSS was verified statically and through the production build; the visual
smoothness, cloud concentration, and side-by-side simultaneity remain the human/Playwright
judgment the matrix above captures.

- `npm run build` succeeds; the route manifest lists `/`, `/coin`, `/localization`,
  `/race`, `/regression`, all prerendered as static content.
- `npm run typecheck` and `npm run lint` pass.
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

## Concurrency (the race is truly simultaneous)

Confirmed end to end: two POSTs to `/run/localization` on the SAME readings minted two
distinct run ids (importance `0`, mh `1`; the second did not replace the first), and
draining both `/stream/<id>` in one ~4s window returned 81 and 78 `data:` lines, the
importance summary carrying ESS (acceptance null) and the MH summary carrying acceptance
(ESS null), both means near the true source 3.0. So the two clouds animate at once.
