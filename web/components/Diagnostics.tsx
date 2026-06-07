"use client";

import { acceptanceRate, effectiveSampleSize } from "@/lib/diagnostics";
import type { Method, Summary } from "@/lib/stream";
import styles from "./Diagnostics.module.css";

// The live diagnostics readout: the MH acceptance rate and the importance-sampling
// effective sample size, in mono tabular-nums so the figures do not jitter as they
// update. The values come from the latest stream Summary (the engine computes
// them with the exact formulas in lib/diagnostics, which the node --test oracle
// pins). When raw normalized weights are supplied instead, the ESS is computed
// here through the same effectiveSampleSize helper, so the displayed number is the
// tested quantity either way (a hardening note). A run that hits its length cap
// without the chosen diagnostic settling surfaces a "not converged" note through
// the accent and weight, the single status hue (design system, edge case
// non-convergence).

type DiagnosticsProps = {
  // The latest running summary, or null before the first arrives.
  summary: Summary | null;
  // The method this run uses, which fixes which diagnostic is meaningful:
  // importance reports ESS (acceptance is null), MH reports acceptance (ESS null).
  method: Method;
  // Optional raw normalized weights for the latest batch; when present the ESS is
  // computed here via the tested helper rather than read off the summary.
  weights?: number[];
  // True once the run has ended, so the readout can flag a chain that finished
  // without a healthy acceptance rate / ESS as not converged.
  done?: boolean;
};

// A healthy MH acceptance rate sits well inside (0, 1); a chain stuck near 0 or
// pinned near 1 has mixed poorly. These are loose legibility bounds for the
// not-converged note, not an inference decision.
const ACCEPT_LOW = 0.05;
const ACCEPT_HIGH = 0.95;

export function Diagnostics({ summary, method, weights, done = false }: DiagnosticsProps) {
  const isMH = method === "MH";

  // Acceptance rate: meaningful for MH only. Clamp the wire value through the
  // tested helper so a malformed rate can never render out of range. When the
  // summary carries a non-null acceptanceRate it is already accepts/total, so it
  // is clamped via acceptanceRate(rate, 1) to reuse the same guard.
  const acceptValue =
    isMH && summary && summary.acceptanceRate !== null
      ? acceptanceRate(summary.acceptanceRate, 1)
      : null;

  // Effective sample size: meaningful for importance. Prefer raw weights (computed
  // through the tested helper) when given; otherwise read the engine's precomputed
  // value off the summary.
  const essValue = (() => {
    if (isMH) return null;
    if (weights && weights.length > 0) return effectiveSampleSize(weights);
    if (summary && summary.effectiveSampleSize !== null) return summary.effectiveSampleSize;
    return null;
  })();

  // The not-converged flag: only after the run is done, and only for the
  // diagnostic that applies to the running method.
  const notConverged =
    done &&
    ((isMH && acceptValue !== null && (acceptValue < ACCEPT_LOW || acceptValue > ACCEPT_HIGH)) ||
      (!isMH && essValue !== null && essValue < 1.5));

  return (
    <div className={styles.root} data-converged={done && !notConverged ? "true" : "false"}>
      <span className={styles.heading}>diagnostics</span>
      <div className={styles.grid}>
        <div className={styles.stat}>
          <span className={styles.statLabel}>acceptance rate</span>
          <span className={styles.statValue}>
            {acceptValue !== null ? acceptValue.toFixed(3) : "--"}
          </span>
          <span className={styles.statNote}>
            {isMH ? "metropolis-hastings" : "n/a (importance)"}
          </span>
        </div>
        <div className={styles.stat}>
          <span className={styles.statLabel}>effective sample size</span>
          <span className={styles.statValue}>{essValue !== null ? essValue.toFixed(1) : "--"}</span>
          <span className={styles.statNote}>{isMH ? "n/a (metropolis)" : "importance"}</span>
        </div>
      </div>
      {notConverged && (
        <p className={styles.warning}>
          The run reached its length cap without settling. Try the other method or simpler data.
        </p>
      )}
    </div>
  );
}
