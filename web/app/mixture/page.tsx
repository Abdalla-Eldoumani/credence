"use client";

import { useCallback, useState } from "react";
import { Diagnostics } from "@/components/Diagnostics";
import { LivePanel } from "@/components/LivePanel";
import { MixtureVisualizer } from "@/components/MixtureVisualizer";
import type { Method, Summary } from "@/lib/stream";
import styles from "./mixture.module.css";

// The planted cluster means the server fixes (a broad Normal(0,10) prior, three
// equally-weighted clusters, known sigma 0.7). These are marked crisp in --viz-analytic
// so the recovered means visibly land on them; they match the server's planted clusters
// exactly, so the demo and the recovery test agree on the oracle.
const PLANTED_CLUSTERS = [-6.0, 0.0, 5.0];

// The synthetic data points, the same fifteen the recovery test plants: five tightly
// around each planted cluster, each cluster's deviations summing to zero so the sample
// mean is exactly the planted value. Loaded by the "sample" affordance in one click
// (edge case first launch), and POSTed verbatim to /run/mixture so the server recovers
// these clusters.
const SAMPLE_POINTS = [
  -6.5, -5.5, -6.3, -5.7, -6.0, -0.5, 0.5, -0.3, 0.3, 0.0, 4.6, 5.4, 4.7, 5.3, 5.0,
];

export default function MixturePage() {
  // The mixture is run under importance on the collapsed model regardless of method;
  // the control stays for parity with the other demos but importance is the meaningful
  // choice (the recovery and the server both run importance).
  const [method] = useState<Method>("Importance");
  const [points, setPoints] = useState<number[]>(SAMPLE_POINTS);
  // Bumping this token tells the visualizer to (re)start a run with the current points.
  // 0 means no run has been requested yet (empty state).
  const [runToken, setRunToken] = useState(0);

  // The latest summary and run-done flag drive the diagnostics readout, fed from the
  // visualizer's single subscription so the page opens no second EventSource.
  const [summary, setSummary] = useState<Summary | null>(null);
  const [done, setDone] = useState(false);

  const handleRunStart = useCallback(() => {
    setSummary(null);
    setDone(false);
  }, []);

  const handleSummary = useCallback((next: Summary) => {
    setSummary(next);
  }, []);

  const handleRunDone = useCallback(() => {
    setDone(true);
  }, []);

  const loadSample = useCallback(() => {
    setPoints(SAMPLE_POINTS);
  }, []);

  const clearPoints = useCallback(() => {
    setPoints([]);
  }, []);

  const startRun = useCallback(() => {
    setRunToken((token) => token + 1);
  }, []);

  const controls = (
    <>
      <div className={styles.section}>
        <span className={styles.label}>method</span>
        <div className={styles.methodRow}>
          <button
            type="button"
            className={`${styles.methodButton} ${styles.methodButtonActive}`}
            aria-pressed={true}
            disabled
          >
            importance
          </button>
        </div>
        <p className={styles.hint}>
          The mixture runs under importance on the collapsed model: summing the per-point assignment
          out keeps the effective sample size healthy, where the assignment-sampling form
          degenerates.
        </p>
      </div>

      <div className={styles.section}>
        <span className={styles.label}>clusters</span>
        <div className={styles.setup}>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>planted means</span>
            <span className={styles.setupValue}>
              {PLANTED_CLUSTERS.map((c) => c.toFixed(1)).join(", ")}
            </span>
          </div>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>cluster sigma</span>
            <span className={styles.setupValue}>0.70</span>
          </div>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>data points</span>
            <span className={styles.setupValue}>{points.length.toString().padStart(2, "0")}</span>
          </div>
        </div>
        <p className={styles.hint}>
          Three well-separated clusters seed the synthetic data. The posterior over the cluster
          means concentrates onto the planted clusters, marked by the pale ticks, label switching
          handled by sorting each particle&apos;s means.
        </p>
        <div className={styles.dataActions}>
          <button type="button" className={styles.textButton} onClick={loadSample}>
            sample
          </button>
          <button type="button" className={styles.textButton} onClick={clearPoints}>
            clear
          </button>
        </div>
      </div>

      <div className={styles.section}>
        <button type="button" className={styles.start} onClick={startRun}>
          {runToken === 0 ? "start run" : "restart"}
        </button>
        <p className={styles.hint}>
          Streams from the engine on 127.0.0.1:3001. Start it with{" "}
          <code>cabal run credence-server</code>.
        </p>
      </div>
    </>
  );

  return (
    <LivePanel
      title="Mixture"
      subtitle="Cluster synthetic data with a Gaussian mixture, the unknowns the cluster means. The posterior over the means concentrates onto the planted clusters as the run converges, label switching resolved by ordering each particle's means."
      controls={controls}
    >
      <MixtureVisualizer
        method={method}
        points={points}
        plantedClusters={PLANTED_CLUSTERS}
        runToken={runToken}
        onRunStart={handleRunStart}
        onSummary={handleSummary}
        onRunDone={handleRunDone}
      />
      <Diagnostics summary={summary} method={method} done={done} />
      <p className={styles.axisNote}>
        Position axis -10 to 10. ESS is the importance effective sample size; the collapsed mixture
        keeps it healthy where the assignment-sampling form would collapse it to ~1.
      </p>
    </LivePanel>
  );
}
