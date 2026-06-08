"use client";

import { useCallback, useState } from "react";
import { Diagnostics } from "@/components/Diagnostics";
import { LivePanel } from "@/components/LivePanel";
import { TrackingVisualizer } from "@/components/TrackingVisualizer";
import type { Method, Summary } from "@/lib/stream";
import styles from "./tracking.module.css";

// A sample observation sequence for the moving target: a hand-planted wandering walk
// (a few dozen noisy reads) the filter has real work to follow, drifting across the
// axis the way the server's random-walk target does. The exact planted trajectory the
// recovery test judges the filter on is generated server-side under a fixed seed and
// never crosses the wire, so this is a believable stand-in the visualizer marks the
// latest read of; the "sample" affordance loads it in one click for the first-launch
// empty state. It stays inside the cloud's +/-10 window so the target never clips an edge.
const SAMPLE_OBSERVATIONS = [
  0.2, -0.6, 0.1, 1.0, 1.9, 2.6, 3.4, 2.9, 3.8, 4.6, 5.1, 4.7, 5.6, 4.9, 4.1, 3.3, 2.6, 1.8, 1.1,
  0.3, -0.5, -1.4, -2.2, -3.1, -3.8, -4.5, -5.2, -4.7, -3.9, -3.0, -2.2, -1.3, -0.5, 0.4, 1.2, 2.1,
  2.9, 3.6, 4.4, 5.0,
];

// The position axis the readouts span, matching the cloud's domain.
const POS_DOMAIN: [number, number] = [-10, 10];

export default function TrackingPage() {
  const [method, setMethod] = useState<Method>("Importance");
  const [observations, setObservations] = useState<number[]>(SAMPLE_OBSERVATIONS);
  // Bumping this token tells the visualizer to (re)start a run with the current method
  // and observations. 0 means no run has been requested yet (empty state).
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
    setObservations(SAMPLE_OBSERVATIONS);
  }, []);

  const clearObservations = useCallback(() => {
    setObservations([]);
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
            className={`${styles.methodButton} ${
              method === "Importance" ? styles.methodButtonActive : ""
            }`}
            aria-pressed={method === "Importance"}
            onClick={() => setMethod("Importance")}
          >
            importance
          </button>
          <button
            type="button"
            className={`${styles.methodButton} ${method === "MH" ? styles.methodButtonActive : ""}`}
            aria-pressed={method === "MH"}
            onClick={() => setMethod("MH")}
          >
            metropolis
          </button>
        </div>
        <p className={styles.hint}>
          The particle filter drives the cloud regardless of method; the filter is the engine that
          tracks a state-space target.
        </p>
      </div>

      <div className={styles.section}>
        <span className={styles.label}>target</span>
        <div className={styles.setup}>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>walk noise q</span>
            <span className={styles.setupValue}>0.60</span>
          </div>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>read noise r</span>
            <span className={styles.setupValue}>1.00</span>
          </div>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>observations</span>
            <span className={styles.setupValue}>
              {observations.length.toString().padStart(2, "0")}
            </span>
          </div>
        </div>
        <p className={styles.hint}>
          Each step is a noisy read of a hidden target drifting by a random walk. The particle cloud
          follows the target over time, the pale mark the latest read it tracks.
        </p>
        <div className={styles.dataActions}>
          <button type="button" className={styles.textButton} onClick={loadSample}>
            sample
          </button>
          <button type="button" className={styles.textButton} onClick={clearObservations}>
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
      title="Tracking"
      subtitle="Track a moving target from noisy reads with a particle filter. A weighted cloud of plausible positions follows the hidden target over time, the estimate locking onto the latest read as the run walks the trajectory."
      controls={controls}
    >
      <TrackingVisualizer
        method={method}
        observations={observations}
        runToken={runToken}
        onRunStart={handleRunStart}
        onSummary={handleSummary}
        onRunDone={handleRunDone}
      />
      <Diagnostics summary={summary} method={method} done={done} />
      <p className={styles.axisNote}>
        Position axis {POS_DOMAIN[0]} to {POS_DOMAIN[1]}. ESS is the filter's effective sample size,
        dropping and resetting as it resamples.
      </p>
    </LivePanel>
  );
}
