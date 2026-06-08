"use client";

import { useCallback, useRef, useState } from "react";
import { CloudVisualizer } from "@/components/CloudVisualizer";
import { Diagnostics } from "@/components/Diagnostics";
import { LivePanel } from "@/components/LivePanel";
import { Traceplot, type TraceplotHandle } from "@/components/Traceplot";
import type { Method, Summary } from "@/lib/stream";
import styles from "./localization.module.css";

// The planted source the demo conditions on, and a default reading set clustered
// near it (the same symmetric, zero-deviation-sum set the recovery test plants, so
// the cloud's mark sits exactly on the readings' mean). The "sample" affordance
// loads this in one click for the first-launch empty state, and the cloud marks
// TRUE_SOURCE so the eye watches the cloud collapse onto it.
const TRUE_SOURCE = 3.0;
const SAMPLE_READINGS = [3.6, 2.4, 3.2, 2.8, 3.5, 2.5, 3.7, 2.3, 3.1, 2.9, 3.4, 2.6];

// The position axis the traceplot spans, matching the cloud's domain so the trace
// fills the band as the chain or the weighted draws move over the source position.
const POS_DOMAIN: [number, number] = [-10, 10];

export default function LocalizationPage() {
  const [method, setMethod] = useState<Method>("Importance");
  const [readings, setReadings] = useState<number[]>(SAMPLE_READINGS);
  // Bumping this token tells the visualizer to (re)start a run with the current
  // method and readings. 0 means no run has been requested yet (empty state).
  const [runToken, setRunToken] = useState(0);

  // The traceplot is fed from the visualizer's single stream subscription so the
  // page opens no second EventSource: the visualizer taps each batch up to here.
  const traceplotRef = useRef<TraceplotHandle | null>(null);

  // The latest summary and run-done flag drive the diagnostics readout, also fed
  // from the visualizer's single subscription.
  const [summary, setSummary] = useState<Summary | null>(null);
  const [done, setDone] = useState(false);

  const handleRunStart = useCallback(() => {
    traceplotRef.current?.reset();
    setSummary(null);
    setDone(false);
  }, []);

  const handleBatchDraws = useCallback((draws: number[]) => {
    traceplotRef.current?.push(draws);
  }, []);

  const handleSummary = useCallback((next: Summary) => {
    setSummary(next);
  }, []);

  const handleRunDone = useCallback(() => {
    setDone(true);
  }, []);

  const loadSample = useCallback(() => {
    setReadings(SAMPLE_READINGS);
  }, []);

  const clearReadings = useCallback(() => {
    setReadings([]);
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
      </div>

      <div className={styles.section}>
        <span className={styles.label}>source</span>
        <div className={styles.setup}>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>true position</span>
            <span className={styles.setupValue}>{TRUE_SOURCE.toFixed(2)}</span>
          </div>
          <div className={styles.setupRow}>
            <span className={styles.setupLabel}>sensor reads</span>
            <span className={styles.setupValue}>{readings.length.toString().padStart(2, "0")}</span>
          </div>
        </div>
        <p className={styles.hint}>
          Each sensor returns a noisy read of the hidden source position. More reads concentrate the
          posterior cloud onto the true source, marked by the pale tick.
        </p>
        <div className={styles.dataActions}>
          <button type="button" className={styles.textButton} onClick={loadSample}>
            sample
          </button>
          <button type="button" className={styles.textButton} onClick={clearReadings}>
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
      title="Localization"
      subtitle="Locate a hidden source from noisy sensor reads. A posterior cloud of plausible positions concentrates on the true source as the reads accumulate, with diagnostics and a traceplot tracking the run."
      controls={controls}
    >
      <CloudVisualizer
        method={method}
        readings={readings}
        trueSource={TRUE_SOURCE}
        runToken={runToken}
        onRunStart={handleRunStart}
        onBatchDraws={handleBatchDraws}
        onSummary={handleSummary}
        onRunDone={handleRunDone}
      />
      <Diagnostics summary={summary} method={method} done={done} />
      <Traceplot ref={traceplotRef} yDomain={POS_DOMAIN} label="position" />
    </LivePanel>
  );
}
