"use client";

import { useCallback, useRef, useState } from "react";
import { Diagnostics } from "@/components/Diagnostics";
import { LivePanel } from "@/components/LivePanel";
import { RegressionVisualizer, SAMPLE_POINTS } from "@/components/RegressionVisualizer";
import { Traceplot, type TraceplotHandle } from "@/components/Traceplot";
import type { Method, Summary } from "@/lib/stream";
import styles from "./regression.module.css";

export default function RegressionPage() {
  const [method, setMethod] = useState<Method>("Importance");
  const [points, setPoints] = useState<[number, number][]>(SAMPLE_POINTS);
  // Bumping this token tells the visualizer to (re)start a run with the current
  // method and points. 0 means no run has been requested yet (empty state).
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

  const addPoint = useCallback(() => {
    // Append near the origin so it lands on the canvas; the user drags it into
    // place. A small offset per point keeps successive adds from stacking exactly.
    setPoints((prev) => [...prev, [prev.length % 2 === 0 ? 1 : -1, 0]]);
  }, []);

  const clearPoints = useCallback(() => {
    setPoints([]);
  }, []);

  const loadSample = useCallback(() => {
    setPoints(SAMPLE_POINTS);
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
        <span className={styles.label}>data</span>
        <p className={styles.hint}>
          Drag the points on the plot to reshape the data. The slope is fit through the origin, so
          the band is the set of lines y = w x.
        </p>
        <div className={styles.dataActions}>
          <button type="button" className={styles.textButton} onClick={addPoint}>
            + point
          </button>
          <button type="button" className={styles.textButton} onClick={clearPoints}>
            clear
          </button>
          <button type="button" className={styles.textButton} onClick={loadSample}>
            sample
          </button>
        </div>
        <p className={styles.tally}>{points.length} points</p>
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
      title="Regression"
      subtitle="Fit a line through noisy points you can drag. A band of plausible lines collapses onto the trend as the slope posterior converges, with diagnostics and a traceplot tracking the run."
      controls={controls}
    >
      <RegressionVisualizer
        method={method}
        points={points}
        onPointsChange={setPoints}
        runToken={runToken}
        onRunStart={handleRunStart}
        onBatchDraws={handleBatchDraws}
        onSummary={handleSummary}
        onRunDone={handleRunDone}
      />
      <Diagnostics summary={summary} method={method} done={done} />
      <Traceplot ref={traceplotRef} label="slope" />
    </LivePanel>
  );
}
