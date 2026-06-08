"use client";

import { useCallback, useState } from "react";
import { LivePanel } from "@/components/LivePanel";
import { MethodRace } from "@/components/MethodRace";
import styles from "./race.module.css";

// The race conditions both methods on the SAME problem from the SAME data, so the
// comparison is honest (the spec). The planted source and a default reading set
// clustered near it are the same symmetric, zero-deviation-sum set the localization
// demo plants, so both clouds' marks sit exactly on the readings' mean.
const TRUE_SOURCE = 3.0;
const SAMPLE_READINGS = [3.6, 2.4, 3.2, 2.8, 3.5, 2.5, 3.7, 2.3, 3.1, 2.9, 3.4, 2.6];

export default function RacePage() {
  const [readings, setReadings] = useState<number[]>(SAMPLE_READINGS);
  // One race token drives BOTH columns: bumping it sets both CloudVisualizers'
  // runToken to the same new value in the same render, so the two concurrent runs
  // start together and animate at once. 0 means no run requested yet.
  const [raceToken, setRaceToken] = useState(0);

  const loadSample = useCallback(() => {
    setReadings(SAMPLE_READINGS);
  }, []);

  const clearReadings = useCallback(() => {
    setReadings([]);
  }, []);

  const startRace = useCallback(() => {
    setRaceToken((token) => token + 1);
  }, []);

  const controls = (
    <>
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
          Both methods see the same noisy reads of one hidden source. Start the race to watch
          importance sampling and Metropolis-Hastings concentrate onto the true source, marked by
          the pale tick, side by side.
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
        <button type="button" className={styles.start} onClick={startRace}>
          {raceToken === 0 ? "start race" : "restart"}
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
      title="Method race"
      subtitle="Importance sampling and Metropolis-Hastings on one localization problem from the same data, side by side. The two clouds animate at once, so the eye compares how each method converges onto the hidden source."
      controls={controls}
    >
      <MethodRace readings={readings} trueSource={TRUE_SOURCE} raceToken={raceToken} />
    </LivePanel>
  );
}
