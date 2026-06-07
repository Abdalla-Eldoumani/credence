"use client";

import { useCallback, useState } from "react";
import { LivePanel } from "@/components/LivePanel";
import styles from "./coin.module.css";

// The wire method string the engine expects (03-01): a bare JSON string, not a
// tagged object. Mirrored here so the controls compile before lib/stream lands.
type Method = "Importance" | "MH";

// A representative sample of flips for the one-click first-launch affordance:
// seven heads, four tails, which pulls the posterior visibly off the prior.
const SAMPLE_FLIPS: boolean[] = [
  true,
  true,
  false,
  true,
  true,
  false,
  true,
  true,
  false,
  true,
  false,
];

export default function CoinPage() {
  const [method, setMethod] = useState<Method>("Importance");
  const [flips, setFlips] = useState<boolean[]>(SAMPLE_FLIPS);
  // Bumping this token tells the visualizer to (re)start a run with the current
  // method and flips. 0 means no run has been requested yet (empty state).
  const [runToken, setRunToken] = useState(0);

  const heads = flips.filter(Boolean).length;
  const tails = flips.length - heads;

  const toggleFlip = useCallback((index: number) => {
    setFlips((prev) => prev.map((flip, i) => (i === index ? !flip : flip)));
  }, []);

  const addFlip = useCallback((isHeads: boolean) => {
    setFlips((prev) => [...prev, isHeads]);
  }, []);

  const clearFlips = useCallback(() => {
    setFlips([]);
  }, []);

  const loadSample = useCallback(() => {
    setFlips(SAMPLE_FLIPS);
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
        <span className={styles.label}>flips</span>
        <div className={styles.flips}>
          {flips.map((isHeads, index) => (
            <button
              // Flips are positional and have no stable id; index is the identity.
              // biome-ignore lint/suspicious/noArrayIndexKey: positional toggles
              key={index}
              type="button"
              className={`${styles.flip} ${isHeads ? styles.flipHeads : ""}`}
              aria-label={`flip ${index + 1}: ${isHeads ? "heads" : "tails"}`}
              onClick={() => toggleFlip(index)}
            >
              {isHeads ? "H" : "T"}
            </button>
          ))}
        </div>
        <p className={styles.tally}>
          {heads}H / {tails}T
        </p>
        <div className={styles.flipActions}>
          <button type="button" className={styles.textButton} onClick={() => addFlip(true)}>
            + heads
          </button>
          <button type="button" className={styles.textButton} onClick={() => addFlip(false)}>
            + tails
          </button>
          <button type="button" className={styles.textButton} onClick={clearFlips}>
            clear
          </button>
          <button type="button" className={styles.textButton} onClick={loadSample}>
            sample
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
      title="Coin"
      subtitle="Infer a coin's bias from its flips. The Beta posterior animates as the engine streams, and the exact analytic curve lands on the approximation."
      controls={controls}
    >
      <p className={styles.hint}>The visualizer mounts here.</p>
    </LivePanel>
  );
}
