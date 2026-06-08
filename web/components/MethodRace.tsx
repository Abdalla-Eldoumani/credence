"use client";

import { useCallback, useRef, useState } from "react";
import { CloudVisualizer } from "@/components/CloudVisualizer";
import { Diagnostics } from "@/components/Diagnostics";
import { Traceplot, type TraceplotHandle } from "@/components/Traceplot";
import type { Method, Summary } from "@/lib/stream";
import styles from "./MethodRace.module.css";

// The two methods run side by side on the SAME problem from the SAME data. The
// simultaneity is the whole point: a single raceToken is passed as the runToken to
// BOTH columns, so one bump starts two CONCURRENT runs. The 04-01 server keys each
// run by its own RunId (POST /run/localization returns a RunId, GET /stream/<id>
// drains that one), so the two CloudVisualizers each open their own subscription and
// both clouds animate at once. This is NOT importance-then-MH sequencing; both POST
// and stream together. Each column owns its own traceplot ref, summary, and done
// flag, reset on its own onRunStart, and reuses the cloud + diagnostics + traceplot
// rather than duplicating any stream, clock, or diagnostics logic.

// The position axis the per-column traceplot spans, matching the cloud's domain so
// the trace fills the band as the chain or weighted draws move over the source.
const POS_DOMAIN: [number, number] = [-10, 10];

type RaceColumnProps = {
  method: Method;
  label: string;
  readings: number[];
  trueSource: number;
  // The shared race token, passed straight through as the CloudVisualizer runToken so
  // both columns start on the same bump.
  raceToken: number;
};

// One method's column: the labelled cloud plus its own diagnostics and traceplot, all
// fed from that cloud's single stream subscription (the four optional taps), exactly
// as the localization page wires a single run. Mounting two of these on one shared
// token is what makes the race simultaneous.
function RaceColumn({ method, label, readings, trueSource, raceToken }: RaceColumnProps) {
  const traceplotRef = useRef<TraceplotHandle | null>(null);
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

  return (
    <section className={styles.column} aria-label={`${label} run`}>
      <h2 className={styles.columnTitle}>{label}</h2>
      <CloudVisualizer
        method={method}
        readings={readings}
        trueSource={trueSource}
        runToken={raceToken}
        onRunStart={handleRunStart}
        onBatchDraws={handleBatchDraws}
        onSummary={handleSummary}
        onRunDone={handleRunDone}
      />
      <Diagnostics summary={summary} method={method} done={done} />
      <Traceplot ref={traceplotRef} yDomain={POS_DOMAIN} label="position" />
    </section>
  );
}

type MethodRaceProps = {
  // The noisy sensor readings BOTH methods condition on (same problem, same data).
  readings: number[];
  // The planted source both clouds mark, so the eye watches both collapse onto it.
  trueSource: number;
  // Bumped once by the shared control to start both concurrent runs together.
  raceToken: number;
};

export function MethodRace({ readings, trueSource, raceToken }: MethodRaceProps) {
  return (
    <div className={styles.race}>
      <RaceColumn
        method="Importance"
        label="Importance"
        readings={readings}
        trueSource={trueSource}
        raceToken={raceToken}
      />
      <RaceColumn
        method="MH"
        label="Metropolis-Hastings"
        readings={readings}
        trueSource={trueSource}
        raceToken={raceToken}
      />
    </div>
  );
}
