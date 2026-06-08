import Link from "next/link";
import type { ReactNode } from "react";
import styles from "./LivePanel.module.css";

type LivePanelProps = {
  title: string;
  subtitle: string;
  // The model-specific controls (method choice, start/restart, data editor).
  // The coin route supplies coin controls; 03-03's regression route supplies its
  // own, so the two-column shell stays shared.
  controls: ReactNode;
  // The visualizer mounts in the right column.
  children: ReactNode;
};

// The shared live-inference shell: a two-column layout (controls and data on the
// left, the animated posterior on the right) collapsing to one column below
// 768px, per the UI spec. It owns layout only; pacing and streaming live in the
// visualizer it wraps.
export function LivePanel({ title, subtitle, controls, children }: LivePanelProps) {
  return (
    <main className={styles.panel}>
      <header className={styles.head}>
        <Link href="/" className={styles.back}>
          back to gallery
        </Link>
        <h1 className={styles.title}>{title}</h1>
        <p className={styles.subtitle}>{subtitle}</p>
      </header>
      <div className={styles.layout}>
        <aside className={styles.controls}>{controls}</aside>
        <div className={styles.viz}>{children}</div>
      </div>
    </main>
  );
}
