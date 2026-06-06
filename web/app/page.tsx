export default function Home() {
  return (
    <main
      style={{ maxWidth: "1200px", margin: "0 auto", padding: "var(--space-16) var(--space-6)" }}
    >
      <h1 style={{ fontSize: "var(--text-display-l)", letterSpacing: "-0.02em", lineHeight: 1.1 }}>
        credence
      </h1>
      <p
        style={{ color: "var(--text-secondary)", fontSize: "var(--text-body-l)", maxWidth: "60ch" }}
      >
        An educational probabilistic programming language with a live inference visualizer. This is
        the build skeleton. The visualizer is built in the stage tracked in the task list.
      </p>
    </main>
  );
}
