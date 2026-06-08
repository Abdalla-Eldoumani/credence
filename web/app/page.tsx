import Link from "next/link";
import styles from "./page.module.css";

type Demo = {
  href: string;
  title: string;
  description: string;
  method: string;
};

const demos: Demo[] = [
  {
    href: "/coin",
    title: "Coin",
    description:
      "Infer a coin's bias from a run of flips. The Beta posterior animates as samples stream, with the exact analytic curve landing on top.",
    method: "beta-bernoulli",
  },
  {
    href: "/regression",
    title: "Regression",
    description:
      "Fit a line to noisy points you can drag. A band of plausible lines collapses onto the trend as the posterior converges.",
    method: "normal-normal",
  },
  {
    href: "/localization",
    title: "Localization",
    description:
      "Locate a hidden source from noisy sensor reads. A posterior cloud of plausible positions concentrates on the true source as the reads accumulate.",
    method: "noisy sensors",
  },
  {
    href: "/race",
    title: "Method race",
    description:
      "Watch importance sampling and Metropolis-Hastings concentrate on one hidden source from the same data, side by side, each cloud converging at once.",
    method: "importance vs metropolis",
  },
  {
    href: "/tracking",
    title: "Tracking",
    description:
      "Track a moving target from noisy reads with a particle filter. A weighted cloud of plausible positions follows the hidden target over time as the estimate locks on.",
    method: "particle filter",
  },
  {
    href: "/mixture",
    title: "Mixture",
    description:
      "Cluster synthetic data with a Gaussian mixture. The posterior over the cluster means concentrates onto the planted clusters as the run converges.",
    method: "gaussian mixture",
  },
];

export default function Home() {
  return (
    <main className={styles.page}>
      <header className={styles.header}>
        <h1 className={styles.title}>credence</h1>
        <p className={styles.lede}>
          A probabilistic programming language with a live inference visualizer. Pick a model and
          watch the posterior converge as the exact answer settles on the approximation.
        </p>
      </header>
      <section className={styles.grid}>
        {demos.map((demo) => (
          <Link key={demo.href} href={demo.href} className={styles.card}>
            <h2 className={styles.cardTitle}>{demo.title}</h2>
            <p className={styles.cardDescription}>{demo.description}</p>
            <span className={styles.cardMeta}>{demo.method}</span>
          </Link>
        ))}
      </section>
    </main>
  );
}
