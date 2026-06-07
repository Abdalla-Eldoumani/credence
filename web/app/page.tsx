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
