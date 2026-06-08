import Link from "next/link";
import styles from "./not-found.module.css";

export default function NotFound() {
  return (
    <main className={styles.main}>
      <section className={styles.panel}>
        <p className={styles.code}>404 / not found</p>
        <h1 className={styles.title}>This outcome had probability zero.</h1>
        <p className={styles.body}>
          The page you asked for is not in the support of this site. Every route that exists is
          linked from the gallery.
        </p>
        <Link href="/" className={styles.link}>
          Return to the gallery
        </Link>
      </section>
    </main>
  );
}
