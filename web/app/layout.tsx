import type { Metadata } from "next";
import { Fraunces, IBM_Plex_Mono, Inter } from "next/font/google";
import type { ReactNode } from "react";
import "./globals.css";

// Fraunces is the display serif; it is a variable font, so requesting the wght
// and opsz axes gives the full weight range plus optical sizing in one face (the
// design system uses 400 and 600 with optical sizing on). next/font forbids
// pinning `weight` alongside `axes`, so the weights are selected in CSS instead.
// Inter is the interface and body sans; IBM Plex Mono is every numeric readout.
// Each font exposes a CSS variable; the design-system names --font-display /
// --font-body / --font-mono are bound to them on <html> below so globals.css
// keeps working while the real fonts replace the system fallbacks.
const fraunces = Fraunces({
  subsets: ["latin"],
  axes: ["opsz"],
  display: "swap",
  variable: "--font-fraunces",
});

const inter = Inter({
  subsets: ["latin"],
  weight: ["400", "500", "600"],
  display: "swap",
  variable: "--font-inter",
});

const plexMono = IBM_Plex_Mono({
  subsets: ["latin"],
  weight: ["400", "500"],
  display: "swap",
  variable: "--font-plex-mono",
});

export const metadata: Metadata = {
  title: {
    default: "credence",
    template: "%s — credence",
  },
  description:
    "An educational probabilistic programming language in Haskell with a live web visualizer that animates inference converging to the analytic answer.",
  applicationName: "credence",
  authors: [{ name: "Abdalla Eldoumani" }],
  keywords: [
    "probabilistic programming",
    "Bayesian inference",
    "importance sampling",
    "Metropolis-Hastings",
    "particle filter",
    "sequential Monte Carlo",
    "Haskell",
    "Next.js",
    "data visualization",
  ],
  openGraph: {
    title: "credence",
    description:
      "Watch a posterior converge live to a model's hidden answer, with the exact analytic curve landing on the approximation.",
    type: "website",
    siteName: "credence",
  },
  twitter: {
    card: "summary",
    title: "credence",
    description:
      "Watch a posterior converge live, on inference proven correct against the analytic answer.",
  },
};

export default function RootLayout({ children }: { children: ReactNode }) {
  return (
    <html
      lang="en"
      className={`${fraunces.variable} ${inter.variable} ${plexMono.variable}`}
      style={{
        // Bind the design-system font tokens to the loaded next/font families,
        // keeping the same fallback stacks declared in globals.css.
        ["--font-display" as string]: "var(--font-fraunces), Georgia, serif",
        ["--font-body" as string]: "var(--font-inter), system-ui, sans-serif",
        ["--font-mono" as string]: "var(--font-plex-mono), monospace",
      }}
    >
      <body>{children}</body>
    </html>
  );
}
