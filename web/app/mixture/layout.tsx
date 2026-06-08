import type { Metadata } from "next";
import type { ReactNode } from "react";

export const metadata: Metadata = { title: "Mixture" };

export default function MixtureLayout({ children }: { children: ReactNode }) {
  return children;
}
