import type { Metadata } from "next";
import type { ReactNode } from "react";

export const metadata: Metadata = { title: "Method Race" };

export default function RaceLayout({ children }: { children: ReactNode }) {
  return children;
}
