import type { Metadata } from "next";
import type { ReactNode } from "react";

export const metadata: Metadata = { title: "Tracking" };

export default function TrackingLayout({ children }: { children: ReactNode }) {
  return children;
}
