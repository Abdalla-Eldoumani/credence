import type { Metadata } from "next";
import type { ReactNode } from "react";

export const metadata: Metadata = { title: "Localization" };

export default function LocalizationLayout({ children }: { children: ReactNode }) {
  return children;
}
