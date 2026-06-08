import type { Metadata } from "next";
import type { ReactNode } from "react";

export const metadata: Metadata = { title: "Coin" };

export default function CoinLayout({ children }: { children: ReactNode }) {
  return children;
}
