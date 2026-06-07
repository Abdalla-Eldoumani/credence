// The engine (03-01) binds 127.0.0.1:3001. WSL2 forwards localhost to the host so
// the browser reaches it. Override with NEXT_PUBLIC_ENGINE_URL if the engine moves.
export const engineBaseUrl: string = process.env.NEXT_PUBLIC_ENGINE_URL ?? "http://127.0.0.1:3001";

// The coin model's Beta prior. The analytic posterior is Beta(priorA + heads,
// priorB + tails); both the histogram target and the overlay derive from it, so
// it lives in one place. Beta(2,2) is a weak, symmetric prior centered at 0.5,
// matching the 03-01 server's coin model.
export const coinPriorA = 2;
export const coinPriorB = 2;
