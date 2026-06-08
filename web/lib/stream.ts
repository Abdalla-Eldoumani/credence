// The EventSource client for the engine stream. The TS types mirror the
// Credence.Wire schema exactly: the stream carries tagged StreamEvent objects, and
// the POST body is a RunRequest. The server replays the latest Summary on connect,
// so a reconnecting client recovers state immediately.
//
// Run-id protocol: each POST to /run/* mints a fresh RunId and returns it as
// {"runId":"..."}; start* resolve to that RunId, and subscribeRun(baseUrl, runId)
// drains that one run by opening /stream/<runId>. Concurrent runs are independent,
// so two runs (importance and MH on the same data) stream side by side without
// cancelling each other.

// The inference method. A BARE JSON string on the wire (aeson renders the nullary
// sum constructor as a string), never a tagged object. Do not wrap in {tag:...}.
export type Method = "Importance" | "MH";

// The running summary the engine emits per batch.
export type Summary = {
  batchIndex: number;
  mean: number;
  variance: number;
  // MH accept fraction; null for importance.
  acceptanceRate: number | null;
  // Importance ESS = 1/sum(w_i^2); null for MH.
  effectiveSampleSize: number | null;
};

// A thinned subsample of latent draws (<= 60), paired with its summary by index.
// weights, when present, are the NORMALIZED per-draw importance weights aligned with
// draws (same length, same order), the opacity-by-weight input the cloud reads; it
// is null for MH, whose chain has no per-draw importance weight. Absent on the
// coin/regression demos, which ignore it.
export type Batch = {
  batchIndex: number;
  draws: number[];
  weights?: number[] | null;
};

// The tagged StreamEvent union. EvDone is terminal (the stream loop stops on it).
export type StreamEvent =
  | { tag: "EvSummary"; contents: Summary }
  | { tag: "EvBatch"; contents: Batch }
  | { tag: "EvDone" };

// The coin dataset, a tagged object because it carries fields.
export type CoinDataset = { tag: "Coin"; flips: boolean[] };

// The regression dataset: observed [x, y] points the slope is fit through. The
// wire shape matches the 03-01 server (Regression{points:[[x,y]]}); the latent is
// the slope w, and the posterior band is the lines y = w_i * x for sampled w_i.
export type RegressionDataset = { tag: "Regression"; points: [number, number][] };

// The localization dataset: noisy sensor readings of a hidden source position. The
// wire shape matches the server (Localization{readings:[...]}); the latent is the
// source position theta, and the posterior cloud is the sampled theta_i with their
// importance weights.
export type LocalizationDataset = { tag: "Localization"; readings: number[] };

export type Dataset = CoinDataset | RegressionDataset | LocalizationDataset;

// The POST body that starts a run.
export type RunRequest = {
  method: Method;
  dataset: Dataset;
  samples: number;
};

export type StreamHandlers = {
  onSummary?: (summary: Summary) => void;
  onBatch?: (batch: Batch) => void;
  onDone?: () => void;
  // EventSource auto-reconnects; this fires when the connection drops and the
  // browser is retrying, so the UI can show the reconnecting indicator and keep
  // the current bars (edge case SSE drop).
  onReconnecting?: () => void;
  // Fires when a message cannot be parsed, or on a terminal error.
  onError?: (error: unknown) => void;
};

export type Subscription = {
  close: () => void;
};

function parseEvent(data: string): StreamEvent | null {
  try {
    const parsed = JSON.parse(data) as StreamEvent;
    if (parsed && typeof parsed === "object" && "tag" in parsed) {
      return parsed;
    }
    return null;
  } catch {
    return null;
  }
}

/**
 * Open one run's stream and dispatch parsed events to the handlers. Drains the run
 * named by runId at /stream/<runId>, so two runs subscribed at once stay
 * independent. Returns a subscription with close(). EventSource reconnects on its
 * own; an open after an error surfaces via onReconnecting, and the server resends
 * the latest Summary so the animation resumes without a gap.
 */
export function subscribeRun(
  baseUrl: string,
  runId: string,
  handlers: StreamHandlers,
): Subscription {
  const source = new EventSource(`${baseUrl}/stream/${encodeURIComponent(runId)}`);
  let closed = false;

  source.onmessage = (event: MessageEvent<string>) => {
    const parsed = parseEvent(event.data);
    if (!parsed) {
      handlers.onError?.(new Error("unparseable stream event"));
      return;
    }
    switch (parsed.tag) {
      case "EvSummary":
        handlers.onSummary?.(parsed.contents);
        break;
      case "EvBatch":
        handlers.onBatch?.(parsed.contents);
        break;
      case "EvDone":
        handlers.onDone?.();
        // Terminal: close so EventSource does not reopen on the done frame.
        closed = true;
        source.close();
        break;
    }
  };

  source.onerror = () => {
    if (closed) return;
    // EventSource sets readyState to CONNECTING while it retries; that is a
    // recoverable reconnect. CLOSED means it gave up.
    if (source.readyState === EventSource.CONNECTING) {
      handlers.onReconnecting?.();
    } else {
      handlers.onError?.(new Error("stream connection closed"));
    }
  };

  return {
    close() {
      closed = true;
      source.close();
    },
  };
}

// POST a RunRequest to one of the engine's /run/* routes and return the RunId the
// engine minted for it. The route fixes which dataset the server expects (coin vs
// regression), so the caller picks the path. A non-2xx rejects with the engine's
// reason so the caller can show an error; the 2xx body is {"runId":"..."}.
async function postRun(baseUrl: string, path: string, request: RunRequest): Promise<string> {
  const response = await fetch(`${baseUrl}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(request),
  });
  if (!response.ok) {
    const reason = await response.text().catch(() => "");
    throw new Error(`engine rejected the run (${response.status}): ${reason}`.trim());
  }
  const started = (await response.json()) as { runId?: string };
  if (!started || typeof started.runId !== "string") {
    throw new Error("engine did not return a run id");
  }
  return started.runId;
}

/**
 * Start a coin run on the engine. POSTs the RunRequest to /run/coin and resolves to
 * the RunId to subscribe; rejects on a non-2xx so the caller can show the error
 * state. The run streams independently of any other, keyed by this RunId.
 */
export async function startRun(baseUrl: string, request: RunRequest): Promise<string> {
  return postRun(baseUrl, "/run/coin", request);
}

/**
 * Start a regression run on the engine. POSTs the RunRequest to /run/regression
 * (which the server matches to its Regression dataset) and resolves to the RunId.
 * The stream half is shared with the coin demo: subscribeRun(baseUrl, runId, ...)
 * drains the run named by that RunId.
 */
export async function startRegressionRun(baseUrl: string, request: RunRequest): Promise<string> {
  return postRun(baseUrl, "/run/regression", request);
}

/**
 * Start a localization run on the engine. POSTs the RunRequest to /run/localization
 * (which the server matches to its Localization dataset) and resolves to the RunId.
 * The stream half is shared with the other demos: subscribeRun(baseUrl, runId, ...)
 * drains the run named by that RunId, and its batches carry per-draw weights the
 * cloud scales particle opacity by.
 */
export async function startLocalizationRun(baseUrl: string, request: RunRequest): Promise<string> {
  return postRun(baseUrl, "/run/localization", request);
}
