# Wright-Fisher Simulation — Analysis Functions

## Overview

These functions operate on the output of either `sequential_sim` or `vectorized_sim`, after passing through `extract_convergence`. They are designed to be composable: the extraction utility runs once, and all analysis and plotting functions consume its output.

---

## Data pipeline

```
sequential_sim / vectorized_sim
        │
        ▼
   sim_results        ← list of n_simulations
                         each item: list of vectors (the raw history)
                         sim_results[[i]][[k]] = state of vector at iteration k-1
        │
        ▼
extract_convergence
        │
        ▼
   conv_data          ← list of n_simulations
                         each item: converged_val, fractions, freq_tables, n_iter, winner_at_iter1
```

---

## `extract_convergence(sim_results, tie_is_winner = TRUE)`

**Scope:** one simulation at a time, applied across all simulations via `mclapply`.

**What it does:**

For each simulation, it identifies the value the vector converged to, then tracks that value's relative frequency at every iteration. It also records whether the converged value was already dominant (or tied for dominant) at the very first iteration.

**Output per simulation:**

| Field | Type | Description |
|---|---|---|
| `converged_val` | integer | The value all elements settled on |
| `fractions` | numeric vector | Fraction of the vector occupied by `converged_val` at each iteration |
| `freq_tables` | list of numeric vectors | Full frequency distribution across all present values, at each iteration |
| `n_iter` | integer | Total number of iterations until convergence |
| `winner_at_iter1` | logical | Was `converged_val` already the most frequent value at iteration 1? |

**`tie_is_winner`:** controls how ties are handled at iteration 1. `TRUE` (default) treats a tie as a win for the converged value. `FALSE` requires strict dominance.

---

## `winner_loser(conv_data)`

**Scope:** filters across all simulations.

**What it reveals:**

Isolates simulations where the eventual winner was *not* the most frequent value at iteration 1 — meaning it was outcompeted early but recovered through drift. These are the statistically interesting trajectories: cases where the outcome contradicts the early signal.

Under neutral drift, the probability of fixation of any value is proportional to its initial frequency. The winner-loser subset captures deviations from that expectation — simulations where a minority value fixed despite not being the modal value at the start.

---

## `fraction_by_iteration(conv_data)`

**Scope:** cross-simulation, per iteration.

**What it does:**

Reshapes `conv_data` from simulation-major to iteration-major order. For each iteration `k`, it collects the fraction of `converged_val` present in every simulation that was still running at that point. Simulations that converged before iteration `k` contribute `NA` and are excluded downstream.

**Output:** a named list `iter_1, iter_2, ...` where each element is a numeric vector of length ≤ `n_simulations`.

**What it reveals:**

The distribution of the winner's frequency at a fixed point in time, across the population of simulations. Early iterations should be dispersed (the winner could be anywhere from `1/N` to near `1`); later iterations should mass near `1` as most simulations have converged.

---

## `plot_fraction_histograms(conv_data, vector_size)`

**Scope:** all iterations, all simulations — pooled.

**What it shows:**

A single histogram of every fraction value recorded for the converged value, across every iteration and every simulation. This collapses the time dimension entirely.

**What to expect:**

- A spike near `1/vector_size`: the converged value starts as one of many equally represented values, so it appears at its baseline frequency in early iterations.
- A spike at `1`: every simulation ends at convergence, contributing a fraction of 1 at its final iteration.
- The shape of the distribution between those two poles reflects the dynamics of the drift process — how quickly the winner accumulates frequency, and how much time is spent in intermediate states.

**Reference lines:**

- Red dashed: `1/vector_size` — expected frequency under uniform initialization
- Green dashed: `1` — convergence

---

## `plot_fraction_at_iter(conv_data, vector_size, iteration)`

**Scope:** one specific iteration, across all simulations that reached it.

**What it shows:**

A histogram of the converged value's frequency at exactly iteration `k`, restricted to simulations still running at that point. The title reports the effective sample size `n`.

**What it reveals:**

A cross-sectional snapshot of the drift process at a fixed time. Stepping through iterations produces an animation of the distribution shifting rightward — from dispersed near `1/vector_size` toward massed at `1`. The rate of that shift, and the shape of intermediate distributions, reflects the convergence speed of the process.

Simulations that converged before iteration `k` are silently omitted. This means later iterations reflect a **survivor bias**: only slower-converging simulations remain, which tends to skew the distribution away from `1` relative to a naive expectation.

---

## `plot_winner_loser_trajectory(sim, top_n = 3)`

**Scope:** a single simulation (typically from `winner_loser` output).

**What it shows:**

A line plot of frequency over time for the `top_n` values by peak frequency. The x-axis is iteration, the y-axis is fraction of the vector. The eventual winner's line is solid; all others are dashed. The winner is marked with ★ in the legend.

Values that disappear mid-simulation drop to zero and are no longer plotted beyond that point.

**What it reveals:**

The internal competitive dynamics of a single simulation. For winner-loser cases specifically, this makes visible the moment of reversal — where a minority value overtakes the early leader and goes on to fix. It also shows the typical pattern of neutral drift: values fluctuate, some go extinct, and eventually one reaches fixation by chance rather than by initial advantage.
