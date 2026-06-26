# List versions - full details

# Algorithms --------------------------------------------------------------


sequential_sim <- \(n_simulations, vector_size) {
  results <- vector("list", n_simulations)
  
  for (i in seq_len(n_simulations)) {
    v <- seq_len(vector_size)
    history <- list(v)  # store initial state (iter 0)
    
    while (length(unique(v)) > 1) {
      v <- sample(v, vector_size, replace = TRUE)
      history <- c(history, list(v))
    }
    
    results[[i]] <- history
  }
  
  results
}

vectorized_sim <- function(n_simulations, vector_size) {
  N <- vector_size
  V <- matrix(seq_len(N), nrow = n_simulations, ncol = N, byrow = TRUE)
  
  orig_idx   <- seq_len(n_simulations)
  iter       <- 0L
  n_active   <- n_simulations
  row_id     <- rep(seq_len(n_active), times = N)
  
  # Pre-allocate: each slot is a list that will grow one matrix-row per iteration
  raw_iters <- vector("list", n_simulations)
  for (s in seq_len(n_simulations)) raw_iters[[s]] <- list()
  
  # Store initial state (iter 0)
  for (s in seq_len(n_active)) {
    raw_iters[[orig_idx[s]]] <- c(raw_iters[[orig_idx[s]]], list(V[s, ]))
  }
  
  while (n_active > 0) {
    col_id  <- sample.int(N, n_active * N, replace = TRUE)
    lin_idx <- row_id + (col_id - 1L) * n_active
    V       <- V[lin_idx]
    dim(V)  <- c(n_active, N)
    
    # Store this iteration's state for every active simulation
    for (s in seq_len(n_active)) {
      raw_iters[[orig_idx[s]]] <- c(raw_iters[[orig_idx[s]]], list(V[s, ]))
    }
    
    converged <- matrixStats::rowMaxs(V) == matrixStats::rowMins(V)
    
    if (any(converged)) {
      keep       <- !converged
      V          <- V[keep, , drop = FALSE]
      orig_idx   <- orig_idx[keep]
      n_active   <- nrow(V)
      if (n_active > 0) row_id <- rep(seq_len(n_active), times = N)
    }
    
    iter <- iter + 1L
  }
  
  raw_iters
}

# Stats extraction --------------------------------------------------------

extract_convergence <- function(sim_results) {
  lapply(sim_results, function(history) {
    final_vec     <- history[[length(history)]]
    converged_val <- final_vec[[1]]
    
    # For each real iteration (skip iter-0), compute full frequency table
    freq_tables <- lapply(history[-1], function(v) {
      table(v) / length(v)
    })
    
    fractions <- sapply(freq_tables, function(ft) {
      ft[[as.character(converged_val)]]
    })
    
    # At iter 1: was the converged value already the most frequent?
    ft1       <- freq_tables[[1]]
    max_freq  <- max(ft1)
    winner_at_iter1 <- ft1[[as.character(converged_val)]] == max_freq
    
    list(
      converged_val    = converged_val,
      fractions        = fractions,
      freq_tables      = freq_tables,
      n_iter           = length(fractions),
      winner_at_iter1  = winner_at_iter1
    )
  })
}

# Returns a named list: iter_1, iter_2, ... each holding a numeric vector
# of length n_simulations (one fraction per simulation at that iteration).
fraction_by_iteration <- function(conv_data) {
  max_iter <- max(sapply(conv_data, `[[`, "n_iter"))
  
  lapply(seq_len(max_iter), function(k) {
    sapply(conv_data, function(sim) {
      if (k <= sim$n_iter) sim$fractions[[k]] else NA_real_
    })
  }) |> setNames(paste0("iter_", seq_len(max_iter)))
}

# Returns a subset of conv_data: only simulations where at iter 1,
# the converged value was NOT the modal value.
winner_loser <- function(conv_data) {
  Filter(function(sim) !sim$winner_at_iter1, conv_data)
}

# Ploting functions -------------------------------------------------------

plot_fraction_histograms <- function(conv_data, vector_size) {
  all_fractions <- unlist(lapply(conv_data, `[[`, "fractions"))
  
  hist(
    all_fractions,
    breaks = seq(0,1,length=vector_size+2),
    main   = "Fraction of converged value across all iterations",
    xlab   = "Fraction",
    ylab   = "Count",
    col    = "#4C72B0",
    border = "white"
  )
  
  abline(v = 1 / vector_size, col = "tomato",   lty = 2, lwd = 1.5)
  abline(v = 1,               col = "seagreen", lty = 2, lwd = 1.5)
}

plot_winner_loser_trajectory <- function(sim, top_n = 3) {
  converged_val <- sim$converged_val
  
  # collect all values that ever appeared
  all_vals <- unique(unlist(lapply(sim$freq_tables, function(ft) as.integer(names(ft)))))
  
  # build a matrix: rows = values, cols = iterations
  # missing = 0 (value was eliminated)
  n_iter <- length(sim$freq_tables)
  traj <- matrix(0, nrow = length(all_vals), ncol = n_iter,
                 dimnames = list(as.character(all_vals), seq_len(n_iter)))
  
  for (k in seq_len(n_iter)) {
    ft <- sim$freq_tables[[k]]
    traj[names(ft), k] <- as.numeric(ft)
  }
  
  # pick top_n by max frequency reached across all iterations
  top_vals <- names(sort(rowMaxs(traj), decreasing = TRUE)[seq_len(min(top_n, nrow(traj)))])
  traj_top <- traj[top_vals, , drop = FALSE]
  
  # plot
  cols <- setNames(
    palette.colors(length(top_vals), palette = "Okabe-Ito"),
    top_vals
  )
  
  plot(
    NULL,
    xlim = c(1, n_iter), ylim = c(0, 1),
    xlab = "Iteration", ylab = "Fraction",
    main = paste("Trajectory — converged to", converged_val)
  )
  
  for (val in top_vals) {
    lines(seq_len(n_iter), traj_top[val, ],
          col = cols[[val]], lwd = 2,
          lty = if (val == as.character(converged_val)) 1 else 2)
    # label at last non-zero point
    last_k <- max(which(traj_top[val, ] > 0))
    text(last_k, traj_top[val, last_k],
         labels = val, col = cols[[val]], pos = 3, cex = 0.8)
  }
  
  legend("topleft",
         legend = paste0(top_vals, ifelse(top_vals == as.character(converged_val), " ★", "")),
         col    = cols, lwd = 2,
         lty    = ifelse(top_vals == as.character(converged_val), 1, 2),
         bty    = "n", cex = 0.8)
}

plot_fraction_at_iter <- function(conv_data, vector_size, iteration) {
  fractions <- vapply(conv_data, function(sim) {
    if (sim$n_iter >= iteration) sim$fractions[[iteration]] else NA_real_
  }, FUN.VALUE = numeric(1))
  
  fractions <- fractions[!is.na(fractions)]
  
  hist(
    fractions,
    breaks = seq(0, 1, length.out = vector_size + 2),
    main   = paste("Fraction of converged value at iteration", iteration,
                   paste0("(n=", length(fractions), ")")),
    xlab   = "Fraction",
    ylab   = "Count",
    col    = "#4C72B0",
    border = "white"
  )
  
  abline(v = 1 / vector_size, col = "tomato",   lty = 2, lwd = 1.5)
  abline(v = 1,               col = "seagreen", lty = 2, lwd = 1.5)
}

# implementation ----------------------------------------------------------

n_sims <- 50
vsize <- 10

raw_sims <- vectorized_sim(n_simulations = n_sims,vsize) # takes time, not really vectorized
conv <- extract_convergence(raw_sims) # takes time
freq_iter <- fraction_by_iteration(conv)

wl <- winner_loser(conv)
walk(1:length(wl),\(iter) plot_winner_loser_trajectory(wl[[iter]]))

plot_fraction_histograms(conv,vsize)
plot_fraction_at_iter(conv,vsize,6)
