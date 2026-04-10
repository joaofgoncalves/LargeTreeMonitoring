

ltm_ts_breaks <- function(spidf_ts) {


  if (!inherits(spidf_ts, "spidf")) {
    stop("Error: 'spidf_ts' must be of class 'spidf'.")
  }

  structure(
    list(
      spidf_ts = spidf_ts,
      algorithms = list()
    ),
    class = "ts_breaks"
  )
}


ltm_add_runs <- function(ts_breaks_obj, ...) {

  if (!inherits(ts_breaks_obj, "ts_breaks")) {
    stop("Error: 'ts_breaks_obj' must be of class 'ts_breaks'.")
  }

  # safe getters
  get_slot <- function(obj, nm, default = NA) {
    val <- obj[[nm]]
    if (is.null(val)) default else val
  }
  get_date <- function(obj, nm) {
    val <- obj[[nm]]
    if (is.null(val)) as.Date(NA) else as.Date(val)
  }

  run_objects <- list(...)

  for (i in seq_along(run_objects)) {

    run_object <- run_objects[[i]]

    if (!inherits(run_object, "ts_breaks_run")) {
      stop("Error: each 'run_object' must be of class 'ts_breaks_run'.")
    }

    algorithm_name <- get_slot(run_object, "method", NA_character_)

    if (is.null(algorithm_name) || !nzchar(algorithm_name)) {
      stop("Error: each 'run_object' must have a non-empty 'method' field.")
    }

    data_type <- get_slot(run_object, "data_type", NA_character_)
    if (!(data_type %in% VALID_DATA_TYPES)) {
      stop("Error: 'data_type' must be one of: ", paste(VALID_DATA_TYPES, collapse = ", "))
    }

    if (!(algorithm_name %in% names(ts_breaks_obj$algorithms))) {
      ts_breaks_obj$algorithms[[algorithm_name]] <- list()
    }

    new_run <- list(
      # core
      method           = algorithm_name,
      data_type        = data_type,
      has_breaks       = isTRUE(get_slot(run_object, "has_breaks", FALSE)),
      breaks_indices   = get_slot(run_object, "breaks_indices"),
      breaks_dates     = get_date(run_object, "breaks_dates"),
      output_object    = get_slot(run_object, "output_object"),
      spidf_ts         = ts_breaks_obj$spidf_ts,
      season_adj       = isTRUE(get_slot(run_object, "season_adj", FALSE)),
      season_used      = isTRUE(get_slot(run_object, "season_used", FALSE)),
      call             = get_slot(run_object, "call"),

      # Long-term aggregation validator
      has_valid_breaks_lt = isTRUE(get_slot(run_object, "has_valid_breaks_lt", FALSE)),
      break_magn       = get_slot(run_object, "break_magn"),

      # Short-term aggregation validator
      has_valid_breaks_st = isTRUE(get_slot(run_object, "has_valid_breaks_st", FALSE)),
      st_change_pct       = get_slot(run_object, "st_change_pct"),
      st_pre              = get_slot(run_object, "st_pre"),
      st_post             = get_slot(run_object, "st_post"),
      st_window_used      = get_slot(run_object, "st_window_used"),

      # Short-term randomized trend validator
      has_valid_breaks_st_trend = isTRUE(get_slot(run_object, "has_valid_breaks_st_trend", FALSE)),
      trend_rand_p_value          = get_slot(run_object, "trend_rand_p_value"),
      trend_slope_ts              = get_slot(run_object, "trend_slope_ts"),
      trend_slope_ts_pct          = get_slot(run_object, "trend_slope_ts_pct"),
      trend_rand_null_mean_pct    = get_slot(run_object, "trend_rand_null_mean_pct"),
      trend_rand_null_sd_pct      = get_slot(run_object, "trend_rand_null_sd_pct"),
      trend_rand_effect_pct       = get_slot(run_object, "trend_rand_effect_pct"),
      trend_rand_B                = get_slot(run_object, "trend_rand_B"),
      trend_rand_len              = get_slot(run_object, "trend_rand_len"),
      trend_window_used           = get_slot(run_object, "trend_window_used"),
      post_prop_below_baseline    = get_slot(run_object, "post_prop_below_baseline"),
      post_avg_deficit_pct        = get_slot(run_object, "post_avg_deficit_pct")
    )

    run_id <- sprintf("run-%02d", length(ts_breaks_obj$algorithms[[algorithm_name]]) + 1L)
    ts_breaks_obj$algorithms[[algorithm_name]][[run_id]] <- new_run
  }

  return(ts_breaks_obj)
}


ltm_get_algorithms <- function(ts_breaks_obj) {
  stopifnot(inherits(ts_breaks_obj, "ts_breaks"))
  names(ts_breaks_obj$algorithms)
}

ltm_get_runs <- function(ts_breaks_obj, algorithm_name) {
  stopifnot(inherits(ts_breaks_obj, "ts_breaks"))
  if (!algorithm_name %in% names(ts_breaks_obj$algorithms)) {
    stop(paste("Algorithm/method", algorithm_name, "does not exist in this object."))
  }
  names(ts_breaks_obj$algorithms[[algorithm_name]])
}

ltm_get_run_details <- function(ts_breaks_obj, algorithm_name, run_id) {
  stopifnot(inherits(ts_breaks_obj, "ts_breaks"))
  if (!algorithm_name %in% names(ts_breaks_obj$algorithms)) {
    stop(paste("Algorithm/method", algorithm_name, "does not exist in this object."))
  }
  if (!run_id %in% names(ts_breaks_obj$algorithms[[algorithm_name]])) {
    stop(paste("Run ID", run_id, "does not exist for algorithm", algorithm_name))
  }
  ts_breaks_obj$algorithms[[algorithm_name]][[run_id]]
}


# Generic print for a ts_breaks container (multiple algorithms and runs)
# Shows long-term aggregation, short-term aggregation, and short-term trend validators.
#' @export
print.ts_breaks <- function(x, digits = 3, max_breaks = 5, ...) {
  stopifnot(is.list(x))

  # helpers
  yn <- function(z) if (isTRUE(z)) "Yes" else if (identical(z, FALSE)) "No" else "NA"
  num <- function(v) {
    if (is.null(v) || length(v) == 0) return("-")
    vv <- suppressWarnings(as.numeric(v))
    vv <- vv[is.finite(vv)]
    if (!length(vv)) "-" else paste(round(vv, digits), collapse = ", ")
  }
  chr <- function(v) {
    if (is.null(v) || !length(v)) return("-")
    paste(as.character(v), collapse = ", ")
  }
  show_head <- function(v, k = max_breaks) {
    if (is.null(v) || !length(v)) return("-")
    v <- as.character(v)
    if (length(v) <= k) return(paste(v, collapse = ", "))
    paste(paste(v[seq_len(k)], collapse = ", "), sprintf("... (+%d more)", length(v) - k))
  }
  has_slot <- function(obj, nm) !is.null(obj[[nm]])

  # header
  cat("Time Series Break Detection Object (ts_breaks)\n")
  cat("====================================================\n")
  total_algs <- if (!is.null(x$algorithms)) length(x$algorithms) else 0L
  cat("Total algorithms:", total_algs, "\n\n")

  if (total_algs == 0L) {
    cat("No algorithms added yet.\n")
    return(invisible(x))
  }

  # iterate algorithms
  for (alg in names(x$algorithms)) {
    runs <- x$algorithms[[alg]]
    cat(sprintf("Algorithm: %s | runs: %d\n", alg, length(runs)))
    cat("----------------------------------------------------\n")

    if (length(runs) == 0) {
      cat("  (no runs)\n\n")
      next
    }

    # iterate runs
    for (run_name in names(runs)) {
      run <- runs[[run_name]]
      lt_fun <- ltm_get_validator_fun_label(run$call, "lt_fun")
      st_fun <- ltm_get_validator_fun_label(run$call, "st_fun")

      # run header
      cat(sprintf("Run: %s\n", run_name))
      cat(sprintf("  method: %s | data_type: %s | breaks_detected: %s\n",
                  ifelse(is.null(run$method), "-", run$method),
                  ifelse(is.null(run$data_type), "-", run$data_type),
                  yn(run$has_breaks)))

      # break summary
      cat("  Break summary:\n")
      if (isTRUE(run$has_breaks)) {
        cat(sprintf("    indices: %s\n", show_head(run$breaks_indices)))
        cat(sprintf("    dates  : %s\n", show_head(run$breaks_dates)))
        cat(sprintf("    change_magnitude_percent: %s\n", num(run$break_magn)))
      } else {
        cat("    no breaks identified\n")
      }

      # validators
      cat("  Validators:\n")

      # Long-term aggregation
      cat(sprintf("    Long-term valid: %s", yn(run$has_valid_breaks_lt)))
      cat(sprintf(" | aggregation_fun: %s", lt_fun))
      if (!is.null(run$break_magn)) cat(sprintf(" | change_percent: %s", num(run$break_magn)))
      cat("\n")

      # Short-term aggregation (if present)
      if (has_slot(run, "has_valid_breaks_st") ||
          has_slot(run, "st_change_pct") ||
          has_slot(run, "st_window_used")) {
        cat(sprintf("    Short-term valid: %s", yn(run$has_valid_breaks_st)))
        cat(sprintf(" | aggregation_fun: %s", st_fun))
        if (has_slot(run, "st_window_used")) cat(sprintf(" | window_n: %s", as.character(run$st_window_used)))
        if (has_slot(run, "st_change_pct"))  cat(sprintf(" | change_percent: %s", num(run$st_change_pct)))
        cat("\n")
      } else {
        cat("    Short-term valid: NA\n")
      }

      # Short-term trend only
      if (has_slot(run, "has_valid_breaks_st_trend")) {
        cat(sprintf("    Short-term trend valid: %s", yn(run$has_valid_breaks_st_trend)))
        if (has_slot(run, "trend_rand_p_value"))     cat(sprintf(" | p_value: %s", num(run$trend_rand_p_value)))
        if (has_slot(run, "trend_slope_ts_pct"))     cat(sprintf(" | slope_percent_per_year: %s", num(run$trend_slope_ts_pct)))
        if (has_slot(run, "trend_window_used"))      cat(sprintf(" | window_n: %s", as.character(run$trend_window_used)))
        cat("\n")
        # brief post-break indicators if available
        if (has_slot(run, "post_prop_below_baseline") || has_slot(run, "post_avg_deficit_pct") ||
            has_slot(run, "trend_rand_null_mean_pct") || has_slot(run, "trend_rand_effect_pct")) {
          cat("      Short-term trend stats:")
          if (has_slot(run, "trend_rand_null_mean_pct"))
            cat(sprintf(" null_mean_percent=%s", num(run$trend_rand_null_mean_pct)))
          if (has_slot(run, "trend_rand_effect_pct"))
            cat(sprintf(" effect_percent=%s", num(run$trend_rand_effect_pct)))
          if (has_slot(run, "post_prop_below_baseline"))
            cat(sprintf(" post_prop_below_baseline=%s", num(run$post_prop_below_baseline)))
          if (has_slot(run, "post_avg_deficit_pct"))
            cat(sprintf(" post_avg_deficit_percent=%s", num(run$post_avg_deficit_pct)))
          cat("\n")
        }
      } else {
        cat("    Short-term trend valid: NA\n")
      }

      cat("\n")
    }
    cat("\n")
  }

  invisible(x)
}


# Convert a ts_breaks container into a flat data.frame (one row per run).
# Assumes each run reports at most one "primary" break.
#' @export
as.data.frame.ts_breaks <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!inherits(x, "ts_breaks")) {
    stop("Error: 'x' must be of class 'ts_breaks'.")
  }

  # Stable schema for downstream use
  col_order <- c(
    "algorithm", "run_id", "method", "data_type",
    "season_adj", "season_used",
    "has_breaks",
    "break_index", "break_date", "break_magn",
    "has_valid_breaks_lt",                         # long-term validator
    "has_valid_breaks_st", "st_window_used", "st_change_pct", "st_pre", "st_post",
    "has_valid_breaks_st_trend", "trend_window_used", "trend_rand_B", "trend_rand_len",
    "trend_rand_p_value", "trend_slope_ts", "trend_slope_ts_pct",
    "trend_rand_null_mean_pct", "trend_rand_null_sd_pct", "trend_rand_effect_pct",
    "post_prop_below_baseline", "post_avg_deficit_pct"
  )

  # empty result with correct classes
  empty_df <- data.frame(
    algorithm = character(0),
    run_id = character(0),
    method = character(0),
    data_type = character(0),
    season_adj = logical(0),
    season_used = logical(0),
    has_breaks = logical(0),
    break_index = integer(0),
    break_date = as.Date(character(0)),
    break_magn = numeric(0),
    has_valid_breaks_lt = logical(0),
    has_valid_breaks_st = logical(0),
    st_window_used = integer(0),
    st_change_pct = numeric(0),
    st_pre = numeric(0),
    st_post = numeric(0),
    has_valid_breaks_st_trend = logical(0),
    trend_window_used = integer(0),
    trend_rand_B = integer(0),
    trend_rand_len = integer(0),
    trend_rand_p_value = numeric(0),
    trend_slope_ts = numeric(0),
    trend_slope_ts_pct = numeric(0),
    trend_rand_null_mean_pct = numeric(0),
    trend_rand_null_sd_pct = numeric(0),
    trend_rand_effect_pct = numeric(0),
    post_prop_below_baseline = numeric(0),
    post_avg_deficit_pct = numeric(0),
    stringsAsFactors = FALSE
  )

  algs <- x$algorithms

  if (is.null(algs) || length(algs) == 0) {
    return(empty_df[, col_order, drop = FALSE])
  }

  rows <- vector("list", 0L)

  for (alg_name in names(algs)) {
    runs <- algs[[alg_name]]
    if (is.null(runs) || length(runs) == 0) next

    for (run_name in names(runs)) {
      run <- runs[[run_name]]

      # simple safe getter
      gs <- function(nm, default = NA) {
        v <- run[[nm]]
        if (is.null(v)) default else v
      }

      has_breaks <- isTRUE(gs("has_breaks", FALSE))
      idx <- if (has_breaks && length(gs("breaks_indices")) > 0) as.integer(gs("breaks_indices")[1]) else NA_integer_
      dte <- if (has_breaks && length(gs("breaks_dates")) > 0) as.Date(gs("breaks_dates")[1]) else as.Date(NA)

      # ensure single numeric magnitude
      magn_raw <- gs("break_magn", NA_real_)
      magn <- suppressWarnings(as.numeric(if (length(magn_raw) > 0) magn_raw[1] else NA_real_))

      row <- data.frame(
        algorithm  = as.character(alg_name),
        run_id     = as.character(run_name),
        method     = as.character(gs("method", NA_character_)),
        data_type  = as.character(gs("data_type", NA_character_)),

        season_adj  = isTRUE(gs("season_adj", FALSE)),
        season_used = isTRUE(gs("season_used", FALSE)),

        has_breaks  = has_breaks,
        break_index = idx,
        break_date  = dte,
        break_magn  = magn,

        # validators
        has_valid_breaks_lt = isTRUE(gs("has_valid_breaks_lt", FALSE)),

        has_valid_breaks_st = isTRUE(gs("has_valid_breaks_st", FALSE)),
        st_window_used      = suppressWarnings(as.integer(gs("st_window_used"))),
        st_change_pct       = suppressWarnings(as.numeric(gs("st_change_pct"))),
        st_pre              = suppressWarnings(as.numeric(gs("st_pre"))),
        st_post             = suppressWarnings(as.numeric(gs("st_post"))),

        has_valid_breaks_st_trend = isTRUE(gs("has_valid_breaks_st_trend", FALSE)),
        trend_window_used           = suppressWarnings(as.integer(gs("trend_window_used"))),
        trend_rand_B                = suppressWarnings(as.integer(gs("trend_rand_B"))),
        trend_rand_len              = suppressWarnings(as.integer(gs("trend_rand_len"))),
        trend_rand_p_value          = suppressWarnings(as.numeric(gs("trend_rand_p_value"))),
        trend_slope_ts              = suppressWarnings(as.numeric(gs("trend_slope_ts"))),
        trend_slope_ts_pct          = suppressWarnings(as.numeric(gs("trend_slope_ts_pct"))),
        trend_rand_null_mean_pct    = suppressWarnings(as.numeric(gs("trend_rand_null_mean_pct"))),
        trend_rand_null_sd_pct      = suppressWarnings(as.numeric(gs("trend_rand_null_sd_pct"))),
        trend_rand_effect_pct       = suppressWarnings(as.numeric(gs("trend_rand_effect_pct"))),
        post_prop_below_baseline    = suppressWarnings(as.numeric(gs("post_prop_below_baseline"))),
        post_avg_deficit_pct        = suppressWarnings(as.numeric(gs("post_avg_deficit_pct"))),

        stringsAsFactors = FALSE
      )

      # enforce schema/order
      missing <- setdiff(col_order, names(row))

      if (length(missing)){
        for (m in missing){
          row[[m]] <- NA
        }
      }
      row <- row[, col_order, drop = FALSE]
      rows[[length(rows) + 1L]] <- row
    }
  }

  if (!length(rows)) {
    return(empty_df[, col_order, drop = FALSE])
  }

  out <- do.call(rbind, rows)

  return(out)
}




