


# helpers ---------------------------------------------------------------

bound_idx <- function(lo, hi, n) pmax(1, pmin(n, seq.int(lo, hi)))


is_len_ok <- function(n, min_n = 3) is.finite(n) && n >= min_n


pct_change <- function(post, pre) {
  if (!is.finite(pre) || abs(pre) < .Machine$double.eps) return(NA)
  return(((post - pre) / pre) * 100)
}

ltm_normalize_lt_window <- function(lt_window) {
  if (is.null(lt_window)) {
    return(NULL)
  }

  if (length(lt_window) != 1L ||
      is.list(lt_window) ||
      is.function(lt_window) ||
      is.logical(lt_window) ||
      inherits(lt_window, c("Date", "POSIXt"))) {
    stop("`lt_window` must be NULL, 0, or a positive whole number.", call. = FALSE)
  }

  if (is.na(lt_window)) {
    stop("`lt_window` must be NULL, 0, or a positive whole number.", call. = FALSE)
  }

  lt_window_num <- suppressWarnings(as.numeric(lt_window))
  if (!is.finite(lt_window_num) || lt_window_num < 0 || lt_window_num %% 1 != 0) {
    stop("`lt_window` must be NULL, 0, or a positive whole number.", call. = FALSE)
  }

  if (lt_window_num == 0) {
    return(NULL)
  }

  as.integer(lt_window_num)
}

ltm_resolve_deprecated_lt_args <- function(dots,
                                           lt_window,
                                           lt_thresh_change,
                                           call = NULL) {
  dots_names <- names(dots)
  if (is.null(dots_names)) {
    dots_names <- rep("", length(dots))
  }

  call_args <- if (is.null(call)) character(0) else names(as.list(call))[-1]
  has_new_window <- "lt_window" %in% call_args
  has_new_thresh <- "lt_thresh_change" %in% call_args

  if ("tresh_int" %in% dots_names) {
    if (has_new_window) {
      stop("Use only one of `lt_window` and deprecated `tresh_int`.", call. = FALSE)
    }
    lt_window <- dots[["tresh_int"]]
    dots[which(dots_names == "tresh_int")] <- NULL
    warning("`tresh_int` is deprecated; use `lt_window`.", call. = FALSE)
  }

  dots_names <- names(dots)
  if (is.null(dots_names)) {
    dots_names <- rep("", length(dots))
  }

  if ("thresh_change" %in% dots_names) {
    if (has_new_thresh) {
      stop("Use only one of `lt_thresh_change` and deprecated `thresh_change`.", call. = FALSE)
    }
    lt_thresh_change <- dots[["thresh_change"]]
    dots[which(dots_names == "thresh_change")] <- NULL
    warning("`thresh_change` is deprecated; use `lt_thresh_change`.", call. = FALSE)
  }

  list(
    dots = dots,
    lt_window = ltm_normalize_lt_window(lt_window),
    lt_thresh_change = lt_thresh_change
  )
}


# steps per year from the date vector (handles irregular cadence)
ltm_steps_per_year <- function(dts) {
  if (length(dts) < 3) return(NA)
  dd <- as.numeric(median(diff(dts)), units = "days")
  if (!is.finite(dd) || dd <= 0) return(NA)
  365.25 / dd
}

## -------------------------------------------------------------------------------- ##


ltm_get_validator_fun_label <- function(call_obj, arg_name, default = "-") {
  if (is.null(call_obj) || is.null(arg_name) || !nzchar(arg_name)) {
    return(default)
  }

  expr <- tryCatch(call_obj[[arg_name]], error = function(e) NULL)
  if (!is.null(expr)) {
    label <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
    if (nzchar(label)) {
      return(label)
    }
  }

  resolve_call_fun <- function(fun_expr) {
    if (is.null(fun_expr)) {
      return(NULL)
    }

    if (is.name(fun_expr) || is.character(fun_expr)) {
      fn_name <- as.character(fun_expr)[1]
      return(tryCatch(match.fun(fn_name), error = function(e) NULL))
    }

    if (is.call(fun_expr) && length(fun_expr) >= 3) {
      op <- as.character(fun_expr[[1]])[1]
      if (op %in% c("::", ":::")) {
        ns_name <- as.character(fun_expr[[2]])[1]
        fn_name <- as.character(fun_expr[[3]])[1]
        if (requireNamespace(ns_name, quietly = TRUE)) {
          ns_env <- asNamespace(ns_name)
          if (exists(fn_name, envir = ns_env, inherits = FALSE)) {
            return(get(fn_name, envir = ns_env, inherits = FALSE))
          }
        }
      }
    }

    NULL
  }

  fn <- resolve_call_fun(tryCatch(call_obj[[1]], error = function(e) NULL))
  if (!is.null(fn)) {
    default_expr <- tryCatch(formals(fn)[[arg_name]], error = function(e) NULL)
    if (!is.null(default_expr)) {
      label <- paste(deparse(default_expr, width.cutoff = 500L), collapse = " ")
      if (nzchar(label)) {
        return(label)
      }
    }
  }

  default
}


#' Print method for `ts_breaks_run` objects
#'
#' Displays a formatted summary of a breakpoint-detection run stored as a
#' `ts_breaks_run` object, including method metadata, detected breaks, and
#' available long-term, short-term, and short-term trend validation results.
#' The recorded function call is printed at the end.
#'
#' @param x An object of class `ts_breaks_run`.
#' @param digits Integer. Number of decimal places used when printing numeric
#'   values.
#' @param ... Additional arguments passed to print methods.
#'
#' @return
#' Invisibly returns `x`.
#'
#' @export
#'
print.ts_breaks_run <- function(x, digits = 4, ...) {

  stopifnot(inherits(x, "ts_breaks_run"))

  yn <- function(z) {
    if (isTRUE(z)) "Yes" else if (identical(z, FALSE)) "No" else "NA"
  }
  num_fmt <- function(v) {
    if (is.null(v) || length(v) == 0) return("-")
    vv <- suppressWarnings(as.numeric(v))
    vv <- vv[is.finite(vv)]
    if (length(vv) == 0) "-" else paste(round(vv, digits), collapse = ", ")
  }
  chr_fmt <- function(v) {
    if (is.null(v) || length(v) == 0) return("-")
    paste(v, collapse = ", ")
  }
  lt_fun <- ltm_get_validator_fun_label(x$call, "lt_fun")
  st_fun <- ltm_get_validator_fun_label(x$call, "st_fun")

  cat("\n.: Break detection run summary:\n")
  cat("-------------------------------------------\n")
  cat(sprintf("%-26s : %s\n", "Method",      if (!is.null(x$method)) x$method else "-"))
  cat(sprintf("%-26s : %s\n", "Data type",   if (!is.null(x$data_type)) x$data_type else "-"))
  cat(sprintf("%-26s : %s\n", "Season requested", if (!is.null(x$season_adj)) yn(x$season_adj) else "NA"))
  if (!is.null(x$season_used)) {
    cat(sprintf("%-26s : %s\n", "Season used", yn(x$season_used)))
  }
  cat(sprintf("%-26s : %s\n", "Breaks detected", yn(x$has_breaks)))
  cat(sprintf("%-26s : %s\n", "Number of breaks",
              if (!is.null(x$breaks_indices)) length(x$breaks_indices) else "0"))
  cat(sprintf("%-26s : %s\n", "Break indices", chr_fmt(x$breaks_indices)))
  cat(sprintf("%-26s : %s\n", "Break dates",   chr_fmt(x$breaks_dates)))

  # Long-term aggregation validator
  if (isTRUE(x$has_breaks)) {

    cat("\n.: Long-term aggregation check:\n")
    cat("-------------------------------------------\n")
    cat(sprintf("%-26s : %s\n", "Aggregation function", lt_fun))
    cat(sprintf("%-26s : %s\n", "Long-term valid", yn(x$has_valid_breaks_lt)))
    cat(sprintf("%-26s : %s\n", "Change magnitudes (%)", num_fmt(x$break_magn)))
  } else {
    cat("No breaks were identified in this run.\n")
  }

  # Short-term aggregation validator
  if (!is.null(x$has_valid_breaks_st) ||
      !is.null(x$st_change_pct) ||
      !is.null(x$st_window_used) ||
      !is.null(x$st_pre) || !is.null(x$st_post)) {

    cat("\n.: Short-term aggregation check:\n")
    cat("-------------------------------------------\n")
    cat(sprintf("%-26s : %s\n", "Aggregation function", st_fun))
    cat(sprintf("%-26s : %s\n", "Short-term valid", yn(x$has_valid_breaks_st)))
    cat(sprintf("%-26s : %s\n", "Short-term window (n)",
                if (!is.null(x$st_window_used)) as.character(x$st_window_used) else "NA"))
    cat(sprintf("%-26s : %s\n", "Short-term change (%)",    num_fmt(x$st_change_pct)))
    if (!is.null(x$st_pre))  cat(sprintf("%-26s : %s\n", "ST pre value",  num_fmt(x$st_pre)))
    if (!is.null(x$st_post)) cat(sprintf("%-26s : %s\n", "ST post value", num_fmt(x$st_post)))
  }

  # Short-term trend validator
  if (!is.null(x$has_valid_breaks_st_trend) ||
      !is.null(x$trend_slope_ts_pct) ||
      !is.null(x$trend_rand_p_value) ||
      !is.null(x$trend_window_used) ||
      !is.null(x$post_prop_below_baseline) ||
      !is.null(x$post_avg_deficit_pct)) {

    cat("\n.: Short-term trend check:\n")
    cat("-------------------------------------------\n")
    cat(sprintf("%-26s : %s\n", "Short-term trend valid", yn(x$has_valid_breaks_st_trend)))
    cat(sprintf("%-26s : %s\n", "Trend window (n)",

    if (!is.null(x$trend_window_used)) as.character(x$trend_window_used) else "NA"))
    if (!is.null(x$trend_slope_ts_pct))
      cat(sprintf("%-26s : %s\n", "Centered slope (%/yr)", num_fmt(x$trend_slope_ts_pct)))
    if (!is.null(x$trend_slope_ts))
      cat(sprintf("%-26s : %s\n", "Centered slope (abs)",  num_fmt(x$trend_slope_ts)))
    if (!is.null(x$trend_rand_p_value))
      cat(sprintf("%-26s : %s\n", "p-value vs pre null",   num_fmt(x$trend_rand_p_value)))
    if (!is.null(x$trend_rand_null_mean_pct))
      cat(sprintf("%-26s : %s\n", "Null mean (%/yr)",      num_fmt(x$trend_rand_null_mean_pct)))
    if (!is.null(x$trend_rand_null_sd_pct))
      cat(sprintf("%-26s : %s\n", "Null sd (%/yr)",        num_fmt(x$trend_rand_null_sd_pct)))
    if (!is.null(x$trend_rand_effect_pct))
      cat(sprintf("%-26s : %s\n", "Effect vs null (%delta/yr)", num_fmt(x$trend_rand_effect_pct)))
    if (!is.null(x$trend_rand_B))
      cat(sprintf("%-26s : %s\n", "Null draws (B)",        num_fmt(x$trend_rand_B)))
    if (!is.null(x$trend_rand_len))
      cat(sprintf("%-26s : %s\n", "Centered len (n)",      num_fmt(x$trend_rand_len)))
    if (!is.null(x$post_prop_below_baseline))
      cat(sprintf("%-26s : %s\n", "Post below baseline (prop)", num_fmt(x$post_prop_below_baseline)))
    if (!is.null(x$post_avg_deficit_pct))
      cat(sprintf("%-26s : %s\n", "Post avg deficit (%)",       num_fmt(x$post_avg_deficit_pct)))
  }

  cat("\nFunction call:\n")
  if (!is.null(x$call)) print(x$call) else cat("<call not recorded>\n")

  invisible(x)
}

## -------------------------------------------------------------------------------- ##


# Convert slope (per-step) to %/yr given a baseline level
ltm_pct_slope <- function(slope_per_step, baseline, steps_per_year) {
  if (!is.finite(slope_per_step) || !is.finite(baseline) ||
      !is.finite(steps_per_year) || steps_per_year <= 0 ||
      abs(baseline) < .Machine$double.eps) return(NA)
  (slope_per_step * steps_per_year) / baseline * 100
}

#' Validate a breakpoint using a randomized short-term trend test
#'
#' Evaluates whether a detected breakpoint is supported by a short-term negative
#' trend around the break. The function computes the Theil-Sen slope on a
#' centered window around `brk` and compares that slope against a null
#' distribution built from random contiguous pre-break windows of the same
#' length. Slopes are expressed in percent per year relative to a robust
#' pre-break baseline. The decision rule can also accept breaks showing a
#' sustained post-break depression, including partial recovery cases.
#'
#' @param ysa Numeric vector. Time-series values used for validation.
#' @param dts Date vector or date-like vector of the same length as `ysa`.
#' @param brk Integer. Break index in `ysa`.
#' @param trend_window Integer. Half-window size used on each side of the break.
#'   The centered validation window excludes the break index itself.
#' @param thresh_date Date or date-like value. The validation window must extend
#'   to at least this date for the break to be considered valid.
#' @param trend_require_lower_level Logical. If `TRUE`, the mean of the
#'   post-break half-window must be lower than the mean of the pre-break
#'   half-window.
#' @param B Integer. Number of random pre-break windows sampled to build the
#'   null distribution.
#' @param seed Optional integer seed for reproducible null sampling.
#' @param post_pct_thresh Numeric. Threshold for the centered slope expressed in
#'   percent per year. Slopes less than or equal to this value support
#'   validation.
#' @param alpha Numeric. One-sided p-value threshold used to compare the
#'   centered slope against the null distribution.
#' @param deficit_tol Numeric. Relative tolerance used to define whether
#'   post-break values are below the pre-break baseline.
#' @param min_prop_below Numeric. Minimum fraction of post-break observations
#'   that must remain below the baseline threshold for depression-based
#'   validation.
#' @param avg_deficit_thresh Numeric. Maximum allowed mean post-break deficit
#'   (in percent relative to baseline) for depression-based validation.
#'
#' @return
#' A named list with validation results:
#' \describe{
#'   \item{has_valid_breaks_st_trend}{Logical indicating whether the break passes
#'   the short-term trend validator.}
#'   \item{trend_rand_p_value}{One-sided p-value comparing the centered slope to
#'   the randomized pre-break null distribution.}
#'   \item{trend_slope_ts}{Theil-Sen slope for the centered validation window on
#'   the original scale.}
#'   \item{trend_slope_ts_pct}{The centered slope expressed as percent per year
#'   relative to the pre-break baseline.}
#'   \item{trend_rand_null_mean_pct}{Mean of the null distribution of percent
#'   slopes.}
#'   \item{trend_rand_null_sd_pct}{Standard deviation of the null distribution of
#'   percent slopes.}
#'   \item{trend_rand_effect_pct}{Difference between the centered percent slope
#'   and the null mean percent slope.}
#'   \item{trend_rand_B}{Effective number of valid null draws used.}
#'   \item{trend_rand_len}{Length of the centered validation window.}
#'   \item{post_prop_below_baseline}{Proportion of post-break observations below
#'   the baseline threshold.}
#'   \item{post_avg_deficit_pct}{Mean post-break percent deficit relative to the
#'   pre-break baseline.}
#' }
#'
#' @details
#' The centered validation window is constructed from `trend_window`
#' observations before the break and `trend_window` observations after the
#' break, excluding the break index itself. If the series is too short, if the
#' centered window is too small, or if there is insufficient pre-break data to
#' sample equally long null windows, the function returns a result with
#' `has_valid_breaks_st_trend = FALSE` and missing diagnostics where
#' appropriate.
#'
#' A break is considered valid only if the centered window reaches at least
#' `thresh_date`, any requested lower-level condition is satisfied, and at least
#' one of the following holds:
#' \enumerate{
#'   \item the centered percent slope is sufficiently negative;
#'   \item the centered percent slope is significantly more negative than the
#'   randomized pre-break null distribution;
#'   \item the post-break half-window remains sufficiently depressed relative to
#'   the pre-break baseline.
#' }
#'
#' @keywords internal
#'
ltm_trend_validator_randomized <- function(ysa, dts, brk,
                                           trend_window,          # half-window on each side
                                           thresh_date,
                                           trend_require_lower_level = TRUE,
                                           # null sampling
                                           B = 99,               # number of random pre-break windows
                                           seed = NULL,
                                           # %/yr thresholds (tune via options or per-call)
                                           post_pct_thresh = -10,  # accept if centered %-slope <= this
                                           alpha = 0.1,          # one-sided p-value threshold
                                           # depression requirements (handles partial recovery)
                                           deficit_tol = 0.05,    # "below baseline" if < baseline*(1-0.05)
                                           min_prop_below = 0.6,  # fraction of post window below threshold
                                           avg_deficit_thresh = -2.5 # mean % deficit over post window
){

  n <- length(ysa)

  if (!is_len_ok(n, 10) || !is_len_ok(trend_window, 3)) {

    return(list(
      has_valid_breaks_st_trend = FALSE,
      trend_rand_p_value = NA,
      trend_slope_ts = NA,
      trend_slope_ts_pct = NA,
      trend_rand_null_mean_pct = NA,
      trend_rand_null_sd_pct = NA,
      trend_rand_effect_pct = NA,
      trend_rand_B = 0,
      trend_rand_len = NA,
      post_prop_below_baseline = NA,
      post_avg_deficit_pct = NA
    ))
  }

  # Build centered window indices around break (exclude the break index itself)
  pre_rng  <- bound_idx(brk - trend_window, brk - 1, n)
  post_rng <- bound_idx(brk + 1,            brk + trend_window, n)
  ts_idx   <- c(pre_rng, post_rng)
  ts_len   <- length(ts_idx)

  if (!is_len_ok(ts_len, 6)) {
    return(list(
      has_valid_breaks_st_trend = FALSE,
      trend_rand_p_value = NA,
      trend_slope_ts = NA,
      trend_slope_ts_pct = NA,
      trend_rand_null_mean_pct = NA,
      trend_rand_null_sd_pct = NA,
      trend_rand_effect_pct = NA,
      trend_rand_B = 0,
      trend_rand_len = ts_len,
      post_prop_below_baseline = NA,
      post_avg_deficit_pct = NA
    ))
  }

  y_ts <- ysa[ts_idx]
  x_ts <- seq_along(y_ts)

  # Robust slope of centered window
  ts_fit <- robslopes::TheilSen(x = x_ts, y = y_ts, verbose = FALSE)
  slope_ts <- ts_fit$slope

  # Pre-break data for null sampling
  pre_all_idx <- seq_len(brk - 1)
  if (length(pre_all_idx) < ts_len) {
    # Not enough pre data to sample a contiguous window of this length
    return(list(
      has_valid_breaks_st_trend = FALSE,
      trend_rand_p_value = NA,
      trend_slope_ts = slope_ts,
      trend_slope_ts_pct = NA,
      trend_rand_null_mean_pct = NA,
      trend_rand_null_sd_pct = NA,
      trend_rand_effect_pct = NA,
      trend_rand_B = 0,
      trend_rand_len = ts_len,
      post_prop_below_baseline = NA,
      post_avg_deficit_pct = NA
    ))
  }

  # Baseline and annualization for %/year
  steps_per_year <- ltm_steps_per_year(dts)
  baseline <- median(ysa[pre_rng], na.rm = TRUE)

  slope_ts_pct <- ltm_pct_slope(slope_ts, baseline, steps_per_year)

  # Build null distribution: contiguous windows of length ts_len, entirely within pre-break
  if (!is.null(seed)) set.seed(seed)
  start_max <- brk - ts_len

  starts <- sample.int(start_max, size = B, replace = TRUE)
  null_pct <- numeric(0)

  for (s in starts) {

    idx <- s:(s + ts_len - 1)
    y_tr <- ysa[idx]

    if (anyNA(y_tr)){
      next
    }

    tr_fit <- robslopes::TheilSen(x = seq_along(y_tr), y = y_tr, verbose = FALSE)
    slope_tr <- tr_fit$slope
    slope_tr_pct <- ltm_pct_slope(slope_tr, baseline, steps_per_year)

    if (is.finite(slope_tr_pct)){
      null_pct <- c(null_pct, slope_tr_pct)
    }
  }

  B_eff <- length(null_pct)
  pval <- if (B_eff > 0 && is.finite(slope_ts_pct)) {
    # one-sided test for "more negative than pre-break behavior"
    (1 + sum(null_pct <= slope_ts_pct)) / (B_eff + 1)
  } else NA

  # Depression metrics on the *post* half-window (handles partial recovery)
  below_thr <- baseline * (1 - deficit_tol)
  post_prop_below <- mean(ysa[post_rng] < below_thr, na.rm = TRUE)
  post_avg_deficit <- 100 * mean((ysa[post_rng] - baseline) / baseline, na.rm = TRUE)

  # Optional lower-level guard
  lower_level_ok <- TRUE
  if (isTRUE(trend_require_lower_level)) {
    lower_level_ok <- mean(ysa[post_rng], na.rm = TRUE) < mean(ysa[pre_rng], na.rm = TRUE)
  }

  # Decision rule:
  #   1) date ok AND lower-level ok
  #   2) EITHER centered %-slope is clearly negative (<= post_pct_thresh)
  #      OR it is significantly more negative than pre-break null (p <= alpha)
  #   3) OR (when slope not very negative) the post is sufficiently depressed
  st_trend_valid <- isTRUE(max(dts[ts_idx]) >= thresh_date) &&
    isTRUE(lower_level_ok) &&
    (
      (is.finite(slope_ts_pct) && slope_ts_pct <= post_pct_thresh) ||
        (is.finite(pval) && pval <= alpha) ||
        (is.finite(post_prop_below)  && post_prop_below >= min_prop_below &&
           is.finite(post_avg_deficit) && post_avg_deficit <= avg_deficit_thresh)
    )

  list(
    has_valid_breaks_st_trend = st_trend_valid,
    trend_rand_p_value = pval,
    trend_slope_ts = slope_ts,
    trend_slope_ts_pct = slope_ts_pct,
    trend_rand_null_mean_pct = if (B_eff > 0) mean(null_pct) else NA,
    trend_rand_null_sd_pct   = if (B_eff > 1) sd(null_pct) else NA,
    trend_rand_effect_pct    = if (B_eff > 0 && is.finite(slope_ts_pct)) slope_ts_pct - mean(null_pct) else NA,
    trend_rand_B = B_eff,
    trend_rand_len = ts_len,
    post_prop_below_baseline = post_prop_below,
    post_avg_deficit_pct = post_avg_deficit
  )
}


#' Detect breakpoints using energy divisive segmentation
#'
#' Applies the energy divisive algorithm from \pkg{ecp} to a time series stored
#' in an `spidf` object and returns a single selected breakpoint together with
#' long-term, short-term, and optional randomized trend-based validation
#' diagnostics.
#'
#' The selected series can be seasonally adjusted before breakpoint detection.
#' When multiple candidate breakpoints are detected, one breakpoint is selected
#' according to `break_select`.
#'
#' @param spidf An object of class `spidf`.
#' @param ts_name Character. Name of the time-series column to analyze. Must be
#'   one of `VALID_DATA_TYPES`.
#' @param sig_lvl Numeric. Significance level passed to `ecp::e.divisive()`.
#' @param R Integer. Number of permutations passed to `ecp::e.divisive()`.
#' @param k Integer. Additional argument passed to `ecp::e.divisive()`.
#' @param min_size Integer. Minimum cluster size passed to `ecp::e.divisive()`.
#' @param alpha Numeric. Significance level adjustment parameter passed to
#'   `ecp::e.divisive()`.
#' @param season_adj Logical. If `TRUE`, applies seasonal adjustment before
#'   breakpoint detection when the series is long enough.
#' @param s_window Integer. Seasonal window used by `stats::stl()` when
#'   `season_adj = TRUE`.
#' @param thresh_date Date or date-like value. The selected breakpoint must
#'   occur on or after this date to be considered valid.
#' @param lt_window Optional integer. If provided, long-term validation is
#'   computed using symmetric windows of this size around the break. Use `NULL`
#'   or `0` to use all observations before and after the break.
#' @param lt_thresh_change Numeric. Maximum allowed percent change for the
#'   long-term validator. More negative values indicate stronger declines.
#' @param lt_fun Function. Aggregation function used by the long-term validator,
#'   typically `median` or `mean`.
#' @param st_window Optional integer. Half-window size for the short-term
#'   validator. If `NULL`, short-term validation is skipped.
#' @param st_thresh_change Numeric. Maximum allowed percent change for the
#'   short-term validator.
#' @param st_fun Function. Aggregation function used for the short-term
#'   validator.
#' @param trend_window Optional integer. Half-window size for the randomized
#'   short-term trend validator. If `NULL`, trend validation is skipped.
#' @param trend_require_lower_level Logical. If `TRUE`, the post-break mean must
#'   be lower than the pre-break mean in the trend validator.
#' @param trend_rand_B Integer. Number of randomized pre-break windows used to
#'   build the null distribution in the trend validator.
#' @param trend_rand_seed Optional integer seed for reproducible randomized
#'   trend validation.
#' @param trend_post_pct_thresh Numeric. Percent-slope threshold used by the
#'   trend validator.
#' @param trend_alpha Numeric. One-sided p-value threshold used by the trend
#'   validator.
#' @param trend_deficit_tol Numeric. Relative tolerance used to define
#'   below-baseline post-break observations in the trend validator.
#' @param trend_min_prop_below Numeric. Minimum proportion of post-break values
#'   that must remain below baseline for depression-based trend validation.
#' @param trend_avg_deficit_thresh Numeric. Maximum allowed mean post-break
#'   deficit, in percent, for depression-based trend validation.
#' @param break_select Character. Rule used to select a breakpoint when multiple
#'   candidates are detected. One of `"first"`, `"first_after_date"`, or
#'   `"largest_drop"`.
#' @param ... Additional arguments reserved for future use.
#'
#' @return
#' An object of class `ts_breaks_run`. It contains the selected breakpoint, the
#' percent magnitude of the long-term change, flags for detected and validated
#' breaks, the raw `ecp::e.divisive()` output, preprocessing metadata, and
#' diagnostics from the short-term and randomized trend validators.
#'
#' @details
#' If the input series is too short, contains too few observations after
#' preprocessing, or no valid breakpoint candidates are found, the function
#' returns a `ts_breaks_run` object with `has_breaks = FALSE` and validation
#' fields set to `FALSE` or `NA` as appropriate.
#'
#' Missing values are interpolated before seasonal adjustment. Seasonal
#' adjustment is only applied when `season_adj = TRUE` and the series is long
#' enough for `stats::stl()`. Otherwise, the original series is used.
#'
#' The long-term validator compares aggregated values before and after the
#' selected break. The short-term validator compares local pre- and post-break
#' windows when `st_window` is provided. The randomized trend validator calls
#' `ltm_trend_validator_randomized()` when `trend_window` is provided.
#'
#' @export
#'
ltm_ed_detect_breaks <- function(spidf,
                                 ts_name        = "spi",
                                 sig_lvl        = 0.05,
                                 R              = 1000,
                                 k              = 1,
                                 min_size       = 30,
                                 alpha          = 1,
                                 season_adj     = TRUE,
                                 s_window       = 30,
                                 thresh_date,
                                 # --- long-term check ---
                                 lt_window        = NULL,
                                 lt_thresh_change = -10,
                                 lt_fun           = median,
                                 # --- short-term check ---
                                 st_window        = NULL,     # integer; if set, enables short-term validator
                                 st_thresh_change = -10,     # % change threshold for short-term validator
                                 st_fun           = median,   # aggregation function for short-term windows
                                 # --- trend check (randomized validator uses this window) ---
                                 trend_window   = NULL,     # integer; if set, enables trend validator
                                 trend_require_lower_level = TRUE, # require post mean < pre mean
                                 trend_rand_B             = 99,
                                 trend_rand_seed          = 11235,
                                 trend_post_pct_thresh    = -10,
                                 trend_alpha              = 0.05,
                                 trend_deficit_tol        = 0.05,
                                 trend_min_prop_below     = 0.6,
                                 trend_avg_deficit_thresh = -1.5,
                                 # --- break selection rule ---
                                 break_select   = c("first", "first_after_date", "largest_drop"),
                                 ...
){
  lt_args <- ltm_resolve_deprecated_lt_args(
    list(...),
    lt_window = lt_window,
    lt_thresh_change = lt_thresh_change,
    call = match.call(expand.dots = FALSE)
  )
  lt_window <- lt_args$lt_window
  lt_thresh_change <- lt_args$lt_thresh_change

  stopifnot(inherits(spidf, "spidf"))
  if (!(ts_name %in% VALID_DATA_TYPES)) {
    stop("Invalid ts_name! Must be one of: ", paste(VALID_DATA_TYPES, collapse = ", "))
  }

  # data prep -------------------------------------------------------------
  yts <- ltm_spidf_to_ts(spidf, ts_name)
  dts <- ltm_get_dates(spidf, remove_leap = TRUE)
  n   <- length(yts)

  if (!is_len_ok(n, max(10, min_size + 5))) {
    ed <- list(cluster = if (n > 0) rep(1, n) else integer(0), estimates = integer(0))

    out <- list(method="ed",
                data_type = ts_name,
                has_breaks = FALSE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = NA,
                breaks_dates = NA,
                output_object = ed,
                season_adj = season_adj,
                season_used = FALSE,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = ifelse(is.null(st_window), NA, as.integer(st_window)),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = ifelse(is.null(trend_window), NA, as.integer(trend_window)),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  # Handle NA before STL if necessary
  y_pre <- yts
  if (anyNA(y_pre)) {
    y_pre <- forecast::na.interp(y_pre)
  }

  # Seasonal adjustment (graceful fallback if too short for stl)
  if (season_adj && is_len_ok(n, s_window * 2 + 7)) {
    decomp <- stats::stl(y_pre, s.window = s_window)
    ysa <- forecast::seasadj(decomp)
    season_used <- TRUE
  } else {
    ysa <- y_pre
    season_used <- FALSE
  }

  mts <- matrix(ysa, nrow = length(ysa), ncol = 1)

  # e.divisive ------------------------------------------------------------
  ed <- ecp::e.divisive(mts, sig.lvl = sig_lvl, R = R, k = k,
                        min.size = min_size, alpha = alpha)

  has_breaks <- (length(unique(ed$cluster)) > 1) &&
    (length(ed$estimates) >= 2)

  # default outputs (will fill in if break exists)
  default_out <- list(method="ed",
                      data_type = ts_name,
                      has_breaks = FALSE,
                      has_valid_breaks_lt = FALSE,
                      break_magn = NA,
                      breaks_indices = NA,
                      breaks_dates = NA,
                      output_object = ed,
                      season_adj = season_adj,
                      season_used = season_used,

                      # short-term
                      has_valid_breaks_st = FALSE,
                      st_change_pct = NA,
                      st_pre = NA,
                      st_post = NA,
                      st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                      # short-term trend validator
                      has_valid_breaks_st_trend = FALSE,
                      trend_rand_p_value = NA,
                      trend_slope_ts = NA,
                      trend_slope_ts_pct = NA,
                      trend_rand_null_mean_pct = NA,
                      trend_rand_null_sd_pct = NA,
                      trend_rand_effect_pct = NA,
                      trend_rand_B = NA,
                      trend_rand_len = NA,
                      trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                      post_prop_below_baseline = NA,
                      post_avg_deficit_pct = NA,

                      call = match.call())

  if (!has_breaks) {
    class(default_out) <- "ts_breaks_run"
    return(default_out)
  }

  # choose break ----------------------------------------------------------
  break_select <- match.arg(break_select)
  brk_candidates <- ed$estimates[ed$estimates > 1 & ed$estimates < n]
  if (length(brk_candidates) == 0) {
    class(default_out) <- "ts_breaks_run"
    return(default_out)
  }

  pick_break <- function() {
    if (break_select == "first") return(brk_candidates[1])

    if (break_select == "first_after_date") {
      cand_dates <- dts[brk_candidates]
      idx <- which(cand_dates >= thresh_date)[1]
      if (is.na(idx)) return(brk_candidates[1]) else return(brk_candidates[idx])
    }

if (break_select == "largest_drop") {
  changes <- sapply(brk_candidates, function(bi) {
    if (is.null(lt_window)) {
      pre_val  <- suppressWarnings(lt_fun(ysa[1:(bi - 1)], na.rm = TRUE))
      post_val <- suppressWarnings(lt_fun(ysa[(bi + 1):n], na.rm = TRUE))
    } else {
      pre_rng  <- bound_idx(bi - lt_window, bi - 1, n)
      post_rng <- bound_idx(bi + 1, bi + lt_window, n)
      pre_val  <- suppressWarnings(lt_fun(ysa[pre_rng],  na.rm = TRUE))
      post_val <- suppressWarnings(lt_fun(ysa[post_rng], na.rm = TRUE))
    }
    pct_change(post_val, pre_val)
  })
  # guard against NA/Inf
  changes[!is.finite(changes)] <- Inf
  return(brk_candidates[which.min(changes)])
}

brk_candidates[1]
}

  brk <- pick_break()
  break_date <- dts[brk]

  # Long-term validator ----------------------------------------------------
  if (is.null(lt_window)) {
    pre_break  <- suppressWarnings(lt_fun(ysa[1:(brk - 1)], na.rm = TRUE))
    post_break <- suppressWarnings(lt_fun(ysa[(brk + 1):n], na.rm = TRUE))
  } else {
    pre_rng  <- bound_idx(brk - lt_window, brk - 1, n)
    post_rng <- bound_idx(brk + 1, brk + lt_window, n)
    pre_break  <- suppressWarnings(lt_fun(ysa[pre_rng],  na.rm = TRUE))
    post_break <- suppressWarnings(lt_fun(ysa[post_rng], na.rm = TRUE))
  }
  break_magn <- pct_change(post_break, pre_break)

  lt_valid <- isTRUE(break_date >= thresh_date) &&
    isTRUE(is.finite(break_magn) && (break_magn <= lt_thresh_change))

  # --- short-term validator ----------------------------------------------
  st_valid <- FALSE
  st_change_pct <- st_pre <- st_post <- NA
  st_used <- if (is.null(st_window)) NA else as.integer(st_window)

  if (!is.null(st_window) && is_len_ok(st_window, 1)) {
    pre_rng_st  <- bound_idx(brk - st_window, brk - 1, n)
    post_rng_st <- bound_idx(brk + 1,      brk + st_window, n)
    st_pre  <- suppressWarnings(st_fun(ysa[pre_rng_st],  na.rm = TRUE))
    st_post <- suppressWarnings(st_fun(ysa[post_rng_st], na.rm = TRUE))
    st_change_pct <- pct_change(st_post, st_pre)
    st_valid <- isTRUE(break_date >= thresh_date) &&
      isTRUE(is.finite(st_change_pct) && (st_change_pct <= st_thresh_change))
  }

  # --- randomized, recovery-aware trend validator ------------------------
  has_valid_breaks_st_trend <- FALSE
  trend_rand_p_value <- NA
  trend_slope_ts <- NA
  trend_slope_ts_pct <- NA
  trend_rand_null_mean_pct <- NA
  trend_rand_null_sd_pct <- NA
  trend_rand_effect_pct <- NA
  trend_rand_B_out <- NA
  trend_rand_len <- NA
  post_prop_below_baseline <- NA
  post_avg_deficit_pct <- NA
  trend_used <- if (is.null(trend_window)) NA else as.integer(trend_window)

  if (!is.null(trend_window) && is_len_ok(trend_window, 3)) {
    trv <- ltm_trend_validator_randomized(
      ysa = ysa, dts = dts, brk = brk, trend_window = trend_window,
      thresh_date = thresh_date,
      trend_require_lower_level = trend_require_lower_level,
      B                  = trend_rand_B,
      seed               = trend_rand_seed,
      post_pct_thresh    = trend_post_pct_thresh,
      alpha              = trend_alpha,
      deficit_tol        = trend_deficit_tol,
      min_prop_below     = trend_min_prop_below,
      avg_deficit_thresh = trend_avg_deficit_thresh
    )
    has_valid_breaks_st_trend <- trv$has_valid_breaks_st_trend
    trend_rand_p_value          <- trv$trend_rand_p_value
    trend_slope_ts              <- trv$trend_slope_ts
    trend_slope_ts_pct          <- trv$trend_slope_ts_pct
    trend_rand_null_mean_pct    <- trv$trend_rand_null_mean_pct
    trend_rand_null_sd_pct      <- trv$trend_rand_null_sd_pct
    trend_rand_effect_pct       <- trv$trend_rand_effect_pct
    trend_rand_B_out            <- trv$trend_rand_B
    trend_rand_len              <- trv$trend_rand_len
    post_prop_below_baseline    <- trv$post_prop_below_baseline
    post_avg_deficit_pct        <- trv$post_avg_deficit_pct
  }

  # assemble output -------------------------------------------------------
  out <- list(method = "ed",
              data_type = ts_name,
              has_breaks = TRUE,
              has_valid_breaks_lt = isTRUE(lt_valid),
              break_magn = break_magn,
              breaks_indices = brk,
              breaks_dates = break_date,
              output_object = ed,
              season_adj = season_adj,
              season_used = season_used,

              # short-term
              has_valid_breaks_st = isTRUE(st_valid),
              st_change_pct = st_change_pct,
              st_pre = st_pre,
              st_post = st_post,
              st_window_used = st_used,

              # short-term trend validator
              has_valid_breaks_st_trend = isTRUE(has_valid_breaks_st_trend),
              trend_rand_p_value = trend_rand_p_value,
              trend_slope_ts = trend_slope_ts,
              trend_slope_ts_pct = trend_slope_ts_pct,
              trend_rand_null_mean_pct = trend_rand_null_mean_pct,
              trend_rand_null_sd_pct = trend_rand_null_sd_pct,
              trend_rand_effect_pct = trend_rand_effect_pct,
              trend_rand_B = trend_rand_B_out,
              trend_rand_len = trend_rand_len,
              trend_window_used = trend_used,
              post_prop_below_baseline = post_prop_below_baseline,
              post_avg_deficit_pct = post_avg_deficit_pct,

              call = match.call())

  class(out) <- "ts_breaks_run"
  return(out)
  }


ltm_cpm_detect_breaks <- function(spidf,
                                  season_adj = TRUE,
                                  ts_name = "spi",
                                  s_window = 30,
                                  cpm_method = "Exponential",
                                  ARL0 = 500,
                                  thresh_date,
                                  # --- long-term check ---
                                  lt_window        = NULL,
                                  lt_thresh_change = -10,
                                  lt_fun           = median,
                                  # --- short-term check ---
                                  st_window        = NULL,
                                  st_thresh_change = -10,
                                  st_fun           = median,
                                  # --- trend check (randomized validator uses this window) ---
                                  trend_window = NULL,
                                  trend_require_lower_level = TRUE,
                                  trend_rand_B             = 99,
                                  trend_rand_seed          = 11235,
                                  trend_post_pct_thresh    = -10,
                                  trend_alpha              = 0.1,
                                  trend_deficit_tol        = 0.05,
                                  trend_min_prop_below     = 0.6,
                                  trend_avg_deficit_thresh = -1.5,
                                  ...){
  lt_args <- ltm_resolve_deprecated_lt_args(
    list(...),
    lt_window = lt_window,
    lt_thresh_change = lt_thresh_change,
    call = match.call(expand.dots = FALSE)
  )
  dots <- lt_args$dots
  lt_window <- lt_args$lt_window
  lt_thresh_change <- lt_args$lt_thresh_change

  stopifnot(inherits(spidf, "spidf"))
  if (!(ts_name %in% VALID_DATA_TYPES)) {
    stop("Invalid ts_name! Must be one of: ", paste(VALID_DATA_TYPES, collapse = ", "))
  }

  # data prep -------------------------------------------------------------
  yts <- ltm_spidf_to_ts(spidf, ts_name)
  dts <- ltm_get_dates(spidf, remove_leap = TRUE)
  n   <- length(yts)

  # too short -> empty skeleton ------------------------------------------
  if (!is_len_ok(n, max(10, s_window * 2 + 7))) {
    cp <- list(changeDetected = FALSE, changePoint = NA)
    out <- list(method = "cpm",
                data_type = ts_name,
                has_breaks = FALSE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = NA,
                breaks_dates = NA,
                output_object = cp,
                season_adj = season_adj,
                season_used = FALSE,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  # NA handling before STL if needed
  y_pre <- yts
  if (anyNA(y_pre)) y_pre <- forecast::na.interp(y_pre)

  # seasonal adjustment ---------------------------------------------------
  if (season_adj && is_len_ok(n, s_window * 2 + 7)) {
    decomp <- stats::stl(y_pre, s.window = s_window)
    ysa <- forecast::seasadj(decomp)
    season_used <- TRUE
  } else {
    ysa <- y_pre
    season_used <- FALSE
  }

  # CPM detection ---------------------------------------------------------
  startup <- ltm_days_between(get_range_start(spidf), thresh_date)

  cp <- do.call(
    cpm::detectChangePoint,
    c(
      list(
        x = ysa,
        cpmType = cpm_method,
        ARL0 = ARL0,
        startup = startup
      ),
      dots
    )
  )

  has_breaks <- isTRUE(cp$changeDetected)

  # default output (if no break) -----------------------------------------
  default_out <- list(method = "cpm",
                      data_type = ts_name,
                      has_breaks = FALSE,
                      has_valid_breaks_lt = FALSE,
                      break_magn = NA,
                      breaks_indices = NA,
                      breaks_dates = NA,
                      output_object = cp,
                      season_adj = season_adj,
                      season_used = season_used,

                      # short-term
                      has_valid_breaks_st = FALSE,
                      st_change_pct = NA,
                      st_pre = NA,
                      st_post = NA,
                      st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                      # short-term trend validator
                      has_valid_breaks_st_trend = FALSE,
                      trend_rand_p_value = NA,
                      trend_slope_ts = NA,
                      trend_slope_ts_pct = NA,
                      trend_rand_null_mean_pct = NA,
                      trend_rand_null_sd_pct = NA,
                      trend_rand_effect_pct = NA,
                      trend_rand_B = NA,
                      trend_rand_len = NA,
                      trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                      post_prop_below_baseline = NA,
                      post_avg_deficit_pct = NA,

                      call = match.call())

  if (!has_breaks) {
    class(default_out) <- "ts_breaks_run"
    return(default_out)
  }

  # Extract break info ----------------------------------------------------
  brk <- cp$changePoint
  if (!is.finite(brk) || brk <= 1 || brk >= n) {
    class(default_out) <- "ts_breaks_run"
    return(default_out)
  }
  break_date <- dts[brk]

  # long-term validator ---------------------------------------------------
  if (is.null(lt_window)) {
    pre_break  <- suppressWarnings(lt_fun(ysa[1:(brk - 1)], na.rm = TRUE))
    post_break <- suppressWarnings(lt_fun(ysa[(brk + 1):n], na.rm = TRUE))
  } else {
    pre_rng  <- bound_idx(brk - lt_window, brk - 1, n)
    post_rng <- bound_idx(brk + 1,        brk + lt_window, n)
    pre_break  <- suppressWarnings(lt_fun(ysa[pre_rng],  na.rm = TRUE))
    post_break <- suppressWarnings(lt_fun(ysa[post_rng], na.rm = TRUE))
  }
  break_magn <- pct_change(post_break, pre_break)

  lt_valid <- isTRUE(break_date >= thresh_date) &&
    isTRUE(is.finite(break_magn) && (break_magn <= lt_thresh_change))

  # short-term validator --------------------------------------------------
  st_valid <- FALSE
  st_change_pct <- st_pre <- st_post <- NA
  st_used <- if (is.null(st_window)) NA else as.integer(st_window)

  if (!is.null(st_window) && is_len_ok(st_window, 1)) {
    pre_rng_st  <- bound_idx(brk - st_window, brk - 1, n)
    post_rng_st <- bound_idx(brk + 1,        brk + st_window, n)
    st_pre  <- suppressWarnings(st_fun(ysa[pre_rng_st],  na.rm = TRUE))
    st_post <- suppressWarnings(st_fun(ysa[post_rng_st], na.rm = TRUE))
    st_change_pct <- pct_change(st_post, st_pre)
    st_valid <- isTRUE(break_date >= thresh_date) &&
      isTRUE(is.finite(st_change_pct) && (st_change_pct <= st_thresh_change))
  }

  # randomized, recovery-aware trend validator ---------------------------
  has_valid_breaks_st_trend <- FALSE
  trend_rand_p_value <- NA
  trend_slope_ts <- NA
  trend_slope_ts_pct <- NA
  trend_rand_null_mean_pct <- NA
  trend_rand_null_sd_pct <- NA
  trend_rand_effect_pct <- NA
  trend_rand_B_out <- NA
  trend_rand_len <- NA
  post_prop_below_baseline <- NA
  post_avg_deficit_pct <- NA
  trend_used <- if (is.null(trend_window)) NA else as.integer(trend_window)

  if (!is.null(trend_window) && is_len_ok(trend_window, 3)) {
    trv <- ltm_trend_validator_randomized(
      ysa = ysa, dts = dts, brk = brk, trend_window = trend_window,
      thresh_date = thresh_date,
      trend_require_lower_level = trend_require_lower_level,
      B                  = trend_rand_B,
      seed               = trend_rand_seed,
      post_pct_thresh    = trend_post_pct_thresh,
      alpha              = trend_alpha,
      deficit_tol        = trend_deficit_tol,
      min_prop_below     = trend_min_prop_below,
      avg_deficit_thresh = trend_avg_deficit_thresh
    )
    has_valid_breaks_st_trend <- trv$has_valid_breaks_st_trend
    trend_rand_p_value          <- trv$trend_rand_p_value
    trend_slope_ts              <- trv$trend_slope_ts
    trend_slope_ts_pct          <- trv$trend_slope_ts_pct
    trend_rand_null_mean_pct    <- trv$trend_rand_null_mean_pct
    trend_rand_null_sd_pct      <- trv$trend_rand_null_sd_pct
    trend_rand_effect_pct       <- trv$trend_rand_effect_pct
    trend_rand_B_out            <- trv$trend_rand_B
    trend_rand_len              <- trv$trend_rand_len
    post_prop_below_baseline    <- trv$post_prop_below_baseline
    post_avg_deficit_pct        <- trv$post_avg_deficit_pct
  }

  # assemble output -------------------------------------------------------
  out <- list(method = "cpm",
              data_type = ts_name,
              has_breaks = TRUE,
              has_valid_breaks_lt = isTRUE(lt_valid),
              break_magn = break_magn,
              breaks_indices = brk,
              breaks_dates = break_date,
              output_object = cp,
              season_adj = season_adj,
              season_used = season_used,

              # short-term
              has_valid_breaks_st = isTRUE(st_valid),
              st_change_pct = st_change_pct,
              st_pre = st_pre,
              st_post = st_post,
              st_window_used = st_used,

              # short-term trend validator
              has_valid_breaks_st_trend = isTRUE(has_valid_breaks_st_trend),
              trend_rand_p_value = trend_rand_p_value,
              trend_slope_ts = trend_slope_ts,
              trend_slope_ts_pct = trend_slope_ts_pct,
              trend_rand_null_mean_pct = trend_rand_null_mean_pct,
              trend_rand_null_sd_pct = trend_rand_null_sd_pct,
              trend_rand_effect_pct = trend_rand_effect_pct,
              trend_rand_B = trend_rand_B_out,
              trend_rand_len = trend_rand_len,
              trend_window_used = trend_used,
              post_prop_below_baseline = post_prop_below_baseline,
              post_avg_deficit_pct = post_avg_deficit_pct,

              call = match.call())

  class(out) <- "ts_breaks_run"
  return(out)
}

#
#
#
# TODO: Check if seasonal adjustment should be used for bfast since it handles stl internally

ltm_bfast01_detect_breaks <- function(
    spidf,
    ts_name       = "spi",
    formula       = response ~ harmon + trend,
    s_window      = 30,
    test          = "OLS-MOSUM",
    level         = 0.05,
    aggregate     = all,
    trim          = NULL,
    bandwidth     = 0.15,
    functional    = "max",
    order         = 3,
    thresh_date,
    # --- long-term check ---
    lt_window        = NULL,
    lt_thresh_change = -10,
    lt_fun           = median,
    # --- short-term check ---
    st_window            = NULL,
    st_thresh_change     = -5,
    st_fun               = median,
    # --- trend check (randomized validator) ---
    trend_window               = NULL,   # integer; if set, enables trend validator
    trend_require_lower_level  = TRUE,
    trend_rand_B               = 99,
    trend_rand_seed            = 11235,
    trend_post_pct_thresh      = -10,
    trend_alpha                = 0.1,
    trend_deficit_tol          = 0.05,
    trend_min_prop_below       = 0.6,
    trend_avg_deficit_thresh   = -1.5,
    ...
) {
  lt_args <- ltm_resolve_deprecated_lt_args(
    list(...),
    lt_window = lt_window,
    lt_thresh_change = lt_thresh_change,
    call = match.call(expand.dots = FALSE)
  )
  dots <- lt_args$dots
  lt_window <- lt_args$lt_window
  lt_thresh_change <- lt_args$lt_thresh_change

  stopifnot(inherits(spidf, "spidf"))
  if (!(ts_name %in% VALID_DATA_TYPES)) {
    stop("Invalid ts_name: ", ts_name)
  }

  # data ---------------------------------------------------------------
  yts <- ltm_spidf_to_ts(spidf, ts_name)
  dts <- ltm_get_dates(spidf, remove_leap = TRUE)
  n   <- length(yts)

  # run bfast01 (detection on raw yts, as before) ----------------------
  bf01 <- do.call(
    bfast::bfast01,
    c(
      list(
        data = yts,
        formula = formula,
        test = test,
        level = level,
        aggregate = aggregate,
        trim = trim,
        bandwidth = bandwidth,
        functional = functional,
        order = order
      ),
      dots
    )
  )

  # Extract break index
  brk_raw <- bf01$breakpoints
  if (length(brk_raw) == 0 || is.null(brk_raw) || all(is.na(brk_raw))) {
    out <- list(method           = "bfast01",
                data_type        = ts_name,
                has_breaks       = FALSE,
                has_valid_breaks_lt = FALSE,
                break_magn       = NA,
                breaks_indices   = NA,
                breaks_dates     = NA,
                output_object    = bf01,
                season_adj       = FALSE,
                season_used      = FALSE,

                # short-term
                has_valid_breaks_st   = FALSE,
                st_change_pct         = NA,
                st_pre                = NA,
                st_post               = NA,
                st_window_used        = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  brk <- as.integer(brk_raw[1])
  if (!is.finite(brk) || brk < 2 || brk >= n) {
    out <- list(method           = "bfast01",
                data_type        = ts_name,
                has_breaks       = TRUE,
                has_valid_breaks_lt = FALSE,
                break_magn       = NA,
                breaks_indices   = brk,
                breaks_dates     = NA,
                output_object    = bf01,
                season_adj       = FALSE,
                season_used      = FALSE,

                # short-term
                has_valid_breaks_st   = FALSE,
                st_change_pct         = NA,
                st_pre                = NA,
                st_post               = NA,
                st_window_used        = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  break_date <- dts[brk]

  # Season-adjust fitted values for magnitude & validators --------------
  # (same approach as your original; detection itself isn't season-adjusted)
  fitted_ts <- ltm_copy_ts(yts, values = fitted(bf01))
  y_pre     <- fitted_ts
  if (anyNA(y_pre)) {
    y_pre <- forecast::na.interp(y_pre)
  }

  if (is_len_ok(n, s_window * 2 + 7)) {
    decomp <- stats::stl(y_pre, s.window = s_window)
    ysa    <- forecast::seasadj(decomp)
    season_used <- TRUE
  } else {
    ysa         <- y_pre
    season_used <- FALSE
  }

  # Pre/post windows for long-term validator ----------------------------
  if (is.null(lt_window)) {
    pre  <- ysa[1:(brk - 1)]
    post <- ysa[(brk + 1):n]
  } else {
    pre_rng  <- bound_idx(brk - lt_window, brk - 1, n)
    post_rng <- bound_idx(brk + 1, brk + lt_window, n)
    pre  <- ysa[pre_rng]
    post <- ysa[post_rng]
  }

  # Long-term magnitude & validity (unchanged logic) --------------------
  val_pre  <- suppressWarnings(lt_fun(pre,  na.rm = TRUE))
  val_post <- suppressWarnings(lt_fun(post, na.rm = TRUE))
  break_magn <- pct_change(val_post, val_pre)

  lt_valid <- isTRUE(break_date >= thresh_date) &&
    isTRUE(is.finite(break_magn) && (break_magn <= lt_thresh_change))

  # Short-term validator -------------------------------------------------
  st_valid <- FALSE
  st_change_pct <- st_pre <- st_post <- NA
  st_used <- if (is.null(st_window)) NA else as.integer(st_window)

  if (!is.null(st_window) && is_len_ok(st_window, 1)) {
    pre_rng_st  <- bound_idx(brk - st_window, brk - 1, n)
    post_rng_st <- bound_idx(brk + 1,      brk + st_window, n)
    st_pre  <- suppressWarnings(st_fun(ysa[pre_rng_st],  na.rm = TRUE))
    st_post <- suppressWarnings(st_fun(ysa[post_rng_st], na.rm = TRUE))
    st_change_pct <- pct_change(st_post, st_pre)
    st_valid <- isTRUE(break_date >= thresh_date) &&
      isTRUE(is.finite(st_change_pct) && (st_change_pct <= st_thresh_change))
  }

  # Randomized, recovery-aware trend validator --------------------------
  has_valid_breaks_st_trend <- FALSE
  trend_rand_p_value <- NA
  trend_slope_ts <- NA
  trend_slope_ts_pct <- NA
  trend_rand_null_mean_pct <- NA
  trend_rand_null_sd_pct <- NA
  trend_rand_effect_pct <- NA
  trend_rand_B_out <- NA
  trend_rand_len <- NA
  post_prop_below_baseline <- NA
  post_avg_deficit_pct <- NA
  trend_used <- if (is.null(trend_window)) NA else as.integer(trend_window)

  if (!is.null(trend_window) && is_len_ok(trend_window, 3)) {
    trv <- ltm_trend_validator_randomized(
      ysa = ysa, dts = dts, brk = brk, trend_window = trend_window,
      thresh_date = thresh_date,
      trend_require_lower_level = trend_require_lower_level,
      B                  = trend_rand_B,
      seed               = trend_rand_seed,
      post_pct_thresh    = trend_post_pct_thresh,
      alpha              = trend_alpha,
      deficit_tol        = trend_deficit_tol,
      min_prop_below     = trend_min_prop_below,
      avg_deficit_thresh = trend_avg_deficit_thresh
    )
    has_valid_breaks_st_trend <- trv$has_valid_breaks_st_trend
    trend_rand_p_value          <- trv$trend_rand_p_value
    trend_slope_ts              <- trv$trend_slope_ts
    trend_slope_ts_pct          <- trv$trend_slope_ts_pct
    trend_rand_null_mean_pct    <- trv$trend_rand_null_mean_pct
    trend_rand_null_sd_pct      <- trv$trend_rand_null_sd_pct
    trend_rand_effect_pct       <- trv$trend_rand_effect_pct
    trend_rand_B_out            <- trv$trend_rand_B
    trend_rand_len              <- trv$trend_rand_len
    post_prop_below_baseline    <- trv$post_prop_below_baseline
    post_avg_deficit_pct        <- trv$post_avg_deficit_pct
  }

  # assemble output -------------------------------------------------------
  out <- list(method = "bfast01",
              data_type = ts_name,
              has_breaks = TRUE,
              has_valid_breaks_lt = isTRUE(lt_valid),
              break_magn = break_magn,
              breaks_indices = brk,
              breaks_dates = break_date,
              output_object = bf01,
              season_adj = FALSE,
              season_used = season_used,

              # short-term
              has_valid_breaks_st = isTRUE(st_valid),
              st_change_pct = st_change_pct,
              st_pre = st_pre,
              st_post = st_post,
              st_window_used = st_used,

              # short-term trend validator
              has_valid_breaks_st_trend = isTRUE(has_valid_breaks_st_trend),
              trend_rand_p_value = trend_rand_p_value,
              trend_slope_ts = trend_slope_ts,
              trend_slope_ts_pct = trend_slope_ts_pct,
              trend_rand_null_mean_pct = trend_rand_null_mean_pct,
              trend_rand_null_sd_pct = trend_rand_null_sd_pct,
              trend_rand_effect_pct = trend_rand_effect_pct,
              trend_rand_B = trend_rand_B_out,
              trend_rand_len = trend_rand_len,
              trend_window_used = trend_used,
              post_prop_below_baseline = post_prop_below_baseline,
              post_avg_deficit_pct = post_avg_deficit_pct,

              call = match.call())

  class(out) <- "ts_breaks_run"
  return(out)
}



mcpSummary <- function(object, width = 0.95, digits = 2,
                       prior = FALSE, ...) {
  fit <- object

  if (!mcp::is.mcpfit(fit)) {
    stop("'object' must be an 'mcpfit' object.", call. = FALSE)
  }
  if (!is.numeric(width) || length(width) != 1L || is.na(width) ||
      width < 0 || width > 1) {
    stop("'width' must be a single numeric value between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != as.integer(digits)) {
    stop("'digits' must be a single non-negative integer.", call. = FALSE)
  }
  if (!is.logical(prior) || length(prior) != 1L || is.na(prior)) {
    stop("'prior' must be TRUE or FALSE.", call. = FALSE)
  }

  ignored_args <- list(...)
  if (length(ignored_args) > 0L) {
    arg_names <- names(ignored_args)
    if (is.null(arg_names) || any(!nzchar(arg_names))) {
      arg_names <- rep("<unnamed>", length(ignored_args))
    }
    stop("Unused arguments: ", paste(arg_names, collapse = ", "), call. = FALSE)
  }

  summary_method <- utils::getS3method("summary", "mcpfit")
  summary_result <- utils::capture.output(
    result <- summary_method(
      fit,
      width = width,
      digits = digits,
      prior = prior
    )
  )
  invisible(summary_result)
  result
}

diagnose_uncertainty <- function(df,
                                 w_width = 1,
                                 w_rhat  = 1,
                                 w_neff  = 1,
                                 cutoff_low = 0.01,
                                 cutoff_med = 0.05) {

  req_cols <- c("mean", "lower", "upper", "Rhat", "n.eff")

  if (!all(req_cols %in% names(df))) {
    stop(paste("Dataframe must contain columns:",
               paste(req_cols, collapse = ", ")))
  }

  width_ratio <- abs(df$upper - df$lower) / abs(df$mean)
  rhat_dev <- abs(df$Rhat - 1)
  inv_neff <- 1 / df$n.eff
  param_uncert <- w_width * width_ratio + w_rhat * rhat_dev + w_neff * inv_neff
  uncertainty_score <- mean(param_uncert, na.rm = TRUE)

  classification <- dplyr::case_when(
    uncertainty_score < cutoff_low ~ "Low",
    uncertainty_score < cutoff_med ~ "Medium",
    TRUE                           ~ "High"
  )

  list(
    uncertainty_score = uncertainty_score,
    classification = classification
  )
}

# --- MCP wrapper with short-term trend validator -------------------------
ltm_mcp_detect_breaks <- function(spidf,
                                  ts_name    = "spi",
                                  season_adj = TRUE,
                                  s_window   = 30,
                                  thresh_date,
                                  sample     = "both",
                                  n_chains   = 3,
                                  n_cores    = 3,
                                  n_adapt    = 500,
                                  n_iter     = 1000,
                                  downsample = NULL,
                                  # --- long-term check ---
                                  lt_window        = NULL,
                                  lt_thresh_change = -10,
                                  lt_fun           = median,
                                  # --- short-term check ---
                                  st_window        = NULL,
                                  st_thresh_change = -10,
                                  st_fun           = median,
                                  # --- trend check (randomized validator) ---
                                  trend_window               = NULL, # integer; if set, enables trend validator
                                  trend_require_lower_level  = TRUE,
                                  trend_rand_B               = 99,
                                  trend_rand_seed            = 11235,
                                  trend_post_pct_thresh      = -10,
                                  trend_alpha                = 0.1,
                                  trend_deficit_tol          = 0.05,
                                  trend_min_prop_below       = 0.6,
                                  trend_avg_deficit_thresh   = -1.5,
                                  ...){
  lt_args <- ltm_resolve_deprecated_lt_args(
    list(...),
    lt_window = lt_window,
    lt_thresh_change = lt_thresh_change,
    call = match.call(expand.dots = FALSE)
  )
  dots <- lt_args$dots
  lt_window <- lt_args$lt_window
  lt_thresh_change <- lt_args$lt_thresh_change

  stopifnot(inherits(spidf, "spidf"))
  if (!(ts_name %in% VALID_DATA_TYPES)) {
    stop("Invalid ts_name! Must be one of: ", paste(VALID_DATA_TYPES, collapse = ", "))
  }

  # Prepare time series ---------------------------------------------------
  yts <- ltm_spidf_to_ts(spidf, ts_name)
  dts <- ltm_get_dates(spidf, remove_leap = TRUE)

  if (!is.null(downsample)) {
    ds_ids <- seq(1, length(yts), by = downsample)
    yts <- ltm_spidf_to_ts(spidf[ds_ids, ], ts_name, freq = downsample)
    dts <- ltm_get_dates(spidf[ds_ids, ], remove_leap = TRUE)
  } else {
    yts <- ltm_spidf_to_ts(spidf, ts_name)
    dts <- ltm_get_dates(spidf, remove_leap = TRUE)
  }

  n <- length(yts)

  # Seasonal adjustment input (with NA handling) --------------------------
  y_pre <- yts
  if (anyNA(y_pre)) y_pre <- forecast::na.interp(y_pre)

  if (season_adj && is_len_ok(n, s_window * 2 + 7)) {
    decomp <- stats::stl(y_pre, s.window = s_window)
    ysa <- forecast::seasadj(decomp)
    season_used <- TRUE
  } else {
    ysa <- y_pre
    season_used <- FALSE
  }

  # Model data for MCP ----------------------------------------------------
  model_df <- data.frame(y = yts, ysa = ysa,
                         jd = lubridate::yday(dts),
                         di = seq_along(ysa))

  # Single change in level (intercepts) over seasonally adjusted series
  model <- list(ysa ~ 1,
                ~ 1)

  # Priors (as before) ----------------------------------------------------
  qts <- round(quantile(ysa, probs = c(0.75, 0.25), na.rm = TRUE), 3)
  std <- round(stats::sd(ysa, na.rm = TRUE), 3)

  my_priors <- list(
    int_1 = paste0("dnorm(", qts[1], ", ", std, ") T(0, 1)"),
    int_2 = paste0("dnorm(", qts[2], ", ", std, ") T(0, int_1)"),
    cp_1  = paste0("dunif(1, ", length(ysa), ")")
  )

  # Fit MCP ---------------------------------------------------------------
  fit_mcp <- do.call(
    mcp::mcp,
    c(
      list(
        model = model,
        data = model_df,
        par_x = "di",
        prior = my_priors,
        adapt = n_adapt,
        sample = sample,
        chains = n_chains,
        cores = n_cores,
        iter = n_iter
      ),
      dots
    )
  )

  # Summaries & break index -----------------------------------------------
  pars <- as.data.frame(mcpSummary(fit_mcp))
  if (is.null(pars) || nrow(pars) < 3) {
    # no reliable summary -> safe empty-ish return
    out <- list(method = "mcp",
                data_type = ts_name,
                has_breaks = FALSE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = NA,
                breaks_dates = NA,
                output_object = fit_mcp,
                season_adj = season_adj,
                season_used = season_used,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                # diagnostics
                uncertainty_diag = NA,
                pars = pars,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  # By convention: cp index is first row, mean in col 2
  brk <- as.integer(round(pars$mean[1]))
  if (!is.finite(brk) || brk < 2 || brk >= n) {
    out <- list(method = "mcp",
                data_type = ts_name,
                has_breaks = TRUE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = brk,
                breaks_dates = NA,
                output_object = fit_mcp,
                season_adj = season_adj,
                season_used = season_used,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                # diagnostics
                uncertainty_diag = diagnose_uncertainty(pars),
                pars = pars,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  break_date <- dts[brk]

  # Long-term aggregation validator ---------------------------------------
  if (is.null(lt_window)) {
    pre_break  <- suppressWarnings(lt_fun(ysa[seq_len(brk - 1)], na.rm = TRUE))
    post_break <- suppressWarnings(lt_fun(ysa[(brk + 1):n], na.rm = TRUE))
  } else {
    pre_rng <- bound_idx(brk - lt_window, brk - 1, n)
    post_rng <- bound_idx(brk + 1, brk + lt_window, n)
    pre_break  <- suppressWarnings(lt_fun(ysa[pre_rng], na.rm = TRUE))
    post_break <- suppressWarnings(lt_fun(ysa[post_rng], na.rm = TRUE))
  }
  break_magn <- pct_change(post_break, pre_break)

  lt_valid <- isTRUE(break_date >= thresh_date) &&
    isTRUE(is.finite(break_magn) && (break_magn <= lt_thresh_change))

  # Short-term validator ---------------------------------------------------
  st_valid <- FALSE
  st_change_pct <- st_pre <- st_post <- NA
  st_used <- if (is.null(st_window)) NA else as.integer(st_window)

  if (!is.null(st_window) && is_len_ok(st_window, 1)) {
    pre_rng_st  <- bound_idx(brk - st_window, brk - 1, n)
    post_rng_st <- bound_idx(brk + 1,      brk + st_window, n)
    st_pre  <- suppressWarnings(st_fun(ysa[pre_rng_st],  na.rm = TRUE))
    st_post <- suppressWarnings(st_fun(ysa[post_rng_st], na.rm = TRUE))
    st_change_pct <- pct_change(st_post, st_pre)
    st_valid <- isTRUE(break_date >= thresh_date) &&
      isTRUE(is.finite(st_change_pct) && (st_change_pct <= st_thresh_change))
  }

  # Randomized, recovery-aware trend validator ----------------------------
  has_valid_breaks_st_trend <- FALSE
  trend_rand_p_value <- NA
  trend_slope_ts <- NA
  trend_slope_ts_pct <- NA
  trend_rand_null_mean_pct <- NA
  trend_rand_null_sd_pct <- NA
  trend_rand_effect_pct <- NA
  trend_rand_B_out <- NA
  trend_rand_len <- NA
  post_prop_below_baseline <- NA
  post_avg_deficit_pct <- NA
  trend_used <- if (is.null(trend_window)) NA else as.integer(trend_window)

  if (!is.null(trend_window) && is_len_ok(trend_window, 3)) {
    trv <- ltm_trend_validator_randomized(
      ysa = ysa, dts = dts, brk = brk, trend_window = trend_window,
      thresh_date = thresh_date,
      trend_require_lower_level = trend_require_lower_level,
      B                  = trend_rand_B,
      seed               = trend_rand_seed,
      post_pct_thresh    = trend_post_pct_thresh,
      alpha              = trend_alpha,
      deficit_tol        = trend_deficit_tol,
      min_prop_below     = trend_min_prop_below,
      avg_deficit_thresh = trend_avg_deficit_thresh
    )
    has_valid_breaks_st_trend <- trv$has_valid_breaks_st_trend
    trend_rand_p_value          <- trv$trend_rand_p_value
    trend_slope_ts              <- trv$trend_slope_ts
    trend_slope_ts_pct          <- trv$trend_slope_ts_pct
    trend_rand_null_mean_pct    <- trv$trend_rand_null_mean_pct
    trend_rand_null_sd_pct      <- trv$trend_rand_null_sd_pct
    trend_rand_effect_pct       <- trv$trend_rand_effect_pct
    trend_rand_B_out            <- trv$trend_rand_B
    trend_rand_len              <- trv$trend_rand_len
    post_prop_below_baseline    <- trv$post_prop_below_baseline
    post_avg_deficit_pct        <- trv$post_avg_deficit_pct
  }

  # assemble output --------------------------------------------------------
  out <- list(method = "mcp",
              data_type = ts_name,
              has_breaks = TRUE,
              has_valid_breaks_lt = isTRUE(lt_valid),
              break_magn = break_magn,
              breaks_indices = brk,
              breaks_dates = break_date,
              output_object = fit_mcp,
              season_adj = season_adj,
              season_used = season_used,

              # short-term
              has_valid_breaks_st = isTRUE(st_valid),
              st_change_pct = st_change_pct,
              st_pre = st_pre,
              st_post = st_post,
              st_window_used = st_used,

              # short-term trend validator
              has_valid_breaks_st_trend = isTRUE(has_valid_breaks_st_trend),
              trend_rand_p_value = trend_rand_p_value,
              trend_slope_ts = trend_slope_ts,
              trend_slope_ts_pct = trend_slope_ts_pct,
              trend_rand_null_mean_pct = trend_rand_null_mean_pct,
              trend_rand_null_sd_pct = trend_rand_null_sd_pct,
              trend_rand_effect_pct = trend_rand_effect_pct,
              trend_rand_B = trend_rand_B_out,
              trend_rand_len = trend_rand_len,
              trend_window_used = trend_used,
              post_prop_below_baseline = post_prop_below_baseline,
              post_avg_deficit_pct = post_avg_deficit_pct,

              # MCP-specific diagnostics
              uncertainty_diag = diagnose_uncertainty(pars),
              pars = pars,

              call = match.call())

  class(out) <- "ts_breaks_run"
  return(out)
}


ltm_strucchange_detect_breaks <- function(
    spidf,
    ts_name    = "spi",
    season_adj = TRUE,
    s_window   = 30,
    h          = 0.15,
    breaks     = 1,
    thresh_date,
    # --- long-term check ---
    lt_window        = NULL,
    lt_thresh_change = -10,
    lt_fun           = median,
    # --- short-term check ---
    st_window        = NULL,
    st_thresh_change = -10,
    st_fun           = median,
    # --- trend check (randomized validator) ---
    trend_window               = NULL,                 # integer; if set, enables trend validator
    trend_require_lower_level  = TRUE,
    trend_rand_B               = 99,
    trend_rand_seed            = 11235,
    trend_post_pct_thresh      = -10,
    trend_alpha                = 0.1,
    trend_deficit_tol          = 0.05,
    trend_min_prop_below       = 0.6,
    trend_avg_deficit_thresh   = -1.5,
    ...
) {
  lt_args <- ltm_resolve_deprecated_lt_args(
    list(...),
    lt_window = lt_window,
    lt_thresh_change = lt_thresh_change,
    call = match.call(expand.dots = FALSE)
  )
  dots <- lt_args$dots
  lt_window <- lt_args$lt_window
  lt_thresh_change <- lt_args$lt_thresh_change

  # basic checks -----------------------------------------------------------
  stopifnot(inherits(spidf, "spidf"))
  if (!(ts_name %in% VALID_DATA_TYPES)) {
    stop("Invalid ts_name! Must be one of: ", paste(VALID_DATA_TYPES, collapse = ", "))
  }

  # data prep --------------------------------------------------------------
  yts <- ltm_spidf_to_ts(spidf, ts_name)
  dts <- ltm_get_dates(spidf, remove_leap = TRUE)
  n   <- length(yts)

  # early exit if series too short for STL etc.
  if (!is_len_ok(n, max(10, s_window * 2 + 7))) {
    bp <- list(breakpoints = NA)
    out <- list(
      method           = "stc",
      data_type        = ts_name,
      has_breaks       = FALSE,
      has_valid_breaks_lt = FALSE,
      break_magn       = NA,
      breaks_indices   = NA,
      breaks_dates     = NA,
      output_object    = bp,
      season_adj       = season_adj,
      season_used      = FALSE,

      # short-term
      has_valid_breaks_st   = FALSE,
      st_change_pct         = NA,
      st_pre                = NA,
      st_post               = NA,
      st_window_used        = if (is.null(st_window)) NA else as.integer(st_window),

      # short-term trend validator
      has_valid_breaks_st_trend = FALSE,
      trend_rand_p_value = NA,
      trend_slope_ts = NA,
      trend_slope_ts_pct = NA,
      trend_rand_null_mean_pct = NA,
      trend_rand_null_sd_pct = NA,
      trend_rand_effect_pct = NA,
      trend_rand_B = NA,
      trend_rand_len = NA,
      trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
      post_prop_below_baseline = NA,
      post_avg_deficit_pct = NA,

      call = match.call()
    )
    class(out) <- "ts_breaks_run"
    return(out)
  }

  # NA handling + seasonal adjustment (for detection & validators) --------
  y_pre <- yts
  if (anyNA(y_pre)) y_pre <- forecast::na.interp(y_pre)
  if (season_adj && is_len_ok(n, s_window * 2 + 7)) {
    decomp <- stats::stl(y_pre, s.window = s_window)
    ysa <- forecast::seasadj(decomp)
    season_used <- TRUE
  } else {
    ysa <- y_pre
    season_used <- FALSE
  }

  # detect structural break(s) --------------------------------------------
  bp <- do.call(
    strucchangeRcpp::breakpoints,
    c(
      list(
        formula = ysa ~ 1,
        h = h,
        breaks = breaks
      ),
      dots
    )
  )

  # no break case
  if (all(is.na(bp$breakpoints))) {
    out <- list(
      method           = "stc",
      data_type        = ts_name,
      has_breaks       = FALSE,
      has_valid_breaks_lt = FALSE,
      break_magn       = NA,
      breaks_indices   = NA,
      breaks_dates     = NA,
      output_object    = bp,
      season_adj       = season_adj,
      season_used      = season_used,

      # short-term
      has_valid_breaks_st   = FALSE,
      st_change_pct         = NA,
      st_pre                = NA,
      st_post               = NA,
      st_window_used        = if (is.null(st_window)) NA else as.integer(st_window),

      # short-term trend validator
      has_valid_breaks_st_trend = FALSE,
      trend_rand_p_value = NA,
      trend_slope_ts = NA,
      trend_slope_ts_pct = NA,
      trend_rand_null_mean_pct = NA,
      trend_rand_null_sd_pct = NA,
      trend_rand_effect_pct = NA,
      trend_rand_B = NA,
      trend_rand_len = NA,
      trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
      post_prop_below_baseline = NA,
      post_avg_deficit_pct = NA,

      call = match.call()
    )
    class(out) <- "ts_breaks_run"
    return(out)
  }

  # use the first break (minimal change for stability) --------------------
  brk <- as.integer(bp$breakpoints[1])
  if (!is.finite(brk) || brk < 2 || brk >= n) {
    out <- list(
      method           = "stc",
      data_type        = ts_name,
      has_breaks       = TRUE,
      has_valid_breaks_lt = FALSE,
      break_magn       = NA,
      breaks_indices   = brk,
      breaks_dates     = NA,
      output_object    = bp,
      season_adj       = season_adj,
      season_used      = season_used,

      # short-term
      has_valid_breaks_st   = FALSE,
      st_change_pct         = NA,
      st_pre                = NA,
      st_post               = NA,
      st_window_used        = if (is.null(st_window)) NA else as.integer(st_window),

      # short-term trend validator
      has_valid_breaks_st_trend = FALSE,
      trend_rand_p_value = NA,
      trend_slope_ts = NA,
      trend_slope_ts_pct = NA,
      trend_rand_null_mean_pct = NA,
      trend_rand_null_sd_pct = NA,
      trend_rand_effect_pct = NA,
      trend_rand_B = NA,
      trend_rand_len = NA,
      trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
      post_prop_below_baseline = NA,
      post_avg_deficit_pct = NA,

      call = match.call()
    )
    class(out) <- "ts_breaks_run"
    return(out)
  }

  break_date <- dts[brk]

  # long-term validator inputs --------------------------------------------
  if (is.null(lt_window)) {
    pre_vals  <- ysa[1:(brk - 1)]
    post_vals <- ysa[(brk + 1):n]
  } else {
    pre_rng   <- bound_idx(brk - lt_window, brk - 1, n)
    post_rng  <- bound_idx(brk + 1, brk + lt_window, n)
    pre_vals  <- ysa[pre_rng]
    post_vals <- ysa[post_rng]
  }

  val_pre    <- suppressWarnings(lt_fun(pre_vals,  na.rm = TRUE))
  val_post   <- suppressWarnings(lt_fun(post_vals, na.rm = TRUE))
  break_magn <- pct_change(val_post, val_pre)

  lt_valid <- isTRUE(break_date >= thresh_date) &&
    isTRUE(is.finite(break_magn) && (break_magn <= lt_thresh_change))

  # short-term validator ---------------------------------------------------
  st_valid <- FALSE
  st_change_pct <- st_pre <- st_post <- NA
  st_used <- if (is.null(st_window)) NA else as.integer(st_window)

  if (!is.null(st_window) && is_len_ok(st_window, 1)) {
    pre_rng_st  <- bound_idx(brk - st_window, brk - 1, n)
    post_rng_st <- bound_idx(brk + 1,      brk + st_window, n)
    st_pre  <- suppressWarnings(st_fun(ysa[pre_rng_st],  na.rm = TRUE))
    st_post <- suppressWarnings(st_fun(ysa[post_rng_st], na.rm = TRUE))
    st_change_pct <- pct_change(st_post, st_pre)
    st_valid <- isTRUE(break_date >= thresh_date) &&
      isTRUE(is.finite(st_change_pct) && (st_change_pct <= st_thresh_change))
  }

  # randomized, recovery-aware trend validator ----------------------------
  has_valid_breaks_st_trend <- FALSE
  trend_rand_p_value <- NA
  trend_slope_ts <- NA
  trend_slope_ts_pct <- NA
  trend_rand_null_mean_pct <- NA
  trend_rand_null_sd_pct <- NA
  trend_rand_effect_pct <- NA
  trend_rand_B_out <- NA
  trend_rand_len <- NA
  post_prop_below_baseline <- NA
  post_avg_deficit_pct <- NA
  trend_used <- if (is.null(trend_window)) NA else as.integer(trend_window)

  if (!is.null(trend_window) && is_len_ok(trend_window, 3)) {
    trv <- ltm_trend_validator_randomized(
      ysa = ysa, dts = dts, brk = brk, trend_window = trend_window,
      thresh_date = thresh_date,
      trend_require_lower_level = trend_require_lower_level,
      B                  = trend_rand_B,
      seed               = trend_rand_seed,
      post_pct_thresh    = trend_post_pct_thresh,
      alpha              = trend_alpha,
      deficit_tol        = trend_deficit_tol,
      min_prop_below     = trend_min_prop_below,
      avg_deficit_thresh = trend_avg_deficit_thresh
    )
    has_valid_breaks_st_trend <- trv$has_valid_breaks_st_trend
    trend_rand_p_value          <- trv$trend_rand_p_value
    trend_slope_ts              <- trv$trend_slope_ts
    trend_slope_ts_pct          <- trv$trend_slope_ts_pct
    trend_rand_null_mean_pct    <- trv$trend_rand_null_mean_pct
    trend_rand_null_sd_pct      <- trv$trend_rand_null_sd_pct
    trend_rand_effect_pct       <- trv$trend_rand_effect_pct
    trend_rand_B_out            <- trv$trend_rand_B
    trend_rand_len              <- trv$trend_rand_len
    post_prop_below_baseline    <- trv$post_prop_below_baseline
    post_avg_deficit_pct        <- trv$post_avg_deficit_pct
  }

  # assemble output --------------------------------------------------------
  out <- list(
    method           = "stc",
    data_type        = ts_name,
    has_breaks       = TRUE,
    has_valid_breaks_lt = isTRUE(lt_valid),
    break_magn       = break_magn,
    breaks_indices   = brk,
    breaks_dates     = break_date,
    output_object    = bp,
    season_adj       = season_adj,
    season_used      = season_used,

    # short-term
    has_valid_breaks_st   = isTRUE(st_valid),
    st_change_pct         = st_change_pct,
    st_pre                = st_pre,
    st_post               = st_post,
    st_window_used        = st_used,

    # short-term trend validator
    has_valid_breaks_st_trend = isTRUE(has_valid_breaks_st_trend),
    trend_rand_p_value = trend_rand_p_value,
    trend_slope_ts = trend_slope_ts,
    trend_slope_ts_pct = trend_slope_ts_pct,
    trend_rand_null_mean_pct = trend_rand_null_mean_pct,
    trend_rand_null_sd_pct = trend_rand_null_sd_pct,
    trend_rand_effect_pct = trend_rand_effect_pct,
    trend_rand_B = trend_rand_B_out,
    trend_rand_len = trend_rand_len,
    trend_window_used = trend_used,
    post_prop_below_baseline = post_prop_below_baseline,
    post_avg_deficit_pct = post_avg_deficit_pct,

    call = match.call()
  )

  class(out) <- "ts_breaks_run"
  return(out)
}



ltm_wbs_detect_breaks <- function(spidf,
                                  ts_name    = "spi",
                                  season_adj = TRUE,
                                  s_window   = 30,
                                  thresh_date,
                                  # --- long-term check ---
                                  lt_window        = NULL,
                                  lt_thresh_change = -10,
                                  lt_fun           = median,
                                  num_intervals    = 1000,
                                  # --- short-term check ---
                                  st_window        = NULL,
                                  st_thresh_change = -10,
                                  st_fun           = median,
                                  # --- trend check (randomized validator) ---
                                  trend_window               = NULL,                 # integer; if set, enables trend validator
                                  trend_require_lower_level  = TRUE,
                                  trend_rand_B               = 99,
                                  trend_rand_seed            = 11235,
                                  trend_post_pct_thresh      = -10,
                                  trend_alpha                = 0.1,
                                  trend_deficit_tol          = 0.05,
                                  trend_min_prop_below       = 0.6,
                                  trend_avg_deficit_thresh   = -1.5,
                                  ...) {
  lt_args <- ltm_resolve_deprecated_lt_args(
    list(...),
    lt_window = lt_window,
    lt_thresh_change = lt_thresh_change,
    call = match.call(expand.dots = FALSE)
  )
  dots <- lt_args$dots
  lt_window <- lt_args$lt_window
  lt_thresh_change <- lt_args$lt_thresh_change

  stopifnot(inherits(spidf, "spidf"))
  if (!(ts_name %in% VALID_DATA_TYPES)) {
    stop("Invalid ts_name! Must be one of: ", paste(VALID_DATA_TYPES, collapse = ", "))
  }

  # data -------------------------------------------------------------------
  yts <- ltm_spidf_to_ts(spidf, ts_name)
  dts <- ltm_get_dates(spidf, remove_leap = TRUE)
  n   <- length(yts)

  # early return if too short ----------------------------------------------
  if (!is_len_ok(n, max(10, s_window * 2 + 7))) {
    cp_fit <- list(cpt.th = numeric(0))
    out <- list(method = "wbs",
                data_type = ts_name,
                has_breaks = FALSE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = NA,
                breaks_dates = NA,
                output_object = cp_fit,
                season_adj = season_adj,
                season_used = FALSE,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  # NA handling + seasonal adjustment -------------------------------------
  y_pre <- yts
  if (anyNA(y_pre)) y_pre <- forecast::na.interp(y_pre)
  if (season_adj && is_len_ok(n, s_window * 2 + 7)) {
    decomp <- stats::stl(y_pre, s.window = s_window)
    ysa <- forecast::seasadj(decomp)
    season_used <- TRUE
  } else {
    ysa <- y_pre
    season_used <- FALSE
  }

  # Wild Binary Segmentation -----------------------------------------------
  wbs_fit <- wbs::wbs(ysa, numIntervals = num_intervals)
  cp_fit  <- do.call(
    wbs::changepoints,
    c(
      list(wbs_fit, penalty = "ssic.penalty"),
      dots
    )
  )

  # extract break candidates -----------------------------------------------
  if (length(cp_fit$cpt.th) == 0) {
    out <- list(method = "wbs",
                data_type = ts_name,
                has_breaks = FALSE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = NA,
                breaks_dates = NA,
                output_object = cp_fit,
                season_adj = season_adj,
                season_used = season_used,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  brk_vec <- if (is.list(cp_fit$cpt.th)) cp_fit$cpt.th[[1]] else cp_fit$cpt.th
  if (length(brk_vec) == 0) {
    out <- list(method = "wbs",
                data_type = ts_name,
                has_breaks = FALSE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = NA,
                breaks_dates = NA,
                output_object = cp_fit,
                season_adj = season_adj,
                season_used = season_used,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  brk <- as.integer(brk_vec[1])
  if (!is.finite(brk) || brk < 2 || brk >= n) {
    out <- list(method = "wbs",
                data_type = ts_name,
                has_breaks = TRUE,
                has_valid_breaks_lt = FALSE,
                break_magn = NA,
                breaks_indices = brk,
                breaks_dates = NA,
                output_object = cp_fit,
                season_adj = season_adj,
                season_used = season_used,

                # short-term
                has_valid_breaks_st = FALSE,
                st_change_pct = NA,
                st_pre = NA,
                st_post = NA,
                st_window_used = if (is.null(st_window)) NA else as.integer(st_window),

                # short-term trend validator
                has_valid_breaks_st_trend = FALSE,
                trend_rand_p_value = NA,
                trend_slope_ts = NA,
                trend_slope_ts_pct = NA,
                trend_rand_null_mean_pct = NA,
                trend_rand_null_sd_pct = NA,
                trend_rand_effect_pct = NA,
                trend_rand_B = NA,
                trend_rand_len = NA,
                trend_window_used = if (is.null(trend_window)) NA else as.integer(trend_window),
                post_prop_below_baseline = NA,
                post_avg_deficit_pct = NA,

                call = match.call())
    class(out) <- "ts_breaks_run"
    return(out)
  }

  break_date <- dts[brk]

  # long-term validator inputs --------------------------------------------
  if (is.null(lt_window)) {
    pre_vals  <- ysa[1:(brk - 1)]
    post_vals <- ysa[(brk + 1):n]
  } else {
    pre_rng   <- bound_idx(brk - lt_window, brk - 1, n)
    post_rng  <- bound_idx(brk + 1, brk + lt_window, n)
    pre_vals  <- ysa[pre_rng]
    post_vals <- ysa[post_rng]
  }

  val_pre    <- suppressWarnings(lt_fun(pre_vals,  na.rm = TRUE))
  val_post   <- suppressWarnings(lt_fun(post_vals, na.rm = TRUE))
  break_magn <- pct_change(val_post, val_pre)

  lt_valid <- isTRUE(break_date >= thresh_date) &&
    isTRUE(is.finite(break_magn) && (break_magn <= lt_thresh_change))

  # short-term validator ---------------------------------------------------
  st_valid <- FALSE
  st_change_pct <- st_pre <- st_post <- NA
  st_used <- if (is.null(st_window)) NA else as.integer(st_window)

  if (!is.null(st_window) && is_len_ok(st_window, 1)) {
    pre_rng_st  <- bound_idx(brk - st_window, brk - 1, n)
    post_rng_st <- bound_idx(brk + 1,      brk + st_window, n)
    st_pre  <- suppressWarnings(st_fun(ysa[pre_rng_st],  na.rm = TRUE))
    st_post <- suppressWarnings(st_fun(ysa[post_rng_st], na.rm = TRUE))
    st_change_pct <- pct_change(st_post, st_pre)
    st_valid <- isTRUE(break_date >= thresh_date) &&
      isTRUE(is.finite(st_change_pct) && (st_change_pct <= st_thresh_change))
  }

  # randomized, recovery-aware trend validator ----------------------------
  has_valid_breaks_st_trend <- FALSE
  trend_rand_p_value <- NA
  trend_slope_ts <- NA
  trend_slope_ts_pct <- NA
  trend_rand_null_mean_pct <- NA
  trend_rand_null_sd_pct <- NA
  trend_rand_effect_pct <- NA
  trend_rand_B_out <- NA
  trend_rand_len <- NA
  post_prop_below_baseline <- NA
  post_avg_deficit_pct <- NA
  trend_used <- if (is.null(trend_window)) NA else as.integer(trend_window)

  if (!is.null(trend_window) && is_len_ok(trend_window, 3)) {
    trv <- ltm_trend_validator_randomized(
      ysa = ysa, dts = dts, brk = brk, trend_window = trend_window,
      thresh_date = thresh_date,
      trend_require_lower_level = trend_require_lower_level,
      B                  = trend_rand_B,
      seed               = trend_rand_seed,
      post_pct_thresh    = trend_post_pct_thresh,
      alpha              = trend_alpha,
      deficit_tol        = trend_deficit_tol,
      min_prop_below     = trend_min_prop_below,
      avg_deficit_thresh = trend_avg_deficit_thresh
    )
    has_valid_breaks_st_trend <- trv$has_valid_breaks_st_trend
    trend_rand_p_value          <- trv$trend_rand_p_value
    trend_slope_ts              <- trv$trend_slope_ts
    trend_slope_ts_pct          <- trv$trend_slope_ts_pct
    trend_rand_null_mean_pct    <- trv$trend_rand_null_mean_pct
    trend_rand_null_sd_pct      <- trv$trend_rand_null_sd_pct
    trend_rand_effect_pct       <- trv$trend_rand_effect_pct
    trend_rand_B_out            <- trv$trend_rand_B
    trend_rand_len              <- trv$trend_rand_len
    post_prop_below_baseline    <- trv$post_prop_below_baseline
    post_avg_deficit_pct        <- trv$post_avg_deficit_pct
  }

  # assemble output --------------------------------------------------------
  out <- list(method = "wbs",
              data_type = ts_name,
              has_breaks = TRUE,
              has_valid_breaks_lt = isTRUE(lt_valid),
              break_magn = break_magn,
              breaks_indices = brk,
              breaks_dates = break_date,
              output_object = cp_fit,
              season_adj = season_adj,
              season_used = season_used,

              # short-term
              has_valid_breaks_st = isTRUE(st_valid),
              st_change_pct = st_change_pct,
              st_pre = st_pre,
              st_post = st_post,
              st_window_used = st_used,

              # short-term trend validator
              has_valid_breaks_st_trend = isTRUE(has_valid_breaks_st_trend),
              trend_rand_p_value = trend_rand_p_value,
              trend_slope_ts = trend_slope_ts,
              trend_slope_ts_pct = trend_slope_ts_pct,
              trend_rand_null_mean_pct = trend_rand_null_mean_pct,
              trend_rand_null_sd_pct = trend_rand_null_sd_pct,
              trend_rand_effect_pct = trend_rand_effect_pct,
              trend_rand_B = trend_rand_B_out,
              trend_rand_len = trend_rand_len,
              trend_window_used = trend_used,
              post_prop_below_baseline = post_prop_below_baseline,
              post_avg_deficit_pct = post_avg_deficit_pct,

              call = match.call())

  class(out) <- "ts_breaks_run"
  return(out)
}








