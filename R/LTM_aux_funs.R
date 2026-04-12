

#' Convert an `spidf` column to a `zoo` time series
#'
#' Creates a [zoo::zoo()] object using the selected value column and the `ti`
#' dates stored in an `spidf` object.
#'
#' @param spidf_obj An object of class `spidf`.
#' @param yname Character scalar naming the column to use as the time-series
#'   values.
#'
#' @return A `zoo` object indexed by `spidf_obj$ti`.
#' @family time-series conversion helpers
#' @export
ltm_spidf_to_zoo <- function(spidf_obj, yname){

  stopifnot(inherits(spidf_obj, "spidf"))

  ts_obj <- zoo::zoo(spidf_obj[,yname], order.by = spidf_obj$ti)

  return(ts_obj)

}

#' Convert an `spidf` column to a regular `ts` object
#'
#' Creates a base R [stats::ts()] object from an `spidf` value column. Leap-day
#' rows are removed before conversion so the default annual frequency of 365
#' remains aligned with day-of-year indexing.
#'
#' @param spidf_obj An object of class `spidf`.
#' @param yname Character scalar naming the column to use as the time-series
#'   values.
#' @param freq Numeric frequency passed to [stats::ts()]. The default is `365`.
#'
#' @return A `ts` object whose start is derived from the first non-leap date in
#'   `spidf_obj$ti`.
#' @family time-series conversion helpers
#' @export
ltm_spidf_to_ts <- function(spidf_obj, yname, freq = 365){

  stopifnot(inherits(spidf_obj, "spidf"))

  spidf_obj <- ltm_remove_leap_days(spidf_obj)

  ts_obj <- stats::ts(
    spidf_obj[,yname],
    start = c(
      lubridate::year(min(spidf_obj$ti)),
      lubridate::yday(min(spidf_obj$ti))
    ),
    frequency = freq
  )

  return(ts_obj)

}

#' Remove leap-day observations from an `spidf`
#'
#' @param spidf_obj An object of class `spidf` containing a `ti` date column.
#' @return An object of class `spidf` without rows dated February 29.
#' @keywords internal
#' @noRd
ltm_remove_leap_days <- function(spidf_obj) {

  stopifnot(inherits(spidf_obj, "spidf"))
  stopifnot("ti" %in% names(spidf_obj))  # Ensure the column exists

  out_df <- spidf_obj[!(format(spidf_obj$ti, "%m-%d") == "02-29"), ]

  out_df <- ltm_copy_metadata(out_df,spidf_obj)
  class(out_df) <- c("spidf", "data.frame")

  return(out_df)
}


#' Extract dates from an `spidf`
#'
#' @param spidf_obj An object of class `spidf` containing a `ti` date column.
#' @param remove_leap Logical scalar. If `TRUE`, omit February 29 dates.
#' @return Date vector.
#' @keywords internal
#' @noRd
ltm_get_dates <- function(spidf_obj, remove_leap=TRUE) {
  stopifnot(inherits(spidf_obj, "spidf"))
  stopifnot("ti" %in% names(spidf_obj))  # Ensure the column exists

  if(remove_leap){
    out_ti <- as.Date(spidf_obj[!(format(spidf_obj$ti, "%m-%d") == "02-29"), "ti"])
  }else{
    out_ti <- as.Date(spidf_obj[, "ti"])
  }
  return(out_ti)
}


#' Identify duplicate acquisition dates in an `spidf`
#'
#' @param spidf_obj An object of class `spidf` containing a `ti` date column.
#' @return Integer vector of row indices whose dates are duplicated.
#' @keywords internal
#' @noRd
ltm_get_duplicate_date_indices <- function(spidf_obj) {

  stopifnot(inherits(spidf_obj, "spidf"))

  stopifnot("ti" %in% names(spidf_obj))

  # Ensure ti is Date class
  ti_dates <- as.Date(spidf_obj$ti)

  # Get duplicated indices: first and subsequent occurrences
  duplicated_indices <- which(duplicated(ti_dates) | duplicated(ti_dates, fromLast = TRUE))

  return(duplicated_indices)
}


#' Remove duplicate acquisition dates from an `spidf`
#'
#' Keeps one row per date, selecting the row with the largest `spi` value when
#' duplicate dates are present.
#'
#' @param spidf_obj An object of class `spidf` containing `ti` and `spi`
#'   columns.
#' @return An object of class `spidf` sorted by date.
#' @keywords internal
#' @noRd
ltm_remove_duplicates <- function(spidf_obj) {

  stopifnot(inherits(spidf_obj, "spidf"))

  stopifnot(all(c("ti", "spi") %in% names(spidf_obj)))

  out_df <- spidf_obj %>%
    dplyr::mutate(ti = as.Date(ti)) %>%
    dplyr::group_by(ti) %>%
    dplyr::slice_max(order_by = spi, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ti) %>%
    as.data.frame()

  out_df <- ltm_copy_metadata(out_df,spidf_obj)
  class(out_df) <- c("spidf", "data.frame")

  return(out_df)
}

#' Copy `spidf` metadata attributes
#'
#' Copies the metadata attributes used by LargeTreeMonitoring from one object to
#' another. This includes spatial coordinates, requested and available date
#' ranges, spectral-index metadata, buffer settings, and preprocessing flags.
#' The function does not validate that `x` and `from` contain compatible rows or
#' time-series values.
#'
#' @param x Object that will receive metadata attributes.
#' @param from Object, normally an `spidf`, from which metadata attributes are
#'   read through the package accessors.
#'
#' @return Object `x` with copied attributes.
#' @family metadata helpers
#' @export
ltm_copy_metadata <- function(x, from){

  #stopifnot(inherits(x, "spidf"))
  #stopifnot(inherits(from, "spidf"))

  # Copy metadata
  attr(x, "lat") <- get_latitude(from)
  attr(x, "lon") <- get_longitude(from)
  attr(x, "start_date") <- get_start_date(from)
  attr(x, "end_date") <- get_end_date(from)
  attr(x, "range_start") <- get_range_start(from)
  attr(x, "range_end") <- get_range_end(from)

  attr(x, "tree_id") <- get_tree_id(from)

  attr(x, "use_buffer") <- get_use_buffer(from)
  attr(x, "buffer_radius_m") <- get_buffer_radius_m(from)
  attr(x, "cloud_mask_threshold") <- get_cloud_mask_threshold(from)

  attr(x, "spi") <- get_spi(from)
  attr(x, "proc_level") <- get_proc_level(from)
  attr(x, "crs_code") <- get_crs_code(from)

  attr(x, "regularized") <- is_regularized(from)
  attr(x, "regularize_method") <- get_regularize_method(from)

  attr(x, "mov_window") <- has_mov_window(from)
  attr(x, "mov_window_quantile") <- get_mov_window_quantile(from)
  attr(x, "mov_window_size_days") <- get_mov_window_size_days(from)

  attr(x, "whit_smoothing") <- is_whit_smoothed(from)
  attr(x, "whit_lambda") <- get_whit_lambda(from)
  attr(x, "whit_quantile_threshold") <- get_whit_quantile_threshold(from)
  attr(x, "whit_weights_used") <- get_whit_weights_used(from)

  return(x)
}


#' Count days between two dates
#'
#' Coerces both inputs to `Date` and returns the integer day difference
#' `end_date - start_date`.
#'
#' @param start_date Start date or value coercible to `Date`.
#' @param end_date End date or value coercible to `Date`.
#'
#' @return Integer scalar number of days between `start_date` and `end_date`.
#' @family time-series conversion helpers
#' @export
ltm_days_between <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  as.integer(difftime(end_date, start_date, units = "days"))
}

#' Convert a fractional-year break date to `Date`
#'
#' @param breakDate Numeric fractional year.
#' @return A `Date` approximating the fractional-year position with a 365-day
#'   year.
#' @keywords internal
#' @noRd
ltm_convert_breakdate_to_date <- function(breakDate) {
  year <- floor(breakDate)
  fractional_year <- breakDate - year
  day_of_year <- round(fractional_year * 365)

  # Generate the date
  formal_date <- as.Date(day_of_year - 1, origin = paste0(year, "-01-01"))

  return(formal_date)
}


#' Copy time-series attributes to new values
#'
#' Builds a new [stats::ts()] object with the same start, end, and frequency as
#' an existing `ts` object, replacing only the observed values.
#'
#' @param ts_to_copy A base R `ts` object whose time attributes are copied.
#' @param values Vector of replacement values. Its length must equal
#'   `length(ts_to_copy)`.
#'
#' @return A `ts` object containing `values` and the time attributes of
#'   `ts_to_copy`.
#' @family time-series conversion helpers
#' @export
ltm_copy_ts <- function(ts_to_copy, values){

  stopifnot(stats::is.ts(ts_to_copy))

  if (length(values) != length(ts_to_copy)){
    stop("Length of values vector must match length of ts_to_copy")
  }

  new_ts <- stats::ts(
    values,
    start = stats::start(ts_to_copy),
    end = stats::end(ts_to_copy),
    frequency = stats::frequency(ts_to_copy)
  )

  return(new_ts)
}



#' Summarize flat break-detection results
#'
#' Creates a human-readable text summary from a data frame of breakpoint runs,
#' such as the output of [as.data.frame.ts_breaks()]. The summary reports the
#' number of runs, detected breaks, validator pass counts, and grouped
#' summaries for long-term, short-term, and short-term trend validators when
#' their columns are present.
#'
#' @param break_df Data frame containing one row per break-detection run. Common
#'   columns include `method` or `algorithm`, `data_type`, `has_breaks`,
#'   `break_date`, `break_magn`, `has_valid_breaks_lt`,
#'   `has_valid_breaks_st`, and `has_valid_breaks_st_trend`.
#'
#' @return Character scalar containing a multi-line summary.
#' @family break result helpers
#' @export
ltm_summarize_break_df <- function(break_df) {
  if (!is.data.frame(break_df)) {
    stop("Error: 'break_df' must be a data.frame.")
  }

  if (nrow(break_df) == 0) {
    return("No break-detection runs are available.")
  }

  normalize_flag <- function(x) {
    x <- as.logical(x)
    x[is.na(x)] <- FALSE
    x
  }

  safe_numeric <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x[is.finite(x)]
  }

  format_group_label <- function(method, data_type) {
    sprintf("%s on %s", toupper(as.character(method)), as.character(data_type))
  }

  format_metric <- function(df, col, label, suffix = "", digits = 2) {
    if (!(col %in% names(df))) {
      return(NULL)
    }

    vals <- safe_numeric(df[[col]])
    if (!length(vals)) {
      return(NULL)
    }

    sprintf("%s = %s%s", label, format(round(mean(vals), digits), nsmall = digits), suffix)
  }

  format_validator_section <- function(df, validator_col, heading, metric_parts, empty_message) {
    if (!(validator_col %in% names(df))) {
      return(c(paste0(heading, ":"), paste0(". ", empty_message)))
    }

    df_valid <- df[normalize_flag(df[[validator_col]]) & !is.na(df$break_date), , drop = FALSE]

    if (nrow(df_valid) == 0) {
      return(c(paste0(heading, ":"), paste0(". ", empty_message)))
    }

    split_key <- paste(df_valid$method, df_valid$data_type, sep = "\r")
    grouped <- split(df_valid, split_key)

    lines <- vapply(grouped, function(chunk) {
      first_break <- min(chunk$break_date, na.rm = TRUE)
      metrics <- unlist(lapply(metric_parts, function(metric) {
        format_metric(
          chunk,
          col = metric$col,
          label = metric$label,
          suffix = metric$suffix,
          digits = metric$digits
        )
      }), use.names = FALSE)

      line <- sprintf(
        ". %s: %d break(s), first on %s",
        format_group_label(chunk$method[[1]], chunk$data_type[[1]]),
        nrow(chunk),
        format(first_break, "%Y-%m-%d")
      )

      if (length(metrics)) {
        line <- sprintf("%s (%s)", line, paste(metrics, collapse = "; "))
      }

      line
    }, character(1))

    c(paste0(heading, ":"), unname(lines))
  }

  if (!("method" %in% names(break_df))) {
    if ("algorithm" %in% names(break_df)) {
      break_df$method <- break_df$algorithm
    } else {
      break_df$method <- "unknown"
    }
  }

  if (!("data_type" %in% names(break_df))) {
    break_df$data_type <- "unknown"
  }

  if (!("break_date" %in% names(break_df))) {
    break_df$break_date <- as.Date(NA)
  } else {
    break_df$break_date <- as.Date(break_df$break_date)
  }

  for (flag_col in c("has_breaks",
                     "has_valid_breaks_lt",
                     "has_valid_breaks_st",
                     "has_valid_breaks_st_trend")) {
    if (!(flag_col %in% names(break_df))) {
      break_df[[flag_col]] <- FALSE
    } else {
      break_df[[flag_col]] <- normalize_flag(break_df[[flag_col]])
    }
  }

  df_detected <- break_df[break_df$has_breaks, , drop = FALSE]

  if (nrow(df_detected) == 0) {
    return("No breaks were detected.")
  }

  lt_count <- sum(df_detected$has_valid_breaks_lt & !is.na(df_detected$break_date))
  st_count <- sum(df_detected$has_valid_breaks_st & !is.na(df_detected$break_date))
  st_trend_count <- sum(df_detected$has_valid_breaks_st_trend & !is.na(df_detected$break_date))
  all_valid_count <- sum(
    df_detected$has_valid_breaks_lt &
      df_detected$has_valid_breaks_st &
      df_detected$has_valid_breaks_st_trend &
      !is.na(df_detected$break_date)
  )

  summary_lines <- c(
    "Break detection summary:",
    "-------------------------------------------------",
    sprintf("Runs analysed: %d", nrow(break_df)),
    sprintf("Breaks detected: %d", nrow(df_detected)),
    "Validator passes:",
    sprintf(". Long-term aggregation: %d", lt_count),
    sprintf(". Short-term aggregation: %d", st_count),
    sprintf(". Short-term trend: %d", st_trend_count),
    sprintf(". All validators: %d", all_valid_count),
    "",
    format_validator_section(
      df_detected,
      validator_col = "has_valid_breaks_lt",
      heading = "Long-term valid breaks",
      metric_parts = list(
        list(col = "break_magn", label = "avg magnitude", suffix = "%", digits = 2)
      ),
      empty_message = "No long-term valid breaks were detected."
    ),
    "",
    format_validator_section(
      df_detected,
      validator_col = "has_valid_breaks_st",
      heading = "Short-term valid breaks",
      metric_parts = list(
        list(col = "st_change_pct", label = "avg short-term change", suffix = "%", digits = 2)
      ),
      empty_message = "No short-term valid breaks were detected."
    ),
    "",
    format_validator_section(
      df_detected,
      validator_col = "has_valid_breaks_st_trend",
      heading = "Short-term trend valid breaks",
      metric_parts = list(
        list(col = "trend_slope_ts_pct", label = "avg slope", suffix = "%/yr", digits = 2),
        list(col = "trend_rand_p_value", label = "avg p-value", suffix = "", digits = 3),
        list(col = "post_prop_below_baseline", label = "prop obs below base", suffix = "", digits = 2),
        list(col = "post_avg_deficit_pct", label = "avg post deficit", suffix = "%", digits = 1)
      ),

      # trend_rand_p_value
      # trend_slope_ts_pct
      # post_prop_below_baseline - Proportion of post-break observations below
      #                            the baseline
      # post_avg_deficit_pct - Mean post-break percent deficit relative to the
      #                        pre-break baseline.
      empty_message = "No short-term trend valid breaks were detected."
    )
  )

  paste(summary_lines, collapse = "\n")
}

#' Compute the 90th percentile
#'
#' @param x Numeric vector.
#' @param na.rm Logical scalar passed to [stats::quantile()].
#' @param ... Additional arguments passed to [stats::quantile()].
#' @return Numeric scalar containing the unnamed 90th percentile.
#' @keywords internal
#' @noRd
Percentile90 <- function(x, na.rm = TRUE, ...) {
  unname(stats::quantile(x, probs = 0.9, na.rm = na.rm, ...))
}


#' Get the current-location cache file path
#'
#' @return Character scalar path to the current-location text file.
#' @keywords internal
#' @noRd
ltm_loc_file <- function() {
  file.path(ltm_cache_dir(), "current_location.txt")
}

#' Read the last stored Shiny location
#'
#' @return Character scalar location string, or `NULL` when absent.
#' @keywords internal
#' @noRd
ltm_read_location <- function() {
  loc_file <- ltm_loc_file()

  # Check if the file exists
  if (!file.exists(loc_file)) {
    return(NULL)
  }
  # read the single line
  loc_str <- readLines(loc_file, warn = FALSE)
  if (length(loc_str) > 0) {
    return(loc_str[1])
  } else {
    return(NULL)
  }
}

#' Store the last Shiny location
#'
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
ltm_store_location <- function(lat, lon) {
  loc_file <- ltm_loc_file()

  # Round or format to avoid floating point artifacts
  loc_str <- paste(round(lat, 6), round(lon, 6), sep = ",")

  writeLines(loc_str, con = loc_file)
}

