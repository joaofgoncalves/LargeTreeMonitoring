

#' Regularize an `spidf` time series to daily observations
#'
#' Converts an irregular Sentinel-2 spectral-index time series to a daily
#' sequence spanning the available data range and imputes missing daily values
#' with a selected method from \pkg{imputeTS}. The output keeps the `spidf`
#' metadata from the input and records the regularization status in attributes.
#'
#' @param spidf_obj An object of class `spidf`.
#' @param method Character scalar interpolation or imputation method. Supported
#'   values are `"linear"`, `"spline"`, `"stine"`, `"mean"`, `"kalman"`,
#'   `"locf"`, and `"nocb"`.
#' @param use_cloud_mask Logical scalar. If `TRUE`, interpolate the
#'   cloud-masked values in `masked_vals`; if `FALSE`, interpolate the raw
#'   spectral-index values in `spi`.
#' @param ... Additional arguments passed to the selected \pkg{imputeTS}
#'   imputation function.
#'
#' @return An object of class `spidf` with one row per day, columns `ti`, `spi`,
#'   `original_spi`, `masked_vals`, and `cloud_mask`, and updated attributes
#'   `regularized = TRUE` and `regularize_method = method`.
#' @family preprocessing helpers
#' @export
ltm_regularize_spidf <- function(spidf_obj, method = "linear", use_cloud_mask = TRUE, ...) {
  
  stopifnot(inherits(spidf_obj, "spidf"))
  
  supported_methods <- c("linear", "spline", "stine", "mean", "kalman", "locf", "nocb")
  if (!(method %in% supported_methods)) {
    stop(paste0("Interpolation method '", method, "' is not supported.\n",
                "Choose one of: ", paste(supported_methods, collapse = ", ")))
  }
  
  # Step 1: Prepare and clean the time series
  df <- spidf_obj %>%
    dplyr::mutate(ts = as.Date(ti)) %>%
    dplyr::arrange(ts)

  # Step 2: Create daily time sequence
  start_date <- get_range_start(spidf_obj)
  end_date   <- get_range_end(spidf_obj)
  regular_dates <- seq(from = start_date, to = end_date, by = "1 day")
  
  regular_df <- data.frame(ts = regular_dates)
  
  # Step 3: Join original data with daily dates
  df_regular <- dplyr::full_join(regular_df, df, by = "ts") %>%
    dplyr::arrange(ts)
  
  # Choose the correct column based on the flag
  spi_ <- if (use_cloud_mask) df_regular$masked_vals else df_regular$spi
  
  
  # Step 4: Interpolate using selected method
  interpolated_vals <- switch(method,
                              "linear" = imputeTS::na_interpolation(spi_, option = "linear", ...),
                              "spline" = imputeTS::na_interpolation(spi_, option = "spline", ...),
                              "stine"  = imputeTS::na_interpolation(spi_, option = "stine", ...),
                              "mean"   = imputeTS::na_mean(spi_, ...),
                              "kalman" = imputeTS::na_kalman(spi_, model = "StructTS", ...),
                              "locf"   = imputeTS::na_locf(spi_, ...),
                              "nocb"   = imputeTS::na_locf(spi_, option = "nocb", ...)
  )
  
  # Step 5: Construct the new spidf object
  out_df <- df_regular %>%
    dplyr::mutate(original_spi = spi) %>% 
    dplyr::mutate(spi = interpolated_vals) %>% 
    dplyr::mutate(ti = ts) %>%
    dplyr::select(ti, spi, original_spi, masked_vals, cloud_mask)
  
  # Copy and update metadata
  out_df <- ltm_copy_metadata(out_df, spidf_obj)
  attr(out_df, "regularized") <- TRUE
  attr(out_df, "regularize_method") <- method
  
  
  # Set class
  class(out_df) <- c("spidf", class(out_df))
  
  return(out_df)
}


#' Apply a moving quantile to a regularized `spidf` time series
#'
#' Computes a centered moving quantile over the daily `spi` column of a
#' regularized `spidf` object. The result is stored in `spi_mov_wind`; existing
#' metadata are preserved and moving-window attributes are updated.
#'
#' @param spidf_obj A regularized object of class `spidf`, typically returned
#'   by [ltm_regularize_spidf()].
#' @param quant Numeric scalar probability passed to [stats::quantile()].
#' @param win_size Integer window size in days. The window is split around each
#'   observation using `floor(win_size / 2)` days before and
#'   `ceiling(win_size / 2)` days after.
#'
#' @return An object of class `spidf` with an added `spi_mov_wind` column and
#'   updated attributes `mov_window`, `mov_window_quantile`, and
#'   `mov_window_size_days`.
#' @family preprocessing helpers
#' @export
ltm_apply_moving_quantile <- function(spidf_obj, quant = 0.95, win_size = 9) {
  
  stopifnot(inherits(spidf_obj, "spidf"))
  
  if (!isTRUE(is_regularized(spidf_obj))) {
    stop("Moving quantile can only be applied to a regularized spidf object.")
  }
  
  # Split the window evenly before and after
  before_days <- floor(win_size / 2)
  after_days  <- ceiling(win_size / 2)
  
  df <- spidf_obj %>%
    dplyr::mutate(
      spi_mov_wind = slider::slide_index_dbl(
        .x = spi,
        .i = ti,
        .f = ~ stats::quantile(.x, probs = quant, na.rm = TRUE),
        .before = lubridate::days(before_days),
        .after = lubridate::days(after_days),
        .complete = FALSE
      )
    )
  
  # Copy and store new metadata
  df <- ltm_copy_metadata(df, spidf_obj)
  
  attr(df, "mov_window") <- TRUE
  attr(df, "mov_window_quantile") <- quant
  attr(df, "mov_window_size_days") <- win_size
  class(df) <- class(spidf_obj)
  
  return(df)
}



#' Apply Whittaker smoothing to a regularized `spidf` time series
#'
#' Smooths the daily `spi` series with [phenofit::whit2()]. When moving-window
#' values are present, the function also smooths `spi_mov_wind` into
#' `spi_mov_smooth`. Optional weights can reduce the influence of lower
#' spectral-index values before smoothing the main `spi` series.
#'
#' @param spidf_obj A regularized object of class `spidf`, typically returned
#'   by [ltm_regularize_spidf()].
#' @param lambda Numeric smoothing parameter passed to [phenofit::whit2()].
#' @param quantile_thresh Numeric probability used to compute the weighting
#'   threshold when `use_weights = TRUE`.
#' @param use_weights Logical scalar. If `TRUE`, values greater than or equal to
#'   the `quantile_thresh` quantile receive weight `1`, lower non-missing values
#'   receive `min_weight`, and missing values receive `0`.
#' @param min_weight Numeric weight assigned to finite values below the
#'   threshold when `use_weights = TRUE`.
#'
#' @return An object of class `spidf` with `spi_smooth`, optional `spi_weight`,
#'   and optional `spi_mov_smooth` columns, plus updated Whittaker-smoothing
#'   attributes.
#' @family preprocessing helpers
#' @export
ltm_apply_whitaker <- function(spidf_obj, lambda = 5000, quantile_thresh = 0.35, 
                           use_weights = TRUE, min_weight = 0.01) {
  
  
  stopifnot(inherits(spidf_obj, "spidf"))
  
  # Check if series is regularized
  if (!isTRUE(is_regularized(spidf_obj))) {
    stop("Whittaker smoother requires a regularized (daily) spidf object.")
  }
  
  df <- spidf_obj
  
  # -----------------------------
  # Create weights (optional)
  # -----------------------------
  if (use_weights) {
    
    threshold <- stats::quantile(df$spi, probs = quantile_thresh, na.rm = TRUE)
    
    df <- df %>%
      dplyr::mutate(
        spi_weight = dplyr::case_when(
          spi >= threshold ~ 1,
          !is.na(spi) ~ min_weight,
          TRUE ~ 0
        )
      )
    
    df$spi_smooth <- phenofit::whit2(y = df$spi, w = df$spi_weight, lambda = lambda)
    
    # Smooth spi_mov_wind if it exists
    if ("spi_mov_wind" %in% names(df)) {
      
      # DO NOT USE WHEIGTS HERE!!!
      df$spi_mov_smooth <- phenofit::whit2(y = df$spi_mov_wind, lambda = lambda)
      
    }
  } else {
    # No weights: standard smoothing
    df$spi_smooth <- phenofit::whit2(y = df$spi, lambda = lambda)
    
    if ("spi_mov_wind" %in% names(df)) {
      df$spi_mov_smooth <- phenofit::whit2(y = df$spi_mov_wind, lambda = lambda)
    }
  }
  
  # -----------------------------
  # Preserve & update metadata
  # -----------------------------

  df <- ltm_copy_metadata(df, spidf_obj)
  
  attr(df, "whit_smoothing") <- TRUE
  attr(df, "whit_lambda") <- lambda
  attr(df, "whit_quantile_threshold") <- if (use_weights) quantile_thresh else NA
  attr(df, "whit_weights_used") <- use_weights
  

  class(df) <- class(spidf_obj)
  return(df)
}



