make_test_spidf <- function() {
  x <- data.frame(
    ti = as.Date("2020-01-01") + 0:9,
    spi = c(0.82, 0.81, 0.8, 0.79, 0.78, 0.62, 0.61, 0.6, 0.59, 0.58),
    spi_mov_wind = c(0.82, 0.81, 0.8, 0.79, 0.78, 0.62, 0.61, 0.6, 0.59, 0.58),
    spi_smooth = c(0.82, 0.81, 0.8, 0.79, 0.78, 0.62, 0.61, 0.6, 0.59, 0.58),
    spi_mov_smooth = c(0.82, 0.81, 0.8, 0.79, 0.78, 0.62, 0.61, 0.6, 0.59, 0.58)
  )
  class(x) <- c("spidf", class(x))

  attr(x, "lat") <- 41.27998
  attr(x, "lon") <- -8.2906
  attr(x, "start_date") <- as.Date("2020-01-01")
  attr(x, "end_date") <- as.Date("2020-01-10")
  attr(x, "range_start") <- as.Date("2020-01-01")
  attr(x, "range_end") <- as.Date("2020-01-10")
  attr(x, "spi") <- "NDVI"
  attr(x, "proc_level") <- "L2A"
  attr(x, "crs_code") <- "EPSG:4326"
  attr(x, "regularized") <- TRUE
  attr(x, "regularize_method") <- "linear"
  attr(x, "mov_window") <- TRUE
  attr(x, "mov_window_quantile") <- 0.9
  attr(x, "mov_window_size_days") <- 30L
  attr(x, "whit_smoothing") <- TRUE
  attr(x, "whit_lambda") <- 1000
  attr(x, "whit_quantile_threshold") <- 0.9
  attr(x, "whit_weights_used") <- TRUE
  attr(x, "tree_id") <- "tree-001"
  attr(x, "use_buffer") <- FALSE
  attr(x, "buffer_radius_m") <- 0
  attr(x, "cloud_mask_threshold") <- 0.2

  x
}

make_test_ts_breaks_run <- function() {
  structure(
    list(
      method = "ed",
      data_type = "spi",
      has_breaks = TRUE,
      has_valid_breaks_lt_med = TRUE,
      has_valid_breaks_st_med = TRUE,
      has_valid_breaks_st_trend = FALSE,
      break_magn = -24.5,
      breaks_indices = 6L,
      breaks_dates = as.Date("2020-01-06"),
      season_adj = FALSE,
      season_used = FALSE,
      st_change_pct = -18.2,
      st_pre = 0.79,
      st_post = 0.61,
      st_window_used = 3L,
      trend_rand_p_value = 0.08,
      trend_slope_ts = -0.03,
      trend_slope_ts_pct = -12.4,
      trend_rand_null_mean_pct = -2.1,
      trend_rand_null_sd_pct = 1.8,
      trend_rand_effect_pct = -10.3,
      trend_rand_B = 99L,
      trend_rand_len = 6L,
      trend_window_used = 3L,
      post_prop_below_baseline = 0.75,
      post_avg_deficit_pct = -7.2,
      call = quote(dummy_call())
    ),
    class = "ts_breaks_run"
  )
}

test_that("ts_breaks stores validator flags with the renamed slots only", {
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_ts_breaks(make_test_spidf())
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_add_runs(ts_breaks_obj, make_test_ts_breaks_run())

  run_details <- LargeTreeMonitoring:::ltm_get_run_details(ts_breaks_obj, "ed", "run-01")

  expect_true(run_details$has_valid_breaks_lt_med)
  expect_true(run_details$has_valid_breaks_st_med)
  expect_false(run_details$has_valid_breaks_st_trend)
  expect_true(all(c(
    "has_valid_breaks_lt_med",
    "has_valid_breaks_st_med",
    "has_valid_breaks_st_trend"
  ) %in% names(run_details)))
})

test_that("as.data.frame.ts_breaks exposes the renamed validator columns", {
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_ts_breaks(make_test_spidf())
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_add_runs(ts_breaks_obj, make_test_ts_breaks_run())

  out <- as.data.frame(ts_breaks_obj)

  expect_identical(
    c(
      "has_valid_breaks_lt_med",
      "has_valid_breaks_st_med",
      "has_valid_breaks_st_trend"
    ),
    names(out)[match(
      c(
        "has_valid_breaks_lt_med",
        "has_valid_breaks_st_med",
        "has_valid_breaks_st_trend"
      ),
      names(out)
    )]
  )
  expect_true(out$has_valid_breaks_lt_med[[1]])
  expect_true(out$has_valid_breaks_st_med[[1]])
  expect_false(out$has_valid_breaks_st_trend[[1]])
})

test_that("print methods display the renamed validator labels", {
  run_obj <- make_test_ts_breaks_run()
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_ts_breaks(make_test_spidf())
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_add_runs(ts_breaks_obj, run_obj)

  run_output <- paste(capture.output(print(run_obj)), collapse = "\n")
  expect_match(run_output, "Long-term median valid", fixed = TRUE)
  expect_match(run_output, "Short-term median valid", fixed = TRUE)
  expect_match(run_output, "Short-term trend valid", fixed = TRUE)

  ts_breaks_output <- paste(capture.output(print(ts_breaks_obj)), collapse = "\n")
  expect_match(ts_breaks_output, "Long-term median valid", fixed = TRUE)
  expect_match(ts_breaks_output, "Short-term median valid", fixed = TRUE)
  expect_match(ts_breaks_output, "Short-term trend valid", fixed = TRUE)
})

test_that("downstream helpers use the renamed validator columns", {
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_ts_breaks(make_test_spidf())
  ts_breaks_obj <- LargeTreeMonitoring:::ltm_add_runs(ts_breaks_obj, make_test_ts_breaks_run())
  df_breaks <- as.data.frame(ts_breaks_obj)

  summary_text <- LargeTreeMonitoring:::ltm_summarize_break_df(df_breaks)
  expect_match(summary_text, "long-term median valid", ignore.case = TRUE)

  valid_breaks_plot <- LargeTreeMonitoring:::plot_valid_breaks(df_breaks)
  expect_true(inherits(valid_breaks_plot, "patchwork") || inherits(valid_breaks_plot, "ggplot"))

  ts_plot <- LargeTreeMonitoring:::ltm_plot_spidf_ts(
    spidf_obj = make_test_spidf(),
    df_breaks = df_breaks,
    only_valid_breaks = TRUE,
    valid_breaks_mode = "all",
    validators_sel = c("lt_med", "st_med", "st_trend")
  )
  expect_s3_class(ts_plot, "ggplot")
})
