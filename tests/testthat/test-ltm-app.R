test_that("ltm_app returns a shiny app object and registers assets", {
  config_path <- tempfile(fileext = ".json")
  file.copy(
    LargeTreeMonitoring:::ltm_default_params_path(),
    config_path,
    overwrite = TRUE
  )

  app <- LargeTreeMonitoring::ltm_app(config_path = config_path)

  expect_s3_class(app, "shiny.appobj")

  www_dir <- system.file("www", package = "LargeTreeMonitoring")
  expect_true(nzchar(www_dir))
  expect_true(file.exists(file.path(www_dir, "LTM_logo-v1.png")))
  expect_identical(
    unname(shiny::resourcePaths()["ltm-www"]),
    www_dir
  )
})

ltm_app_ui_source <- function() {
  paste(
    c(
      deparse(body(LargeTreeMonitoring::ltm_app), width.cutoff = 500L),
      deparse(body(LargeTreeMonitoring:::ltm_workflow_step_ui), width.cutoff = 500L),
      deparse(body(LargeTreeMonitoring:::ltm_validator_controls_ui), width.cutoff = 500L)
    ),
    collapse = "\n"
  )
}

test_that("Percentile90 supports na.rm and returns a scalar numeric", {
  expect_equal(
    LargeTreeMonitoring:::Percentile90(c(1, 2, 3, 4, 5, NA), na.rm = TRUE),
    4.6
  )
})

test_that("workflow request keys and state classify sidebar progress", {
  target_request <- list(
    lat = 41.27998123,
    lon = -8.29060123,
    tree_id = "GV0001"
  )
  request_key <- LargeTreeMonitoring:::ltm_current_request_key(
    coord_request = target_request,
    start_date = as.Date("2015-01-01"),
    end_date = as.Date("2024-12-31"),
    spi = "NDVI",
    proc_level = "L1C"
  )

  expect_identical(request_key$lat, 41.279981)
  expect_identical(request_key$lon, -8.290601)
  expect_identical(request_key$tree_id, "GV0001")

  spidf <- data.frame(
    ti = as.Date("2020-01-01") + 0:2,
    spi = c(0.8, 0.7, 0.6)
  )
  class(spidf) <- c("spidf", class(spidf))

  state <- LargeTreeMonitoring:::ltm_workflow_state(
    target_request = target_request,
    current_request_key = request_key
  )
  expect_true(state$target_ready)
  expect_false(state$data_ready)
  expect_false(state$preprocess_ready)

  state <- LargeTreeMonitoring:::ltm_workflow_state(
    target_request = target_request,
    current_request_key = request_key,
    spidf = spidf,
    fetched_request_key = request_key
  )
  expect_true(state$data_ready)
  expect_false(state$preprocess_ready)

  regularized <- spidf
  attr(regularized, "regularized") <- TRUE
  state <- LargeTreeMonitoring:::ltm_workflow_state(
    target_request = target_request,
    current_request_key = request_key,
    spidf = regularized,
    fetched_request_key = request_key
  )
  expect_true(state$data_ready)
  expect_true(state$regularized)
  expect_false(state$preprocess_ready)

  preprocessed <- regularized
  preprocessed$spi_mov_wind <- c(0.8, 0.75, 0.7)
  preprocessed$spi_smooth <- c(0.79, 0.72, 0.64)
  preprocessed$spi_mov_smooth <- c(0.79, 0.74, 0.69)
  state <- LargeTreeMonitoring:::ltm_workflow_state(
    target_request = target_request,
    current_request_key = request_key,
    spidf = preprocessed,
    fetched_request_key = request_key
  )
  expect_true(state$preprocess_ready)
  expect_false(state$breaks_ready)

  state <- LargeTreeMonitoring:::ltm_workflow_state(
    target_request = target_request,
    current_request_key = request_key,
    spidf = preprocessed,
    fetched_request_key = request_key,
    break_results = list(df_breaks = data.frame())
  )
  expect_true(state$breaks_ready)

  stale_key <- request_key
  stale_key$end_date <- "2025-12-31"
  state <- LargeTreeMonitoring:::ltm_workflow_state(
    target_request = target_request,
    current_request_key = stale_key,
    spidf = preprocessed,
    fetched_request_key = request_key,
    break_results = list(df_breaks = data.frame())
  )
  expect_true(state$data_stale)
  expect_false(state$data_ready)
  expect_false(state$preprocess_ready)
  expect_false(state$breaks_ready)
})

test_that("validator settings helper maps Shiny selections to detector arguments", {
  validator_args <- LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
    lt_fun_label = "90% percentile",
    lt_window = 30,
    lt_thresh_change = -10,
    st_window = 30,
    st_thresh_change = -12,
    st_fun_label = "Median",
    trend_window = 30,
    trend_post_pct_thresh = -8,
    trend_alpha = 0.1,
    trend_deficit_tol = 0.08,
    trend_min_prop_below = 0.75,
    trend_avg_deficit_thresh = -3.5
  )

  expect_equal(validator_args$lt_window, 30L)
  expect_equal(validator_args$lt_thresh_change, -10)
  expect_equal(validator_args$st_window, 30L)
  expect_equal(validator_args$st_thresh_change, -12)
  expect_equal(validator_args$trend_window, 30L)
  expect_equal(validator_args$trend_post_pct_thresh, -8)
  expect_equal(validator_args$trend_alpha, 0.1)
  expect_equal(validator_args$trend_deficit_tol, 0.08)
  expect_equal(validator_args$trend_min_prop_below, 0.75)
  expect_equal(validator_args$trend_avg_deficit_thresh, -3.5)
  expect_equal(validator_args$lt_fun(c(1, 2, 3, 4, 5), na.rm = TRUE), 4.6)
  expect_equal(validator_args$st_fun(c(1, 2, 3), na.rm = TRUE), 2)
  expect_identical(
    attr(validator_args, "call_labels"),
    list(lt_fun = quote(Percentile90), st_fun = quote(stats::median))
  )
})

test_that("validator settings helper allows zero-valued windows to disable optional validators", {
  validator_args <- LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
    lt_fun_label = "Median",
    lt_window = 0,
    lt_thresh_change = -10,
    st_window = 0,
    st_thresh_change = -10,
    st_fun_label = "Median",
    trend_window = 0,
    trend_post_pct_thresh = -10,
    trend_alpha = 0.1,
    trend_deficit_tol = 0.05,
    trend_min_prop_below = 0.6,
    trend_avg_deficit_thresh = -1.5
  )

  expect_null(validator_args$lt_window)
  expect_null(validator_args$st_window)
  expect_null(validator_args$trend_window)
})

test_that("validator settings helper rejects unsupported thresholds", {
  expect_error(
    LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
      lt_fun_label = "Median",
      lt_window = 30,
      lt_thresh_change = 5,
      st_window = 30,
      st_thresh_change = -10,
      st_fun_label = "Median",
      trend_window = 30,
      trend_post_pct_thresh = -10,
      trend_alpha = 0.1,
      trend_deficit_tol = 0.05,
      trend_min_prop_below = 0.6,
      trend_avg_deficit_thresh = -1.5
    ),
    "Long-term percent change threshold"
  )

  expect_error(
    LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
      lt_fun_label = "Median",
      lt_window = 30,
      lt_thresh_change = -10,
      st_window = 30,
      st_thresh_change = -10,
      st_fun_label = "Median",
      trend_window = 2,
      trend_post_pct_thresh = -10,
      trend_alpha = 0.1,
      trend_deficit_tol = 0.05,
      trend_min_prop_below = 0.6,
      trend_avg_deficit_thresh = -1.5
    ),
    "Short-term trend window size"
  )

  expect_error(
    LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
      lt_fun_label = "Median",
      lt_window = 30,
      lt_thresh_change = -10,
      st_window = 30,
      st_thresh_change = -10,
      st_fun_label = "Median",
      trend_window = 30,
      trend_post_pct_thresh = -10,
      trend_alpha = 0.1,
      trend_deficit_tol = 1.2,
      trend_min_prop_below = 0.6,
      trend_avg_deficit_thresh = -1.5
    ),
    "Short-term trend deficit tolerance"
  )

  expect_error(
    LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
      lt_fun_label = "Median",
      lt_window = 30,
      lt_thresh_change = -10,
      st_window = 30,
      st_thresh_change = -10,
      st_fun_label = "Median",
      trend_window = 30,
      trend_post_pct_thresh = -10,
      trend_alpha = 0.1,
      trend_deficit_tol = 0.05,
      trend_min_prop_below = 1.1,
      trend_avg_deficit_thresh = -1.5
    ),
    "Short-term trend minimum proportion below baseline"
  )

  expect_error(
    LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
      lt_fun_label = "Median",
      lt_window = 30,
      lt_thresh_change = -10,
      st_window = 30,
      st_thresh_change = -10,
      st_fun_label = "Median",
      trend_window = 30,
      trend_post_pct_thresh = -10,
      trend_alpha = 0.1,
      trend_deficit_tol = 0.05,
      trend_min_prop_below = 0.6,
      trend_avg_deficit_thresh = 2
    ),
    "Short-term trend average deficit threshold"
  )
})

test_that("validator call labels are written back onto stored runs", {
  validator_args <- LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
    lt_fun_label = "90% percentile",
    lt_window = 30,
    lt_thresh_change = -10,
    st_window = 30,
    st_thresh_change = -10,
    st_fun_label = "Median",
    trend_window = 30,
    trend_post_pct_thresh = -10,
    trend_alpha = 0.1,
    trend_deficit_tol = 0.05,
    trend_min_prop_below = 0.6,
    trend_avg_deficit_thresh = -1.5
  )

  run_obj <- structure(
    list(call = quote(ltm_ed_detect_breaks(lt_fun = lt_fun, st_fun = st_fun))),
    class = "ts_breaks_run"
  )

  run_obj <- LargeTreeMonitoring:::ltm_apply_validator_call_labels(run_obj, validator_args)

  expect_identical(run_obj$call$lt_fun, quote(Percentile90))
  expect_identical(run_obj$call$st_fun, quote(stats::median))
})

test_that("default startup loads sample tree choices", {
  config_path <- tempfile(fileext = ".json")
  file.copy(
    LargeTreeMonitoring:::ltm_default_params_path(),
    config_path,
    overwrite = TRUE
  )

  startup <- LargeTreeMonitoring:::ltm_app_startup(config_path = config_path)

  expect_true(nzchar(startup$config_path))
  expect_gt(length(startup$tree_state$choices), 1L)
  expect_null(startup$tree_state$message)
})

test_that("tree location helper resolves configured list coordinates", {
  config_path <- tempfile(fileext = ".json")
  file.copy(
    LargeTreeMonitoring:::ltm_default_params_path(),
    config_path,
    overwrite = TRUE
  )

  startup <- LargeTreeMonitoring:::ltm_app_startup(config_path = config_path)
  loc <- LargeTreeMonitoring:::ltm_get_tree_location(
    startup$tree_state,
    "GV0001"
  )

  expect_identical(loc$tree_id, "GV0001")
  expect_equal(loc$lat, 41.27998)
  expect_equal(loc$lon, -8.2906)
  expect_null(
    LargeTreeMonitoring:::ltm_get_tree_location(startup$tree_state, "---")
  )
  expect_error(
    LargeTreeMonitoring:::ltm_get_tree_location(startup$tree_state, "missing"),
    "not found"
  )

  duplicate_state <- list(
    data = data.frame(
      id = c("A", "A"),
      lat = c(1, 2),
      lon = c(3, 4)
    ),
    tree_ids_col = "id",
    lat_col = "lat",
    lon_col = "lon",
    message = NULL
  )

  expect_error(
    LargeTreeMonitoring:::ltm_get_tree_location(duplicate_state, "A"),
    "ambiguous"
  )
})

test_that("coordinate request helper resolves explicit source", {
  config_path <- tempfile(fileext = ".json")
  file.copy(
    LargeTreeMonitoring:::ltm_default_params_path(),
    config_path,
    overwrite = TRUE
  )

  startup <- LargeTreeMonitoring:::ltm_app_startup(config_path = config_path)

  manual_request <- LargeTreeMonitoring:::ltm_resolve_coordinate_request(
    coord_source = LargeTreeMonitoring:::ltm_coord_source_manual(),
    location_id = "GV0001",
    latitude = 12.345678,
    longitude = -45.678912,
    tree_state = startup$tree_state
  )

  expect_equal(manual_request$lat, 12.345678)
  expect_equal(manual_request$lon, -45.678912)
  expect_null(manual_request$tree_id)
  expect_identical(
    manual_request$source,
    LargeTreeMonitoring:::ltm_coord_source_manual()
  )

  list_request <- LargeTreeMonitoring:::ltm_resolve_coordinate_request(
    coord_source = LargeTreeMonitoring:::ltm_coord_source_tree_list(),
    location_id = "GV0001",
    latitude = 12.345678,
    longitude = -45.678912,
    tree_state = startup$tree_state
  )

  expect_equal(list_request$lat, 41.27998)
  expect_equal(list_request$lon, -8.2906)
  expect_identical(list_request$tree_id, "GV0001")
  expect_identical(
    list_request$source,
    LargeTreeMonitoring:::ltm_coord_source_tree_list()
  )

  expect_error(
    LargeTreeMonitoring:::ltm_resolve_coordinate_request(
      coord_source = LargeTreeMonitoring:::ltm_coord_source_tree_list(),
      location_id = "---",
      latitude = 12.345678,
      longitude = -45.678912,
      tree_state = startup$tree_state
    ),
    "Select a Location ID"
  )
})

test_that("ltm_app UI exposes the new validator controls", {
  app_body <- ltm_app_ui_source()

  expect_match(app_body, '"st_window"', fixed = TRUE)
  expect_match(app_body, '"lt_window"', fixed = TRUE)
  expect_match(app_body, '"lt_thresh_change"', fixed = TRUE)
  expect_match(app_body, '"st_thresh_change"', fixed = TRUE)
  expect_match(app_body, '"st_fun"', fixed = TRUE)
  expect_match(app_body, '"trend_window"', fixed = TRUE)
  expect_match(app_body, '"trend_post_pct_thresh"', fixed = TRUE)
  expect_match(app_body, '"trend_alpha"', fixed = TRUE)
  expect_match(app_body, '"trend_deficit_tol"', fixed = TRUE)
  expect_match(app_body, '"trend_min_prop_below"', fixed = TRUE)
  expect_match(app_body, '"trend_avg_deficit_thresh"', fixed = TRUE)
})

test_that("ltm_app UI exposes coordinate source controls", {
  app_body <- ltm_app_ui_source()

  expect_match(app_body, '"coordinate_source_ui"', fixed = TRUE)
  expect_match(app_body, '"coordinate_source_status"', fixed = TRUE)
  expect_match(app_body, '"coord_source"', fixed = TRUE)
  expect_match(app_body, "ltm-coordinate-highlight", fixed = TRUE)
  expect_match(app_body, "ltm_resolve_coordinate_request", fixed = TRUE)
})

test_that("ltm_app UI exposes the workflow sidebar sections", {
  app_body <- ltm_app_ui_source()

  expect_match(app_body, '"target"', fixed = TRUE)
  expect_match(app_body, '"fetch"', fixed = TRUE)
  expect_match(app_body, '"preprocess"', fixed = TRUE)
  expect_match(app_body, '"breaks"', fixed = TRUE)
  expect_match(app_body, '"workflow_validator_settings"', fixed = TRUE)
  expect_match(app_body, '"continue_to_fetch"', fixed = TRUE)
  expect_match(app_body, '"run_preprocessing"', fixed = TRUE)
  expect_match(app_body, '"target_step_status"', fixed = TRUE)
  expect_match(app_body, '"breaks_step_summary"', fixed = TRUE)
})

test_that("invalid tree list path falls back without crashing app creation", {
  params <- LargeTreeMonitoring:::ltm_read_params(
    LargeTreeMonitoring:::ltm_default_params_path()
  )
  params$tree_list_file$tree_locs_file <- "missing-tree-list.csv"

  config_path <- tempfile(fileext = ".json")
  LargeTreeMonitoring:::ltm_write_params(params, config_path)

  startup <- LargeTreeMonitoring:::ltm_app_startup(config_path = config_path)
  expect_identical(startup$tree_state$choices, c("---"))
  expect_match(startup$tree_state$message, "Tree list file not found")
  expect_s3_class(
    LargeTreeMonitoring::ltm_app(config_path = config_path),
    "shiny.appobj"
  )
})

test_that("invalid tree list columns fall back without crashing app creation", {
  params <- LargeTreeMonitoring:::ltm_read_params(
    LargeTreeMonitoring:::ltm_default_params_path()
  )
  params$tree_list_file$tree_locs_file <- LargeTreeMonitoring:::ltm_default_csv_path()
  params$tree_list_file$tree_ids_col <- "missing_id"

  config_path <- tempfile(fileext = ".json")
  LargeTreeMonitoring:::ltm_write_params(params, config_path)

  startup <- LargeTreeMonitoring:::ltm_app_startup(config_path = config_path)
  expect_identical(startup$tree_state$choices, c("---"))
  expect_match(startup$tree_state$message, "missing configured columns")
  expect_s3_class(
    LargeTreeMonitoring::ltm_app(config_path = config_path),
    "shiny.appobj"
  )
})

test_that("cache path uses the package cache directory and round-trips", {
  cache_dir <- tempfile("ltm-cache-")
  dir.create(cache_dir, recursive = TRUE)
  old_cache_dir <- getOption("LargeTreeMonitoring.cache_dir")
  options(LargeTreeMonitoring.cache_dir = cache_dir)
  on.exit(options(LargeTreeMonitoring.cache_dir = old_cache_dir), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cache_dir <- LargeTreeMonitoring:::ltm_cache_dir()
  expect_true(dir.exists(cache_dir))

  file_name <- LargeTreeMonitoring:::ltm_cache_file_name(
    lat = 41.27998,
    lon = -8.2906,
    start_date = "2015-01-01",
    end_date = "2024-12-31",
    spi = "NDVI",
    proc_level = "L1C"
  )

  expect_identical(
    normalizePath(dirname(file_name), winslash = "/", mustWork = FALSE),
    normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  )

  dummy_data <- data.frame(
    ti = as.Date("2024-01-01") + 0:1,
    spi = c(0.5, 0.6)
  )

  on.exit(unlink(file_name), add = TRUE)

  written_file <- LargeTreeMonitoring:::ltm_save_to_cache(
    dummy_data,
    metadata_tags = list(
      lat = 41.27998,
      lon = -8.2906,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L1C",
      tree_id = NULL,
      buffer_radius_m = NULL
    )
  )

  expect_identical(written_file, file_name)
  expect_true(file.exists(file_name))
  expect_identical(
    LargeTreeMonitoring:::ltm_check_cache(
      lat = 41.27998,
      lon = -8.2906,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L1C"
    ),
    file_name
  )
})

test_that("cache lookup round-trips with tree IDs used by the Shiny app", {
  cache_dir <- tempfile("ltm-cache-")
  dir.create(cache_dir, recursive = TRUE)
  old_cache_dir <- getOption("LargeTreeMonitoring.cache_dir")
  options(LargeTreeMonitoring.cache_dir = cache_dir)
  on.exit(options(LargeTreeMonitoring.cache_dir = old_cache_dir), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  file_name <- LargeTreeMonitoring:::ltm_cache_file_name(
    lat = 41.27998,
    lon = -8.2906,
    start_date = "2015-01-01",
    end_date = "2024-12-31",
    spi = "NDVI",
    proc_level = "L1C",
    tree_id = "tree-001"
  )

  dummy_data <- data.frame(
    ti = as.Date("2024-01-01") + 0:1,
    spi = c(0.5, 0.6)
  )

  on.exit(unlink(file_name), add = TRUE)

  written_file <- LargeTreeMonitoring:::ltm_save_to_cache(
    dummy_data,
    metadata_tags = list(
      lat = 41.27998,
      lon = -8.2906,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L1C",
      tree_id = "tree-001",
      buffer_radius_m = NULL
    )
  )

  expect_identical(written_file, file_name)
  expect_identical(
    LargeTreeMonitoring:::ltm_check_cache(
      lat = 41.27998,
      lon = -8.2906,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L1C",
      tree_id = "tree-001"
    ),
    file_name
  )
})

test_that("tree-aware cache lookup falls back to older tree-less cache files", {
  cache_dir <- tempfile("ltm-cache-")
  dir.create(cache_dir, recursive = TRUE)
  old_cache_dir <- getOption("LargeTreeMonitoring.cache_dir")
  options(LargeTreeMonitoring.cache_dir = cache_dir)
  on.exit(options(LargeTreeMonitoring.cache_dir = old_cache_dir), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  lat <- 41.12345
  lon <- -8.54321

  tree_less_file <- LargeTreeMonitoring:::ltm_cache_file_name(
    lat = lat,
    lon = lon,
    start_date = "2015-01-01",
    end_date = "2024-12-31",
    spi = "NDVI",
    proc_level = "L2A"
  )

  dummy_data <- data.frame(
    ti = as.Date("2024-01-01") + 0:1,
    spi = c(0.5, 0.6)
  )

  on.exit(unlink(tree_less_file), add = TRUE)

  LargeTreeMonitoring:::ltm_save_to_cache(
    dummy_data,
    metadata_tags = list(
      lat = lat,
      lon = lon,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L2A",
      tree_id = NULL,
      buffer_radius_m = NULL
    )
  )

  expect_identical(
    LargeTreeMonitoring:::ltm_check_cache(
      lat = lat,
      lon = lon,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L2A",
      tree_id = "tree-001"
    ),
    tree_less_file
  )
})

test_that("buffer-aware cache lookup falls back to older buffer-less cache files", {
  cache_dir <- tempfile("ltm-cache-")
  dir.create(cache_dir, recursive = TRUE)
  old_cache_dir <- getOption("LargeTreeMonitoring.cache_dir")
  options(LargeTreeMonitoring.cache_dir = cache_dir)
  on.exit(options(LargeTreeMonitoring.cache_dir = old_cache_dir), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  lat <- 41.54321
  lon <- -8.12345

  tree_less_file <- LargeTreeMonitoring:::ltm_cache_file_name(
    lat = lat,
    lon = lon,
    start_date = "2015-01-01",
    end_date = "2024-12-31",
    spi = "NDVI",
    proc_level = "L2A",
    tree_id = "tree-001"
  )

  dummy_data <- data.frame(
    ti = as.Date("2024-01-01") + 0:1,
    spi = c(0.5, 0.6)
  )

  on.exit(unlink(tree_less_file), add = TRUE)

  LargeTreeMonitoring:::ltm_save_to_cache(
    dummy_data,
    metadata_tags = list(
      lat = lat,
      lon = lon,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L2A",
      tree_id = "tree-001",
      buffer_radius_m = NULL
    )
  )

  expect_identical(
    LargeTreeMonitoring:::ltm_check_cache(
      lat = lat,
      lon = lon,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L2A",
      tree_id = "tree-001",
      buffer_radius_m = 30
    ),
    tree_less_file
  )
})

test_that("plain cached data frames are upgraded into valid spidf objects", {
  cache_dir <- tempfile("ltm-cache-")
  dir.create(cache_dir, recursive = TRUE)
  old_cache_dir <- getOption("LargeTreeMonitoring.cache_dir")
  options(LargeTreeMonitoring.cache_dir = cache_dir)
  on.exit(options(LargeTreeMonitoring.cache_dir = old_cache_dir), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  cache_file <- LargeTreeMonitoring:::ltm_cache_file_name(
    lat = 41.27998,
    lon = -8.2906,
    start_date = "2015-01-01",
    end_date = "2024-12-31",
    spi = "NDVI",
    proc_level = "L1C"
  )

  legacy_df <- data.frame(
    ti = c("2024-01-01", "2024-01-02"),
    spi = c(0.5, 0.6)
  )

  on.exit(unlink(cache_file), add = TRUE)
  readr::write_rds(legacy_df, cache_file)

  upgraded <- LargeTreeMonitoring:::ltm_read_cached_spidf(
    cache_file,
    metadata_tags = list(
      lat = 41.27998,
      lon = -8.2906,
      start_date = "2015-01-01",
      end_date = "2024-12-31",
      spi = "NDVI",
      proc_level = "L1C",
      tree_id = NULL,
      buffer_radius_m = NULL
    )
  )

  expect_s3_class(upgraded, "spidf")
  expect_true(all(c("id", "ti", "masked_vals", "spi", "cloud_mask") %in% names(upgraded)))
  expect_identical(LargeTreeMonitoring:::get_tree_id(upgraded), NULL)
  expect_identical(LargeTreeMonitoring:::get_proc_level(upgraded), "L1C")
  expect_equal(upgraded$masked_vals, upgraded$spi)
})

test_that("GEE feature properties are converted to a plain data frame", {
  features <- list(
    list(
      id = "S2A_001",
      geometry = NULL,
      properties = list(
        "system:id" = "S2A_001",
        NDVI = 0.61,
        cloud_mask = 0
      )
    ),
    list(
      id = "S2A_002",
      geometry = NULL,
      properties = list(
        NDVI = 0.47,
        cloud_mask = 1
      )
    )
  )

  out <- LargeTreeMonitoring:::ltm_ee_features_to_df(features)

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("system.id", "NDVI", "cloud_mask"))
  expect_identical(out$system.id, c("S2A_001", "S2A_002"))
  expect_equal(out$NDVI, c(0.61, 0.47))
  expect_equal(out$cloud_mask, c(0, 1))
})
