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

test_that("Percentile90 supports na.rm and returns a scalar numeric", {
  expect_equal(
    LargeTreeMonitoring:::Percentile90(c(1, 2, 3, 4, 5, NA), na.rm = TRUE),
    4.6
  )
})

test_that("validator settings helper maps Shiny selections to detector arguments", {
  validator_args <- LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
    lt_fun_label = "90% percentile",
    thresh_change = -10,
    st_window = 30,
    st_thresh_change = -12,
    st_fun_label = "Median",
    trend_window = 30,
    trend_post_pct_thresh = -8,
    trend_alpha = 0.1
  )

  expect_equal(validator_args$thresh_change, -10)
  expect_equal(validator_args$st_window, 30L)
  expect_equal(validator_args$st_thresh_change, -12)
  expect_equal(validator_args$trend_window, 30L)
  expect_equal(validator_args$trend_post_pct_thresh, -8)
  expect_equal(validator_args$trend_alpha, 0.1)
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
    thresh_change = -10,
    st_window = 0,
    st_thresh_change = -10,
    st_fun_label = "Median",
    trend_window = 0,
    trend_post_pct_thresh = -10,
    trend_alpha = 0.1
  )

  expect_null(validator_args$st_window)
  expect_null(validator_args$trend_window)
})

test_that("validator settings helper rejects unsupported thresholds", {
  expect_error(
    LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
      lt_fun_label = "Median",
      thresh_change = 5,
      st_window = 30,
      st_thresh_change = -10,
      st_fun_label = "Median",
      trend_window = 30,
      trend_post_pct_thresh = -10,
      trend_alpha = 0.1
    ),
    "Long-term percent change threshold"
  )

  expect_error(
    LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
      lt_fun_label = "Median",
      thresh_change = -10,
      st_window = 30,
      st_thresh_change = -10,
      st_fun_label = "Median",
      trend_window = 2,
      trend_post_pct_thresh = -10,
      trend_alpha = 0.1
    ),
    "Short-term trend window size"
  )
})

test_that("validator call labels are written back onto stored runs", {
  validator_args <- LargeTreeMonitoring:::ltm_collect_shiny_break_validator_args(
    lt_fun_label = "90% percentile",
    thresh_change = -10,
    st_window = 30,
    st_thresh_change = -10,
    st_fun_label = "Median",
    trend_window = 30,
    trend_post_pct_thresh = -10,
    trend_alpha = 0.1
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

test_that("ltm_app UI exposes the new validator controls", {
  app_body <- paste(
    deparse(body(LargeTreeMonitoring::ltm_app), width.cutoff = 500L),
    collapse = "\n"
  )

  expect_match(app_body, '"st_window"', fixed = TRUE)
  expect_match(app_body, '"st_thresh_change"', fixed = TRUE)
  expect_match(app_body, '"st_fun"', fixed = TRUE)
  expect_match(app_body, '"trend_window"', fixed = TRUE)
  expect_match(app_body, '"trend_post_pct_thresh"', fixed = TRUE)
  expect_match(app_body, '"trend_alpha"', fixed = TRUE)
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
