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
