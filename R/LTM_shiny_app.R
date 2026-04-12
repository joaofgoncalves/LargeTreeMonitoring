
#' Render Unicode code points as HTML entities
#'
#' @param ... Hexadecimal Unicode code-point strings.
#' @return An `htmltools::HTML` object containing the encoded entities.
#' @keywords internal
#' @noRd
emoji_html <- function(...) {
  cps <- toupper(unlist(list(...), use.names = FALSE))
  ok <- grepl("^[0-9A-F]+$", cps)
  if (!all(ok)) {
    stop("Code points must be hex strings like '1F680' or '2764'.", call. = FALSE)
  }
  htmltools::HTML(paste0("&#x", cps, ";", collapse = ""))
}

#' Locate the packaged default Shiny parameter file
#'
#' @return Character scalar path to `params-default.json` in `inst/extdata`.
#' @keywords internal
#' @noRd
ltm_default_params_path <- function() {
  system.file(
    "extdata", "params-default.json",
    package = "LargeTreeMonitoring",
    mustWork = TRUE
  )
}

#' Locate the packaged sample tree-list file
#'
#' @return Character scalar path to `_SAMPLE_TREE_LIST_.csv` in `inst/extdata`.
#' @keywords internal
#' @noRd
ltm_default_csv_path <- function() {
  system.file(
    "extdata", "_SAMPLE_TREE_LIST_.csv",
    package = "LargeTreeMonitoring",
    mustWork = TRUE
  )
}

#' Get the user configuration directory
#'
#' Returns the platform-specific user configuration directory used by
#' LargeTreeMonitoring and creates it if needed. The location is resolved with
#' [tools::R_user_dir()] using `which = "config"`.
#'
#' @return Normalized character scalar path to the package configuration
#'   directory.
#' @family cache and configuration helpers
#' @export
ltm_config_dir <- function() {
  path <- tools::R_user_dir("LargeTreeMonitoring", which = "config")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Get the Shiny parameter file path
#'
#' @return Character scalar path to the user-level `params.json` file.
#' @keywords internal
#' @noRd
ltm_params_path <- function() {
  file.path(ltm_config_dir(), "params.json")
}

#' Initialize the Shiny parameter file
#'
#' @param overwrite Logical scalar. If `TRUE`, replace an existing user
#'   parameter file with the packaged default.
#' @return Character scalar path to the initialized parameter file.
#' @keywords internal
#' @noRd
ltm_init_params <- function(overwrite = FALSE) {
  target <- ltm_params_path()

  if (overwrite || !file.exists(target)) {
    file.copy(ltm_default_params_path(), target, overwrite = overwrite)
  }

  target
}

#' Read Shiny parameters from JSON
#'
#' @param path Character scalar path to a JSON parameter file.
#' @return A list containing the parsed parameter values.
#' @keywords internal
#' @noRd
ltm_read_params <- function(path = ltm_params_path()) {
  jsonlite::read_json(path, simplifyVector = TRUE)
}

#' Write Shiny parameters to JSON
#'
#' @param params List of parameter values to serialize.
#' @param path Character scalar destination path.
#' @return Invisibly returns `path`.
#' @keywords internal
#' @noRd
ltm_write_params <- function(params, path = ltm_params_path()) {
  jsonlite::write_json(
    params,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  invisible(path)
}

#' Test whether a directory can be created and written
#'
#' @param path Character scalar directory path.
#' @return Logical scalar indicating whether `path` exists and is writable.
#' @keywords internal
#' @noRd
ltm_is_writable_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.exists(path) && file.access(path, mode = 2) == 0
}

#' Get the cache directory
#'
#' Resolves the writable cache directory used by LargeTreeMonitoring. Candidate
#' locations are checked in order: option `LargeTreeMonitoring.cache_dir`,
#' environment variable `LTM_CACHE_DIR`, the platform-specific R user cache
#' directory, and the package's legacy `ltm_cache` directory under the current
#' working directory.
#'
#' @return Normalized character scalar path to a writable cache directory.
#' @family cache and configuration helpers
#' @export
ltm_cache_dir <- function() {
  option_path <- getOption("LargeTreeMonitoring.cache_dir")
  env_path <- Sys.getenv("LTM_CACHE_DIR", unset = "")

  candidates <- Filter(
    ltm_has_path_value,
    c(
      option_path,
      env_path,
      tools::R_user_dir("LargeTreeMonitoring", which = "cache"),
      ltm_legacy_cache_dir()
    )
  )

  for (candidate in candidates) {
    if (ltm_is_writable_dir(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }

  stop("Could not create a writable cache directory for LargeTreeMonitoring.")
}

#' Shiny resource path prefix
#'
#' Internal prefix used when registering packaged `www` assets with Shiny.
#'
#' @keywords internal
#' @noRd
ltm_app_resource_prefix <- "ltm-www"

#' Register packaged Shiny web resources
#'
#' @return Invisibly returns the path to the installed `www` resource
#'   directory.
#' @keywords internal
#' @noRd
ltm_register_app_resources <- function() {
  resource_dir <- system.file("www", package = "LargeTreeMonitoring")

  if (!nzchar(resource_dir)) {
    stop("Could not locate installed Shiny assets for LargeTreeMonitoring.")
  }

  resource_paths <- shiny::resourcePaths()
  current_path <- unname(resource_paths[ltm_app_resource_prefix])
  if (length(current_path) == 0L || !identical(current_path, resource_dir)) {
    shiny::addResourcePath(ltm_app_resource_prefix, resource_dir)
  }

  invisible(resource_dir)
}

#' Build a Shiny resource URL for a packaged asset
#'
#' @param file_name Character scalar file name under the packaged `www`
#'   directory.
#' @return Character scalar resource URL relative to the Shiny app.
#' @keywords internal
#' @noRd
ltm_app_asset <- function(file_name) {
  paste0(ltm_app_resource_prefix, "/", file_name)
}

#' Check whether a path-like value is usable
#'
#' @param path Candidate path value.
#' @return Logical scalar indicating whether `path` is a non-empty scalar.
#' @keywords internal
#' @noRd
ltm_has_path_value <- function(path) {
  !is.null(path) && length(path) == 1L && !is.na(path) && nzchar(path)
}

#' Normalize the selected tree identifier from Shiny input
#'
#' @param location_id Selected location identifier from the app.
#' @return Character scalar tree identifier, or `NULL` for blank and placeholder
#'   selections.
#' @keywords internal
#' @noRd
ltm_selected_tree_id <- function(location_id) {
  if (!ltm_has_path_value(location_id) || identical(location_id, "---")) {
    return(NULL)
  }

  as.character(location_id)
}

#' Label the manual coordinate source
#'
#' @return Character scalar used in Shiny coordinate-source choices.
#' @keywords internal
#' @noRd
ltm_coord_source_manual <- function() {
  "manual"
}

#' Label the tree-list coordinate source
#'
#' @return Character scalar used in Shiny coordinate-source choices.
#' @keywords internal
#' @noRd
ltm_coord_source_tree_list <- function() {
  "tree_list"
}

#' Build coordinate-source choices for the Shiny app
#'
#' @param tree_list_available Logical scalar indicating whether a usable tree
#'   list is loaded.
#' @return Named character vector of coordinate-source labels.
#' @keywords internal
#' @noRd
ltm_coord_source_choices <- function(tree_list_available = TRUE) {
  choices <- c("Manual / map coordinates" = ltm_coord_source_manual())

  if (isTRUE(tree_list_available)) {
    choices <- c(choices, "Location ID list" = ltm_coord_source_tree_list())
  }

  choices
}

#' Check whether a tree-list state contains selectable trees
#'
#' @param tree_state List returned by the tree-list loading helpers.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
ltm_tree_list_available <- function(tree_state) {
  !is.null(tree_state$data) && is.null(tree_state$message)
}

#' Validate a latitude and longitude pair
#'
#' @param latitude Numeric latitude candidate.
#' @param longitude Numeric longitude candidate.
#' @return Invisibly returns `TRUE` when both values are finite and within
#'   geographic coordinate bounds; otherwise raises an error.
#' @keywords internal
#' @noRd
ltm_validate_coordinate_pair <- function(latitude, longitude) {
  lat <- suppressWarnings(as.numeric(latitude))
  lon <- suppressWarnings(as.numeric(longitude))

  if (
    length(lat) != 1L || length(lon) != 1L ||
      is.na(lat) || is.na(lon) ||
      !is.finite(lat) || !is.finite(lon)
  ) {
    stop("Latitude and longitude must be finite numeric values.", call. = FALSE)
  }

  list(lat = lat, lon = lon)
}

#' Extract a tree location from loaded tree-list state
#'
#' @param tree_state List containing tree-list data and status metadata.
#' @param tree_id Character scalar tree identifier.
#' @return Named list containing tree identifier and coordinates, or `NULL`
#'   when the identifier is absent or unavailable.
#' @keywords internal
#' @noRd
ltm_get_tree_location <- function(tree_state, tree_id) {
  selected_tree_id <- ltm_selected_tree_id(tree_id)

  if (is.null(selected_tree_id)) {
    return(NULL)
  }

  if (is.null(tree_state) || is.null(tree_state$data)) {
    stop("Location list is unavailable.", call. = FALSE)
  }

  required_cols <- list(tree_state$tree_ids_col, tree_state$lat_col, tree_state$lon_col)
  if (any(!vapply(required_cols, ltm_has_path_value, logical(1)))) {
    stop("Location list columns are not configured correctly.", call. = FALSE)
  }
  required_cols <- unlist(required_cols, use.names = FALSE)

  missing_cols <- setdiff(required_cols, names(tree_state$data))
  if (length(missing_cols) > 0L) {
    stop(
      "Location list is missing configured columns: ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  loc_row <- tree_state$data[
    as.character(tree_state$data[[tree_state$tree_ids_col]]) == selected_tree_id,
    ,
    drop = FALSE
  ]

  if (nrow(loc_row) == 0L) {
    stop(
      "Selected Location ID not found in list: ",
      selected_tree_id,
      call. = FALSE
    )
  }

  if (nrow(loc_row) > 1L) {
    stop(
      "Selected Location ID is ambiguous in list: ",
      selected_tree_id,
      call. = FALSE
    )
  }

  coords <- ltm_validate_coordinate_pair(
    loc_row[[tree_state$lat_col]][1],
    loc_row[[tree_state$lon_col]][1]
  )

  list(
    lat = coords$lat,
    lon = coords$lon,
    tree_id = selected_tree_id
  )
}

#' Resolve a Shiny coordinate request
#'
#' @param coord_source Selected coordinate source.
#' @param location_id Selected tree identifier.
#' @param latitude Manual latitude value.
#' @param longitude Manual longitude value.
#' @param tree_state Loaded tree-list state.
#' @return A list with `tree_id`, `latitude`, `longitude`, and `source`.
#' @keywords internal
#' @noRd
ltm_resolve_coordinate_request <- function(
    coord_source,
    location_id,
    latitude,
    longitude,
    tree_state
) {
  if (!ltm_has_path_value(coord_source)) {
    coord_source <- ltm_coord_source_manual()
  }

  if (identical(coord_source, ltm_coord_source_tree_list())) {
    tree_location <- ltm_get_tree_location(tree_state, location_id)

    if (is.null(tree_location)) {
      stop(
        "Select a Location ID before fetching data from the Location ID list.",
        call. = FALSE
      )
    }

    tree_location$source <- ltm_coord_source_tree_list()
    tree_location$source_label <- "Location ID list"
    tree_location$status_label <- paste0("Location ID ", tree_location$tree_id)

    return(tree_location)
  }

  coords <- ltm_validate_coordinate_pair(latitude, longitude)

  list(
    lat = coords$lat,
    lon = coords$lon,
    tree_id = NULL,
    source = ltm_coord_source_manual(),
    source_label = "Manual / map coordinates",
    status_label = "manual/map coordinates"
  )
}

#' List aggregation functions available in Shiny controls
#'
#' @return Named character vector mapping user-facing labels to function labels.
#' @keywords internal
#' @noRd
ltm_validator_fun_choices <- function() {
  c("Median", "90% percentile")
}

#' Resolve a validator aggregation function from a label
#'
#' @param label Character scalar selected in Shiny controls.
#' @return Function object used for break validation.
#' @keywords internal
#' @noRd
ltm_get_validator_fun <- function(label) {
  if (!ltm_has_path_value(label)) {
    stop("A validator aggregation function must be selected.")
  }

  switch(
    as.character(label),
    "Median" = stats::median,
    "90% percentile" = Percentile90,
    stop("Unsupported validator aggregation function: ", label)
  )
}

#' Resolve the call label for a validator aggregation function
#'
#' @param label Character scalar selected in Shiny controls.
#' @return Character scalar suitable for inclusion in a call expression.
#' @keywords internal
#' @noRd
ltm_get_validator_fun_call <- function(label) {
  if (!ltm_has_path_value(label)) {
    stop("A validator aggregation function must be selected.")
  }

  switch(
    as.character(label),
    "Median" = quote(stats::median),
    "90% percentile" = quote(Percentile90),
    stop("Unsupported validator aggregation function: ", label)
  )
}

#' Parse and validate numeric Shiny validator input
#'
#' @param value Candidate numeric value.
#' @param field Character scalar field name used in error messages.
#' @param min_value Optional numeric lower bound.
#' @param max_value Optional numeric upper bound.
#' @return Numeric scalar.
#' @keywords internal
#' @noRd
ltm_parse_validator_numeric <- function(value, field, min_value = NULL, max_value = NULL) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    stop(field, " must be provided.")
  }

  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    stop(field, " must be numeric.")
  }

  if (!is.null(min_value) && value < min_value) {
    stop(field, " must be greater than or equal to ", min_value, ".")
  }

  if (!is.null(max_value) && value > max_value) {
    stop(field, " must be less than or equal to ", max_value, ".")
  }

  value
}

#' Parse an optional validator window from Shiny input
#'
#' @param value Candidate value; blank values disable the window.
#' @param field Character scalar field name used in error messages.
#' @param min_value Integer lower bound for non-empty values.
#' @return Integer scalar window size, or `NULL`.
#' @keywords internal
#' @noRd
ltm_parse_optional_validator_window <- function(value, field, min_value = 1L) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return(NULL)
  }

  value <- suppressWarnings(as.integer(value))
  if (!is.finite(value)) {
    stop(field, " must be a whole number.")
  }

  if (value <= 0L) {
    return(NULL)
  }

  if (value < min_value) {
    stop(field, " must be at least ", min_value, ", or 0 to disable it.")
  }

  value
}

#' Collect break-validator arguments from Shiny inputs
#'
#' @param lt_fun_label Long-term aggregation function label.
#' @param lt_window Long-term window input.
#' @param lt_thresh_change Long-term change threshold input.
#' @param st_window Short-term window input.
#' @param st_thresh_change Short-term change threshold input.
#' @param st_fun_label Short-term aggregation function label.
#' @param trend_window Trend window input.
#' @param trend_post_pct_thresh Trend slope threshold input.
#' @param trend_alpha Trend-validator alpha input.
#' @param trend_deficit_tol Deficit-tolerance input.
#' @param trend_min_prop_below Minimum post-break proportion input.
#' @param trend_avg_deficit_thresh Average deficit threshold input.
#' @return List of normalized arguments and display labels.
#' @keywords internal
#' @noRd
ltm_collect_shiny_break_validator_args <- function(
    lt_fun_label,
    lt_window,
    lt_thresh_change,
    st_window,
    st_thresh_change,
    st_fun_label,
    trend_window,
    trend_post_pct_thresh,
    trend_alpha,
    trend_deficit_tol = 0.05,
    trend_min_prop_below = 0.6,
    trend_avg_deficit_thresh = -1.5
) {
  lt_fun_call <- ltm_get_validator_fun_call(lt_fun_label)
  st_fun_call <- ltm_get_validator_fun_call(st_fun_label)

  out <- list(
    lt_window = ltm_parse_optional_validator_window(
      lt_window,
      field = "Long-term window size",
      min_value = 1L
    ),
    lt_thresh_change = ltm_parse_validator_numeric(
      lt_thresh_change,
      field = "Long-term percent change threshold",
      max_value = 0
    ),
    lt_fun = ltm_get_validator_fun(lt_fun_label),
    st_window = ltm_parse_optional_validator_window(
      st_window,
      field = "Short-term window size",
      min_value = 1L
    ),
    st_thresh_change = ltm_parse_validator_numeric(
      st_thresh_change,
      field = "Short-term percent change threshold",
      max_value = 0
    ),
    st_fun = ltm_get_validator_fun(st_fun_label),
    trend_window = ltm_parse_optional_validator_window(
      trend_window,
      field = "Short-term trend window size",
      min_value = 3L
    ),
    trend_post_pct_thresh = ltm_parse_validator_numeric(
      trend_post_pct_thresh,
      field = "Short-term trend post-break percent threshold",
      max_value = 0
    ),
    trend_alpha = ltm_parse_validator_numeric(
      trend_alpha,
      field = "Short-term trend alpha",
      min_value = 0,
      max_value = 1
    ),
    trend_deficit_tol = ltm_parse_validator_numeric(
      trend_deficit_tol,
      field = "Short-term trend deficit tolerance",
      min_value = 0,
      max_value = 1
    ),
    trend_min_prop_below = ltm_parse_validator_numeric(
      trend_min_prop_below,
      field = "Short-term trend minimum proportion below baseline",
      min_value = 0,
      max_value = 1
    ),
    trend_avg_deficit_thresh = ltm_parse_validator_numeric(
      trend_avg_deficit_thresh,
      field = "Short-term trend average deficit threshold",
      max_value = 0
    )
  )

  attr(out, "call_labels") <- list(
    lt_fun = lt_fun_call,
    st_fun = st_fun_call
  )

  out
}

#' Attach validator call labels to a run object
#'
#' @param run_obj A `ts_breaks_run` object.
#' @param validator_args List returned by
#'   `ltm_collect_shiny_break_validator_args()`.
#' @return The modified `run_obj`.
#' @keywords internal
#' @noRd
ltm_apply_validator_call_labels <- function(run_obj, validator_args) {
  if (!inherits(run_obj, "ts_breaks_run") || is.null(run_obj$call)) {
    return(run_obj)
  }

  call_labels <- attr(validator_args, "call_labels", exact = TRUE)
  if (is.null(call_labels)) {
    return(run_obj)
  }

  if (!is.null(call_labels$lt_fun)) {
    run_obj$call$lt_fun <- call_labels$lt_fun
  }

  if (!is.null(call_labels$st_fun)) {
    run_obj$call$st_fun <- call_labels$st_fun
  }

  run_obj
}

#' Check whether a path is absolute
#'
#' @param path Character scalar path.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
ltm_is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", path)
}

#' Build an empty tree-list state
#'
#' @param message Optional character scalar status message.
#' @return List representing an unavailable tree-list state.
#' @keywords internal
#' @noRd
ltm_empty_tree_state <- function(message = NULL) {
  list(
    data = NULL,
    path = NULL,
    tree_ids_col = NULL,
    lat_col = NULL,
    lon_col = NULL,
    choices = c("---"),
    message = message
  )
}

#' Load tree-list state for the Shiny app
#'
#' @param params Parsed Shiny parameter list.
#' @param config_path Optional parameter-file path used to resolve relative tree
#'   list paths.
#' @return List containing load status, choices, data, and messages.
#' @keywords internal
#' @noRd
ltm_load_tree_list_state <- function(params, config_path = NULL) {
  tree_cfg <- params$tree_list_file

  if (is.null(tree_cfg)) {
    return(
      ltm_empty_tree_state(
        "Tree list configuration is missing. Starting without predefined locations."
      )
    )
  }

  tree_list_path <- tree_cfg$tree_locs_file
  if (!ltm_has_path_value(tree_list_path)) {
    return(ltm_empty_tree_state())
  }

  if (identical(tree_list_path, "_SAMPLE_TREE_LIST_.csv")) {
    tree_list_path <- ltm_default_csv_path()
  } else if (!ltm_is_absolute_path(tree_list_path) && !is.null(config_path)) {
    tree_list_path <- file.path(dirname(config_path), tree_list_path)
  }

  tree_ids_col <- tree_cfg$tree_ids_col
  tree_lat_col <- tree_cfg$lat_col
  tree_lon_col <- tree_cfg$lon_col
  required_cols <- c(tree_ids_col, tree_lat_col, tree_lon_col)

  if (any(!vapply(required_cols, ltm_has_path_value, logical(1)))) {
    return(
      ltm_empty_tree_state(
        "Tree list columns are not configured correctly. Starting without predefined locations."
      )
    )
  }

  if (!file.exists(tree_list_path)) {
    return(
      ltm_empty_tree_state(
        paste0(
          "Tree list file not found: ",
          normalizePath(tree_list_path, mustWork = FALSE),
          ". Starting without predefined locations."
        )
      )
    )
  }

  tree_list <- tryCatch(
    readr::read_csv(tree_list_path, show_col_types = FALSE),
    error = function(error) {
      error
    }
  )

  if (inherits(tree_list, "error")) {
    return(
      ltm_empty_tree_state(
        paste0(
          "Could not read tree list file: ",
          tree_list$message,
          ". Starting without predefined locations."
        )
      )
    )
  }

  missing_cols <- setdiff(required_cols, names(tree_list))
  if (length(missing_cols) > 0L) {
    return(
      ltm_empty_tree_state(
        paste0(
          "Tree list file is missing configured columns: ",
          paste(missing_cols, collapse = ", "),
          ". Starting without predefined locations."
        )
      )
    )
  }

  tree_ids <- sort(unique(as.character(tree_list[[tree_ids_col]])))
  tree_ids <- tree_ids[!is.na(tree_ids) & nzchar(tree_ids)]

  list(
    data = tree_list,
    path = tree_list_path,
    tree_ids_col = tree_ids_col,
    lat_col = tree_lat_col,
    lon_col = tree_lon_col,
    choices = c("---", tree_ids),
    message = NULL
  )
}

#' Update Shiny tree-selection controls
#'
#' @param session Shiny session object.
#' @param tree_state Loaded tree-list state.
#' @param selected Character scalar currently selected tree identifier.
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
ltm_update_tree_choices <- function(session, tree_state, selected = "---") {
  shiny::updateSelectInput(
    session = session,
    inputId = "location_id",
    choices = tree_state$choices,
    selected = selected
  )
}

#' Initialize app configuration and tree-list state
#'
#' @param config_path Optional path to a JSON parameter file.
#' @return List containing parameters, config path, and tree-list state.
#' @keywords internal
#' @noRd
ltm_app_startup <- function(config_path = NULL) {
  if (is.null(config_path)) {
    config_path <- ltm_init_params()
  } else {
    config_path <- normalizePath(config_path, mustWork = FALSE)
    if (!file.exists(config_path)) {
      stop("Config file does not exist: ", config_path)
    }
  }

  ltm_cache_dir()

  params <- ltm_read_params(config_path)
  tree_state <- ltm_load_tree_list_state(params, config_path = config_path)

  list(
    config_path = config_path,
    params = params,
    tree_state = tree_state
  )
}

#' Render a Shiny control grid
#'
#' @param ... UI elements placed inside the grid.
#' @return Shiny UI tag object.
#' @keywords internal
#' @noRd
ltm_control_grid <- function(...) {
  tags$div(class = "ltm-control-grid", ...)
}

#' Render Shiny help text
#'
#' @param text Character scalar help text.
#' @return Shiny UI tag object.
#' @keywords internal
#' @noRd
ltm_help_text <- function(text) {
  tags$p(class = "ltm-help", text)
}

#' Render a workflow status badge
#'
#' @param label Character scalar status label.
#' @return Shiny UI tag object.
#' @keywords internal
#' @noRd
ltm_workflow_status_badge <- function(label) {
  status_key <- tolower(gsub("[^[:alnum:]]+", "-", label))
  status_key <- gsub("(^-|-$)", "", status_key)

  tags$span(
    class = paste("ltm-status-badge", paste0("ltm-status-", status_key)),
    label
  )
}

#' Render one workflow step for the Shiny app
#'
#' @param step_id Character scalar step identifier.
#' @param step_number Step number or label rendered in the step header.
#' @param title Character scalar step title.
#' @param status_output_id Shiny output id used for the status badge.
#' @param summary_output_id Shiny output id used for the step summary.
#' @param open Logical scalar indicating whether the step starts expanded.
#' @param ... Shiny UI elements placed inside the step body.
#' @return Shiny UI tag object.
#' @keywords internal
#' @noRd
ltm_workflow_step_ui <- function(
    step_id,
    step_number,
    title,
    status_output_id,
    summary_output_id,
    open = FALSE,
    ...
) {
  panel_id <- paste0("workflow_step_", step_id)
  body_id <- paste0(panel_id, "_body")
  body_class <- paste(
    "panel-collapse collapse",
    if (isTRUE(open)) "in" else ""
  )

  tags$section(
    id = panel_id,
    class = "ltm-workflow-step panel panel-default",
    tags$div(
      class = "ltm-step-heading panel-heading",
      tags$a(
        class = "ltm-step-toggle",
        href = paste0("#", body_id),
        `data-toggle` = "collapse",
        `aria-expanded` = if (isTRUE(open)) "true" else "false",
        tags$span(class = "ltm-step-number", step_number),
        tags$span(class = "ltm-step-title", title),
        shiny::uiOutput(status_output_id, inline = TRUE),
        tags$span(
          class = "ltm-step-summary",
          shiny::uiOutput(summary_output_id, inline = TRUE)
        )
      )
    ),
    tags$div(
      id = body_id,
      class = body_class,
      tags$div(class = "ltm-step-body panel-body", ...)
    )
  )
}

#' Render break-validator controls for the Shiny app
#'
#' @return Shiny UI tag object containing validator parameter controls.
#' @keywords internal
#' @noRd
ltm_validator_controls_ui <- function() {
  tagList(
    tags$div(class = "ltm-subsection-title", "Long-term validator"),
    ltm_control_grid(
      shiny::numericInput(
        "lt_window",
        "Long-term window (observations per side; 0 = full series)",
        value = 0,
        min = 0,
        max = 365,
        step = 1
      ),
      shiny::numericInput(
        "lt_thresh_change",
        "Long-term % change threshold",
        value = -10,
        max = 0
      )
    ),
    shiny::radioButtons(
      inputId = "lt_fun",
      label = "Long-term aggregation function",
      choices = ltm_validator_fun_choices(),
      selected = "Median"
    ),

    tags$div(class = "ltm-subsection-title", "Short-term validator"),
    ltm_control_grid(
      shiny::numericInput(
        "st_window",
        "Short-term window (observations per side; 0 disables)",
        value = 30,
        min = 0,
        max = 365,
        step = 1
      ),
      shiny::numericInput(
        "st_thresh_change",
        "Short-term % change threshold",
        value = -10,
        max = 0
      )
    ),
    shiny::radioButtons(
      inputId = "st_fun",
      label = "Short-term aggregation function",
      choices = ltm_validator_fun_choices(),
      selected = "Median"
    ),

    tags$div(class = "ltm-subsection-title", "Short-term trend validator"),
    ltm_control_grid(
      shiny::numericInput(
        "trend_window",
        "Trend window (observations per side; 0 disables)",
        value = 30,
        min = 0,
        max = 365,
        step = 1
      ),
      shiny::numericInput(
        "trend_post_pct_thresh",
        "Trend post-break % threshold",
        value = -10,
        max = 0
      ),
      shiny::numericInput(
        "trend_alpha",
        "Trend alpha",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.01
      ),
      shiny::numericInput(
        "trend_deficit_tol",
        "Trend deficit tolerance",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.01
      ),
      shiny::numericInput(
        "trend_min_prop_below",
        "Trend minimum proportion below baseline",
        value = 0.6,
        min = 0,
        max = 1,
        step = 0.05
      ),
      shiny::numericInput(
        "trend_avg_deficit_thresh",
        "Trend average deficit threshold (%)",
        value = -1.5,
        max = 0,
        step = 0.1
      )
    )
  )
}

#' Build a cache key for the current Shiny request
#'
#' @param coord_request Resolved coordinate request returned by
#'   `ltm_resolve_coordinate_request()`.
#' @param start_date Requested start date.
#' @param end_date Requested end date.
#' @param spi Spectral index label.
#' @param proc_level Sentinel-2 processing-level label.
#' @return List representing the request parameters, or `NULL` when the request
#'   is incomplete.
#' @keywords internal
#' @noRd
ltm_current_request_key <- function(
    coord_request,
    start_date,
    end_date,
    spi,
    proc_level
) {
  if (is.null(coord_request) || inherits(coord_request, "error")) {
    return(NULL)
  }

  if (
    is.null(start_date) || is.null(end_date) ||
      length(start_date) != 1L || length(end_date) != 1L ||
      is.na(start_date) || is.na(end_date) ||
      !ltm_has_path_value(spi) || !ltm_has_path_value(proc_level)
  ) {
    return(NULL)
  }

  list(
    lat = round(as.numeric(coord_request$lat), 6),
    lon = round(as.numeric(coord_request$lon), 6),
    tree_id = if (is.null(coord_request$tree_id)) "" else as.character(coord_request$tree_id),
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    spi = as.character(spi),
    proc_level = as.character(proc_level)
  )
}

#' Check whether an `spidf` contains required columns
#'
#' @param spidf Candidate `spidf` object.
#' @param columns Character vector of required column names.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
ltm_spidf_has_columns <- function(spidf, columns) {
  !is.null(spidf) && all(columns %in% names(spidf))
}

#' Derive Shiny workflow readiness flags
#'
#' @param target_request Current resolved target-location request.
#' @param current_request_key Current request key.
#' @param spidf Current `spidf` object, if available.
#' @param fetched_request_key Request key used for currently loaded data.
#' @param break_results Current break-results object, if available.
#' @return List of logical workflow flags.
#' @keywords internal
#' @noRd
ltm_workflow_state <- function(
    target_request = NULL,
    current_request_key = NULL,
    spidf = NULL,
    fetched_request_key = NULL,
    break_results = NULL
) {
  target_ready <- !is.null(target_request) &&
    !inherits(target_request, "error") &&
    !is.null(current_request_key)

  has_data <- !is.null(spidf)
  data_ready <- target_ready &&
    has_data &&
    !is.null(fetched_request_key) &&
    identical(current_request_key, fetched_request_key)
  data_stale <- has_data && !isTRUE(data_ready)

  regularized <- has_data &&
    isTRUE(tryCatch(is_regularized(spidf), error = function(error) FALSE))
  has_moving_window <- ltm_spidf_has_columns(spidf, "spi_mov_wind")
  has_whittaker <- ltm_spidf_has_columns(spidf, c("spi_smooth", "spi_mov_smooth"))
  preprocess_ready <- data_ready &&
    regularized &&
    has_moving_window &&
    has_whittaker
  breaks_ready <- preprocess_ready && !is.null(break_results)

  list(
    target_ready = target_ready,
    data_ready = data_ready,
    preprocess_ready = preprocess_ready,
    breaks_ready = breaks_ready,
    data_stale = data_stale,
    has_data = has_data,
    regularized = regularized,
    has_moving_window = has_moving_window,
    has_whittaker = has_whittaker
  )
}


#' Create the LargeTreeMonitoring Shiny app
#'
#' Creates the packaged Shiny application for analysing satellite-image time
#' series and breakpoints.
#'
#' @param config_path Optional path to a JSON parameter file. When omitted, the
#'   package uses the user-specific configuration file in the standard R user
#'   config directory, creating it from the default template when needed.
#'
#' @return A `shiny.appobj` that can be launched with [shiny::runApp()].
#' @export
#'
ltm_app <- function(config_path = NULL) {

  startup <- ltm_app_startup(config_path)
  config_path <- startup$config_path
  tree_ids <- startup$tree_state$choices

  ltm_register_app_resources()


  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),

    tags$head(
      tags$style(shiny::HTML("
      * {
        box-sizing: border-box;
      }
      body {
        background: #FFFFFF;
        color: #202a27;
        padding-top: 76px;
      }
      .ltm-banner {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        background-color: #1f5a4f;
        color: #ffffff;
        padding: 8px 20px;
        z-index: 1000;
        border-bottom: 1px solid #17473f;
      }
      .ltm-banner h2 {
        margin: 3px;
        padding: 3px;
        font-size: 24px;
        color: #ffffff;
      }
      .ltm-app-shell {
        display: flex;
        align-items: flex-start;
        gap: 18px;
        padding: 16px 20px 24px;
      }
      .ltm-sidebar {
        flex: 0 0 380px;
        max-width: 430px;
        max-height: calc(100vh - 96px);
        overflow-y: auto;
        position: sticky;
        top: 88px;
        background-color: #ffffff;
        border: 1px solid #cfd8d5;
        border-radius: 8px;
        padding: 12px;
      }
      .ltm-sidebar-header h3 {
        font-size: 18px;
        margin: 0 0 4px;
      }
      .ltm-sidebar-header p {
        color: #52635e;
        font-size: 12px;
        line-height: 1.35;
        margin: 0 0 10px;
      }
      .main-panel {
        flex: 1 1 auto;
        min-width: 0;
        padding: 0 0 20px;
      }
      .form-group,
      .shiny-input-container {
        margin-bottom: 7px;
        font-size: 12px;
        width: 100%;
      }
      .shiny-input-label,
      .control-label,
      .checkbox label,
      .radio label {
        color: #20342f !important;
        font-weight: 500;
      }
      .form-control,
      .selectize-input {
        background-color: #f7fbfa !important;
        color: #202a27 !important;
        border: 1px solid #aebfba;
        border-radius: 6px;
        font-size: 12px;
        min-height: 31px;
      }
      .form-control:focus,
      .selectize-input.focus,
      .selectize-input:focus {
        border-color: #2d7f6f;
        box-shadow: 0 0 0 2px rgba(45, 127, 111, 0.16);
      }
      .selectize-dropdown-content {
        background-color: #ffffff;
        color: #202a27;
      }
      .btn {
        border: none;
        border-radius: 6px;
        background-color: #2d7f6f;
        color: #ffffff;
        font-size: 12px;
        font-weight: 700;
        margin-top: 3px;
        padding: 6px 10px;
      }
      .btn:hover,
      .btn:focus {
        background-color: #23695c;
        color: #ffffff;
      }
      .btn:disabled,
      .btn.disabled {
        background-color: #aab7b3 !important;
        color: #eef3f1 !important;
        cursor: not-allowed;
      }
      .ltm-primary-action {
        width: 100%;
      }
      .ltm-inline-actions {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        margin-top: 6px;
      }
      .ltm-workflow-step.panel,
      .ltm-validator-accordion.panel {
        border-color: #d8e0dd;
        border-radius: 8px;
        box-shadow: none;
        margin-bottom: 8px;
        overflow: hidden;
      }
      .ltm-step-heading.panel-heading,
      .ltm-validator-heading.panel-heading {
        background-color: #eef5f2;
        border-color: #d8e0dd;
        padding: 0;
      }
      .ltm-step-toggle,
      .ltm-validator-toggle {
        color: #20342f;
        display: grid;
        grid-template-columns: auto 1fr auto;
        gap: 6px;
        padding: 9px 10px;
        text-decoration: none !important;
      }
      .ltm-step-toggle:hover,
      .ltm-step-toggle:focus,
      .ltm-validator-toggle:hover,
      .ltm-validator-toggle:focus {
        color: #20342f;
        text-decoration: none;
      }
      .ltm-step-number {
        align-items: center;
        background-color: #20342f;
        border-radius: 50%;
        color: #ffffff;
        display: inline-flex;
        font-size: 11px;
        font-weight: 700;
        height: 22px;
        justify-content: center;
        width: 22px;
      }
      .ltm-step-title,
      .ltm-validator-title {
        font-size: 13px;
        font-weight: 700;
        line-height: 1.2;
      }
      .ltm-step-summary,
      .ltm-validator-summary {
        color: #52635e;
        font-size: 11px;
        grid-column: 2 / 4;
        line-height: 1.25;
      }
      .ltm-step-body.panel-body,
      .ltm-validator-body.panel-body {
        padding: 10px;
      }
      .ltm-status-badge {
        align-self: start;
        border-radius: 999px;
        font-size: 10px;
        font-weight: 700;
        line-height: 1;
        padding: 4px 7px;
        white-space: nowrap;
      }
      .ltm-status-ready {
        background-color: #dcefe8;
        color: #14513d;
      }
      .ltm-status-complete {
        background-color: #c8eadf;
        color: #0d4734;
      }
      .ltm-status-needs-data,
      .ltm-status-needs-preprocessing {
        background-color: #f4ead2;
        color: #694d00;
      }
      .ltm-status-out-of-date {
        background-color: #ffe0ca;
        color: #7a3d00;
      }
      .ltm-status-error {
        background-color: #f8d7da;
        color: #842029;
      }
      .ltm-control-grid {
        display: grid;
        gap: 6px 8px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .ltm-grid-full {
        grid-column: 1 / -1;
      }
      .ltm-help,
      .ltm-gate-notice,
      .ltm-step-note {
        color: #52635e;
        font-size: 11px;
        line-height: 1.35;
        margin: 0 0 8px;
      }
      .ltm-gate-notice {
        background-color: #f8f2df;
        border: 1px solid #ead7a7;
        border-radius: 6px;
        color: #694d00;
        padding: 7px 8px;
      }
      .ltm-ready-note {
        background-color: #edf7f3;
        border-color: #b8d8cc;
        color: #14513d;
      }
      #latitude.ltm-coordinate-highlight,
      #longitude.ltm-coordinate-highlight {
        background-color: #fff1a8 !important;
        border-color: #946200 !important;
        box-shadow: 0 0 0 2px rgba(148, 98, 0, 0.18);
      }
      .ltm-coordinate-status {
        background-color: #edf7f3;
        border: 1px solid #b8d8cc;
        border-radius: 6px;
        color: #14513d;
        font-size: 11px;
        line-height: 1.35;
        margin-bottom: 8px;
        padding: 7px 8px;
      }
      .ltm-coordinate-status-warning {
        background-color: #fff3cd;
        border-color: #f0d98a;
        color: #5f4a00;
      }
      .ltm-map-picker {
        align-items: center;
        display: flex;
        gap: 8px;
        margin: 0 0 8px;
      }
      .ltm-map-picker .btn {
        margin: 0;
      }
      .ltm-details {
        border-top: 1px solid #e4ebe8;
        margin-top: 9px;
        padding-top: 8px;
      }
      .ltm-details summary {
        color: #20342f;
        cursor: pointer;
        font-size: 12px;
        font-weight: 700;
        margin-bottom: 6px;
      }
      .ltm-subsection-title {
        color: #20342f;
        font-size: 12px;
        font-weight: 700;
        margin: 9px 0 5px;
      }
      .ltm-sidebar-settings {
        border-top: 1px solid #d8e0dd;
        margin-top: 10px;
        padding-top: 10px;
      }
      .ltm-sidebar-settings pre {
        white-space: pre-wrap;
      }
      @media (max-width: 900px) {
        body {
          padding-top: 74px;
        }
        .ltm-app-shell {
          display: block;
          padding: 12px;
        }
        .ltm-sidebar {
          max-width: none;
          max-height: none;
          position: static;
          width: 100%;
        }
        .main-panel {
          margin-top: 16px;
        }
      }
      @media (max-width: 520px) {
        .ltm-control-grid {
          grid-template-columns: 1fr;
        }
        .ltm-step-toggle,
        .ltm-validator-toggle {
          grid-template-columns: auto 1fr;
        }
        .ltm-status-badge {
          grid-column: 2;
          justify-self: start;
        }
        .ltm-step-summary,
        .ltm-validator-summary {
          grid-column: 1 / -1;
        }
      }
      ")),
      tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('ltm-collapse', function(message) {
        var panel = $('#' + message.id);
        if (!panel.length) {
          return;
        }
        if (message.action === 'show') {
          panel.collapse('show');
        }
        if (message.action === 'hide') {
          panel.collapse('hide');
        }
      });
      "))
    ),

    shiny::div(
      class = "ltm-banner",
      shiny::div(
        style = "display: flex; align-items: center;",
        tags$img(
          src = ltm_app_asset("LTM_logo-v1.png"),
          height = "60px",
          style = "margin-right: 5px;"
        ),
        h2("Large Tree Monitoring")
      )
    ),

    shiny::div(
      class = "ltm-app-shell",
      tags$aside(
        class = "ltm-sidebar",
        shiny::div(
          class = "ltm-sidebar-header",
          tags$h3("Workflow"),
          tags$p("Follow the steps from tree selection to validated breakpoints.")
        ),

        shiny::div(
          class = "ltm-workflow-accordion",
          ltm_workflow_step_ui(
            step_id = "target",
            step_number = 1,
            title = "Target point",
            status_output_id = "target_step_status",
            summary_output_id = "target_step_summary",
            open = TRUE,
            ltm_help_text("Choose a tree point before requesting Sentinel-2 data."),
            shiny::uiOutput("coordinate_source_ui"),
            ltm_control_grid(
              shiny::selectInput(
                "location_id",
                "Location ID",
                choices = tree_ids,
                selectize = TRUE,
                selected = "---"
              ),
              shiny::br(),
              shiny::numericInput("latitude", "Latitude", value = 41.720898),
              shiny::numericInput("longitude", "Longitude", value = -8.747039)
            ),
            shiny::uiOutput("tree_list_warning"),
            shiny::p(
              tags$span("Select point from map"),
              shiny::actionButton(
                "choose_coords",
                "",
                icon = shiny::icon("map-location-dot")
              ),
              class = "ltm-map-picker control-label"
            ),
            shiny::uiOutput("coordinate_source_status"),
            shiny::actionButton(
              "continue_to_fetch",
              "Continue to data settings",
              class = "ltm-primary-action"
            )
          ),

          ltm_workflow_step_ui(
            step_id = "fetch",
            step_number = 2,
            title = "Fetch data",
            status_output_id = "fetch_step_status",
            summary_output_id = "fetch_step_summary",
            shiny::uiOutput("fetch_gate_notice"),
            ltm_control_grid(
              shiny::dateInput(
                "start_date",
                tags$span(shiny::icon("calendar"), "Start Date"),
                value = "2015-01-01"
              ),
              shiny::dateInput(
                "end_date",
                tags$span(shiny::icon("calendar"), "End Date"),
                value = "2024-12-31"
              ),
              shiny::selectInput(
                "spi",
                tags$span(shiny::icon("buffer"), "Spectral Index"),
                choices = c("NDVI", "EVI", "EVI2", "NBR", "NDRE")
              ),
              shiny::selectInput(
                "proc_level",
                "Processing Level",
                choices = c("L1C", "L2A")
              )
            ),
            shiny::actionButton(
              "fetch_data",
              "Fetch Time Series Data",
              class = "ltm-primary-action"
            ),
            shiny::uiOutput("fetched_data_summary")
          ),

          ltm_workflow_step_ui(
            step_id = "preprocess",
            step_number = 3,
            title = "Preprocess",
            status_output_id = "preprocess_step_status",
            summary_output_id = "preprocess_step_summary",
            shiny::uiOutput("preprocess_gate_notice"),
            ltm_help_text("Run the full preprocessing chain before breakpoint analysis."),
            ltm_control_grid(
              shiny::selectInput(
                "regularize_method",
                tags$span(shiny::icon("chart-bar"), "Regularization"),
                choices = c("linear", "spline", "stine", "mean", "kalman", "locf", "nocb")
              ),
              shiny::checkboxInput("use_cloud_mask", "Use cloud mask", value = TRUE),
              shiny::selectInput(
                "quant",
                tags$span(shiny::icon("chart-line"), "Window quantile"),
                choices = c(0.95, 0.9, 0.75)
              ),
              shiny::numericInput(
                "win_size",
                "Window size (days)",
                value = 15,
                min = 5,
                max = 180
              ),
              shiny::numericInput(
                "lambda",
                tags$span(shiny::icon("chart-line"), "Whittaker lambda"),
                value = 20000,
                min = 1
              ),
              shiny::numericInput(
                "quantile_thresh",
                "Whittaker quantile threshold",
                value = 0.35,
                min = 0.01,
                max = 1
              ),
              shiny::checkboxInput("use_weights", "Use Whittaker weights", value = TRUE)
            ),
            shiny::actionButton(
              "run_preprocessing",
              "Run preprocessing",
              class = "ltm-primary-action"
            ),
            tags$details(
              class = "ltm-details",
              tags$summary("Run preprocessing steps individually"),
              shiny::div(
                class = "ltm-inline-actions",
                shiny::actionButton("regularize", "Regularize/interpolate Time Series"),
                shiny::actionButton("apply_mov_quantile", "Apply Moving Window"),
                shiny::actionButton("apply_whittaker", "Apply Whittaker Smoother")
              )
            )
          ),

          ltm_workflow_step_ui(
            step_id = "breaks",
            step_number = 4,
            title = "Breakpoint analysis",
            status_output_id = "breaks_step_status",
            summary_output_id = "breaks_step_summary",
            shiny::uiOutput("breaks_gate_notice"),
            ltm_help_text("Select at least one method. Bayesian Change Points can take much longer than the other methods."),
            shiny::checkboxGroupInput(
              "ts_names",
              "Time series type(s)",
              choices = c(
                "Original time series (daily)" = "spi",
                "Moving window series" = "spi_mov_wind",
                "Smoothed series" = "spi_smooth",
                "Moving window smoothed" = "spi_mov_smooth"
              ),
              selected = c("spi")
            ),
            ltm_control_grid(
              shiny::numericInput(
                "s_window",
                "STL window size",
                value = 30,
                min = 5,
                max = 365
              ),
              shiny::dateInput(
                "break_thresh_date",
                "Break threshold date",
                value = "2016-01-01"
              )
            ),
            shiny::checkboxGroupInput(
              inputId = "break_methods",
              label = "Break-detection methods",
              choiceNames = list(
                HTML("Change Point Model (cpm) <span title='Detects sequential changes using the cpm package'>[i]</span>"),
                HTML("Energy Divisive (ed) <span title='ecp package e.divisive method: clustering-based method'>[i]</span>"),
                HTML("BFAST <span title='Breaks in season-trend using bfast'>[i]</span>"),
                HTML("Bayesian Change Points (mcp) <span title='Bayesian model-based detection using mcp'>[i]</span>"),
                HTML("Structural Changes (stc) <span title='strucchange package for structural breaks'>[i]</span>"),
                HTML("Wild Binary Segmentation (wbs) <span title='Fast segmentation of time series'>[i]</span>")
              ),
              choiceValues = c("cpm", "ed", "bfast", "mcp", "stc", "wbs"),
              selected = NULL
            ),
            tags$div(
              id = "workflow_validator_settings",
              class = "ltm-validator-accordion panel panel-default",
              tags$div(
                class = "ltm-validator-heading panel-heading",
                tags$a(
                  class = "ltm-validator-toggle",
                  href = "#workflow_validator_settings_body",
                  `data-toggle` = "collapse",
                  `aria-expanded` = "false",
                  tags$span(class = "ltm-step-number", "V"),
                  tags$span(class = "ltm-validator-title", "Validation settings"),
                  tags$span(
                    class = "ltm-status-badge ltm-status-ready",
                    "Defaults"
                  ),
                  tags$span(
                    class = "ltm-validator-summary",
                    "Long-term, short-term, and trend validators"
                  )
                )
              ),
              tags$div(
                id = "workflow_validator_settings_body",
                class = "panel-collapse collapse",
                tags$div(
                  class = "ltm-validator-body panel-body",
                  ltm_validator_controls_ui()
                )
              )
            ),
            shiny::actionButton(
              "analyze_breaks",
              "Analyze Breaks",
              class = "ltm-primary-action"
            )
          )
        ),

        shiny::div(
          class = "ltm-sidebar-settings",
          tags$span(shiny::icon("gear"), " Configure parameters"),
          shiny::actionButton("edit_params", "Edit parameters"),
          shiny::verbatimTextOutput("param_save_status")
        )
      ),

      ## Right-side with analysis results

      shiny::div(
        class = "main-panel",

      shiny::uiOutput("loading"),

      shiny::verbatimTextOutput("status"),

      # Time series plot + small thumbnail map side by side
      shiny::fluidRow(
        column(
          width = 9,
          plotOutput("plot_spidf", height = "500px")
        ),
        column(
          width = 3,
          leafletOutput("location_map", height = "500px")  # <-- Leaflet map output
        )
      ),

      shiny::uiOutput("download_data_ui"),  # <- dynamic button only when data is ready


      shiny::hr(),

      shiny::verbatimTextOutput("break_analysis_status"),

      shiny::div(
        style = "margin: 20px;",
        tableOutput("break_results_table"),

        # Add this download button next to the table
        shiny::uiOutput("download_breaks_ui")  # <- dynamic button only when data is ready

      ),

      # Two new plots side by side (patchwork) - only if valid breaks exist
      shiny::fluidRow(
        column(
          width = 12,
          plotOutput("valid_breaks_plots", height = "400px")
        )
      ),

        shiny::uiOutput("break_summary_ui")
      )
    )
  )



  server <- function(input, output, session) {

    params <- shiny::reactiveVal(startup$params)
    tree_state <- shiny::reactiveVal(startup$tree_state)
    spidf_data <- reactiveVal(NULL)
    fetched_request_key <- reactiveVal(NULL)
    breakResults <- reactiveVal(NULL)

    # Store the actual location used by the last fetch, independent of form edits.
    selectedCoord <- reactiveVal(NULL)
    active_coord_source <- reactiveVal(ltm_coord_source_manual())
    coord_inputs_updating <- reactiveVal(FALSE)
    coord_update_token <- reactiveVal(0L)

    set_collapse_panel <- function(panel_id, action = c("show", "hide")) {
      action <- match.arg(action)
      session$sendCustomMessage(
        "ltm-collapse",
        list(id = panel_id, action = action)
      )
    }

    show_workflow_step <- function(step_id) {
      set_collapse_panel(paste0("workflow_step_", step_id, "_body"), "show")
    }

    hide_workflow_step <- function(step_id) {
      set_collapse_panel(paste0("workflow_step_", step_id, "_body"), "hide")
    }

    set_inputs_enabled <- function(input_ids, enabled) {
      for (input_id in input_ids) {
        if (isTRUE(enabled)) {
          shinyjs::enable(input_id)
        } else {
          shinyjs::disable(input_id)
        }
      }
    }

    gate_notice <- function(text, ready = FALSE) {
      div(
        class = paste("ltm-gate-notice", if (isTRUE(ready)) "ltm-ready-note" else ""),
        text
      )
    }

    set_coordinate_source <- function(source) {
      if (!source %in% c(ltm_coord_source_manual(), ltm_coord_source_tree_list())) {
        source <- ltm_coord_source_manual()
      }

      active_coord_source(source)
      if (!identical(shiny::isolate(input$coord_source), source)) {
        shiny::updateRadioButtons(session, "coord_source", selected = source)
      }
    }

    clear_coordinate_highlight <- function() {
      shinyjs::removeClass(id = "latitude", class = "ltm-coordinate-highlight")
      shinyjs::removeClass(id = "longitude", class = "ltm-coordinate-highlight")
    }

    highlight_coordinate_inputs <- function() {
      shinyjs::addClass(id = "latitude", class = "ltm-coordinate-highlight")
      shinyjs::addClass(id = "longitude", class = "ltm-coordinate-highlight")
      shinyjs::delay(1400, {
        clear_coordinate_highlight()
      })
    }

    update_coordinate_inputs <- function(lat, lon, highlight = FALSE) {
      current_token <- coord_update_token() + 1L
      coord_update_token(current_token)
      coord_inputs_updating(TRUE)

      shiny::updateNumericInput(session, "latitude", value = lat)
      shiny::updateNumericInput(session, "longitude", value = lon)

      if (isTRUE(highlight)) {
        highlight_coordinate_inputs()
      } else {
        clear_coordinate_highlight()
      }

      shinyjs::delay(500, {
        if (identical(coord_update_token(), current_token)) {
          coord_inputs_updating(FALSE)
        }
      })
    }

    clear_location_id <- function() {
      if (!is.null(ltm_selected_tree_id(shiny::isolate(input$location_id)))) {
        shiny::updateSelectInput(session, "location_id", selected = "---")
      }
    }

    coordinate_status_div <- function(text, warning = FALSE) {
      div(
        class = paste(
          "ltm-coordinate-status",
          if (isTRUE(warning)) "ltm-coordinate-status-warning" else ""
        ),
        #tags$strong("Coordinates source:"),
        #tags$br(),
        text
      )
    }

    target_request <- shiny::reactive({
      tryCatch(
        ltm_resolve_coordinate_request(
          coord_source = active_coord_source(),
          location_id = input$location_id,
          latitude = input$latitude,
          longitude = input$longitude,
          tree_state = tree_state()
        ),
        error = function(error) {
          error
        }
      )
    })

    current_request_key <- shiny::reactive({
      ltm_current_request_key(
        coord_request = target_request(),
        start_date = input$start_date,
        end_date = input$end_date,
        spi = input$spi,
        proc_level = input$proc_level
      )
    })

    workflow_state <- shiny::reactive({
      ltm_workflow_state(
        target_request = target_request(),
        current_request_key = current_request_key(),
        spidf = spidf_data(),
        fetched_request_key = fetched_request_key(),
        break_results = breakResults()
      )
    })

    shiny::observe({
      state <- workflow_state()

      set_inputs_enabled(
        c("start_date", "end_date", "spi", "proc_level", "fetch_data"),
        state$target_ready
      )
      set_inputs_enabled(
        c(
          "regularize_method", "use_cloud_mask", "quant", "win_size",
          "lambda", "quantile_thresh", "use_weights", "run_preprocessing",
          "regularize", "apply_mov_quantile", "apply_whittaker"
        ),
        state$data_ready
      )
      set_inputs_enabled(
        c(
          "ts_names", "s_window", "break_thresh_date", "break_methods",
          "lt_window", "lt_thresh_change", "lt_fun", "st_window",
          "st_thresh_change", "st_fun", "trend_window",
          "trend_post_pct_thresh", "trend_alpha", "trend_deficit_tol",
          "trend_min_prop_below", "trend_avg_deficit_thresh",
          "analyze_breaks"
        ),
        state$preprocess_ready
      )
    })

    observeEvent(current_request_key(), {
      if (
        !is.null(fetched_request_key()) &&
          !identical(current_request_key(), fetched_request_key())
      ) {
        breakResults(NULL)
        output$break_analysis_status <- renderText("")
      }
    }, ignoreInit = TRUE)

    output$coordinate_source_ui <- renderUI({
      choices <- ltm_coord_source_choices(
        ltm_tree_list_available(tree_state())
      )
      selected <- active_coord_source()
      if (!selected %in% unname(choices)) {
        selected <- ltm_coord_source_manual()
      }

      shiny::radioButtons(
        "coord_source",
        #tags$span(emoji_html("1F4CD"), "Coordinate source"),
        tags$span(shiny::icon("map-pin"), "Coordinate source:", class = "control-label"),
        choices = choices,
        selected = selected
      )
    })

    output$coordinate_source_status <- renderUI({
      request <- target_request()

      if (inherits(request, "error")) {
        return(coordinate_status_div(conditionMessage(request), warning = TRUE))
      }

      coordinate_status_div(
        sprintf(
          "Point coordinates will use %s: %.6f, %.6f",
          request$status_label,
          request$lat,
          request$lon
        )
      )
    })

    output$target_step_status <- renderUI({
      request <- target_request()
      if (inherits(request, "error")) {
        return(ltm_workflow_status_badge("Error"))
      }

      ltm_workflow_status_badge("Ready")
    })

    output$target_step_summary <- renderUI({
      request <- target_request()
      if (inherits(request, "error")) {
        return(conditionMessage(request))
      }

      sprintf(
        "%s: %.6f, %.6f",
        request$status_label,
        request$lat,
        request$lon
      )
    })

    output$fetch_step_status <- renderUI({
      state <- workflow_state()

      if (!state$target_ready) {
        return(ltm_workflow_status_badge("Error"))
      }
      if (state$data_ready) {
        return(ltm_workflow_status_badge("Complete"))
      }
      if (state$data_stale) {
        return(ltm_workflow_status_badge("Out of date"))
      }

      ltm_workflow_status_badge("Needs data")
    })

    output$fetch_step_summary <- renderUI({
      state <- workflow_state()

      if (!state$target_ready) {
        return("Fix the target point first")
      }
      if (state$data_ready) {
        return(sprintf(
          "%s %s, %s to %s",
          input$spi,
          input$proc_level,
          input$start_date,
          input$end_date
        ))
      }
      if (state$data_stale) {
        return("Fetch again for the current settings")
      }

      "Set dates, spectral index, and processing level"
    })

    output$preprocess_step_status <- renderUI({
      state <- workflow_state()

      if (state$data_stale) {
        return(ltm_workflow_status_badge("Out of date"))
      }
      if (state$preprocess_ready) {
        return(ltm_workflow_status_badge("Complete"))
      }
      if (!state$data_ready) {
        return(ltm_workflow_status_badge("Needs data"))
      }

      ltm_workflow_status_badge("Needs preprocessing")
    })

    output$preprocess_step_summary <- renderUI({
      state <- workflow_state()

      if (state$data_stale) {
        return("Fetch data again before preprocessing")
      }
      if (!state$data_ready) {
        return("Fetch time series data first")
      }
      if (state$preprocess_ready) {
        return("Regularized, windowed, and smoothed")
      }
      if (state$regularized) {
        return("Regularized; complete moving window and smoothing")
      }

      "Ready for regularization, windowing, and smoothing"
    })

    output$breaks_step_status <- renderUI({
      state <- workflow_state()

      if (state$data_stale) {
        return(ltm_workflow_status_badge("Out of date"))
      }
      if (state$breaks_ready) {
        return(ltm_workflow_status_badge("Complete"))
      }
      if (!state$preprocess_ready) {
        return(ltm_workflow_status_badge("Needs preprocessing"))
      }

      ltm_workflow_status_badge("Ready")
    })

    output$breaks_step_summary <- renderUI({
      state <- workflow_state()

      if (state$data_stale) {
        return("Fetch and preprocess the current request")
      }
      if (!state$preprocess_ready) {
        return("Complete preprocessing first")
      }
      if (state$breaks_ready) {
        return("Break analysis complete")
      }

      "Choose series and detection methods"
    })

    output$fetch_gate_notice <- renderUI({
      state <- workflow_state()

      if (!state$target_ready) {
        return(gate_notice("Fix the target point before fetching data."))
      }
      if (state$data_stale) {
        return(gate_notice("Target or data settings changed. Fetch data again before continuing."))
      }
      if (state$data_ready) {
        return(gate_notice("Data is ready for the current target and settings.", ready = TRUE))
      }

      NULL
    })

    output$fetched_data_summary <- renderUI({
      state <- workflow_state()

      if (!state$has_data) {
        return(NULL)
      }

      if (state$data_stale) {
        return(gate_notice("The displayed time series belongs to an earlier request. Fetch data again to update it."))
      }

      gate_notice(
        sprintf("Fetched time series is ready (%s rows).", nrow(spidf_data())),
        ready = TRUE
      )
    })

    output$preprocess_gate_notice <- renderUI({
      state <- workflow_state()

      if (state$data_stale) {
        return(gate_notice("Fetch data again for the current target and data settings before preprocessing."))
      }
      if (!state$data_ready) {
        return(gate_notice("Fetch time series data before preprocessing."))
      }
      if (state$preprocess_ready) {
        return(gate_notice("Preprocessing is complete for the current data.", ready = TRUE))
      }

      NULL
    })

    output$breaks_gate_notice <- renderUI({
      state <- workflow_state()

      if (state$data_stale) {
        return(gate_notice("Fetch and preprocess the current request before breakpoint analysis."))
      }
      if (!state$preprocess_ready) {
        return(gate_notice("Run preprocessing before breakpoint analysis."))
      }
      if (state$breaks_ready) {
        return(gate_notice("Break analysis is complete. Results are shown in the main panel.", ready = TRUE))
      }

      NULL
    })

    output$tree_list_warning <- renderUI({
      tree_message <- tree_state()$message

      if (is.null(tree_message) || !nzchar(tree_message)) {
        return(NULL)
      }

      div(
        style = paste(
          "background-color: #fff3cd;",
          "border: 1px solid #f0d98a;",
          "border-radius: 4px;",
          "color: #5f4a00;",
          "font-size: 12px;",
          "margin-bottom: 10px;",
          "padding: 8px;"
        ),
        tree_message
      )
    })

    observeEvent(tree_state(), {
      if (
        !ltm_tree_list_available(tree_state()) &&
          identical(active_coord_source(), ltm_coord_source_tree_list())
      ) {
        set_coordinate_source(ltm_coord_source_manual())
      }
    }, ignoreInit = TRUE)

    observeEvent(input$coord_source, {
      source <- input$coord_source

      if (
        identical(source, ltm_coord_source_tree_list()) &&
          !ltm_tree_list_available(tree_state())
      ) {
        set_coordinate_source(ltm_coord_source_manual())
        return(NULL)
      }

      set_coordinate_source(source)

      if (identical(source, ltm_coord_source_manual())) {
        clear_location_id()
        clear_coordinate_highlight()
        return(NULL)
      }

      tree_location <- tryCatch(
        ltm_get_tree_location(tree_state(), input$location_id),
        error = function(error) {
          error
        }
      )

      if (!inherits(tree_location, "error") && !is.null(tree_location)) {
        update_coordinate_inputs(
          tree_location$lat,
          tree_location$lon,
          highlight = TRUE
        )
      }
    }, ignoreInit = TRUE)

    observeEvent(input$location_id, {
      selected_tree_id <- ltm_selected_tree_id(input$location_id)

      if (is.null(selected_tree_id)) {
        if (identical(active_coord_source(), ltm_coord_source_tree_list())) {
          set_coordinate_source(ltm_coord_source_manual())
        }
        clear_coordinate_highlight()
        return(NULL)
      }

      tree_location <- tryCatch(
        ltm_get_tree_location(tree_state(), selected_tree_id),
        error = function(error) {
          error
        }
      )

      if (inherits(tree_location, "error")) {
        shinyalert::shinyalert(
          title = "Location list error",
          text = conditionMessage(tree_location),
          type = "error"
        )
        set_coordinate_source(ltm_coord_source_manual())
        shiny::updateSelectInput(session, "location_id", selected = "---")
        return(NULL)
      }

      set_coordinate_source(ltm_coord_source_tree_list())
      update_coordinate_inputs(
        tree_location$lat,
        tree_location$lon,
        highlight = TRUE
      )
    }, ignoreInit = TRUE)

    observeEvent({
      list(input$latitude, input$longitude)
    }, {
      if (isTRUE(coord_inputs_updating())) {
        return(NULL)
      }

      set_coordinate_source(ltm_coord_source_manual())
      clear_location_id()
      clear_coordinate_highlight()
    }, ignoreInit = TRUE)

    observeEvent(input$continue_to_fetch, {
      request <- target_request()

      if (inherits(request, "error")) {
        shinyalert::shinyalert(
          title = "Coordinate source error",
          text = conditionMessage(request),
          type = "error"
        )
        show_workflow_step("target")
        return(NULL)
      }

      hide_workflow_step("target")
      show_workflow_step("fetch")
    })

    ## ------------------------------------------------------------------------ ##
    #  ---- Coordinate picker with reactive val to update main coordinates ----
    ## ------------------------------------------------------------------------ ##

    # Reactive value to store the coordinate chosen in the modal
    pickedCoords <- reactiveVal(list(lat = NULL, lon = NULL))

    observeEvent(input$choose_coords, {
      # Reset the picked coordinates
      pickedCoords(list(lat = NULL, lon = NULL))

      # Re-render the leaflet output for the modal to ensure no marker is present
      output$coord_picker_map <- renderLeaflet({
        lat <- input$latitude
        lon <- input$longitude
        leaflet() %>%
          addTiles(group = "OpenStreetMap") %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          setView(lng = lon, lat = lat, zoom = 11) %>%
          addLayersControl(
            baseGroups = c("OpenStreetMap", "Satellite"),
            options = layersControlOptions(collapsed = FALSE)
          )
      })

      showModal(modalDialog(
        title = "Select location",
        size = "xl",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_coords", "Confirm selection")
        ),
        leafletOutput("coord_picker_map", height = "600px")
      ))
    })

    output$coord_picker_map <- renderLeaflet({
      lat <- input$latitude
      lon <- input$longitude

      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        setView(lng = lon, lat = lat, zoom = 13) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    observeEvent(input$coord_picker_map_click, {

      click <- input$coord_picker_map_click

      if (!is.null(click)) {

        pickedCoords(list(lat = click$lat, lon = click$lng))

        leafletProxy("coord_picker_map") %>%
          clearMarkers() %>%
          addMarkers(lng = click$lng, lat = click$lat, popup = "Chosen location")
      }
    })

    observeEvent(input$confirm_coords, {
      coords <- pickedCoords()

      if (!is.null(coords$lat) && !is.null(coords$lon)) {

        set_coordinate_source(ltm_coord_source_manual())
        clear_location_id()
        update_coordinate_inputs(
          coords$lat,
          coords$lon,
          highlight = FALSE
        )
      }

      removeModal()
    })


    ## -------------------------------------------------------------- ##
    # ---- Data fetching logic ----
    ## -------------------------------------------------------------- ##

    observeEvent(input$fetch_data, {

      output$loading <- renderUI({
        tagList(
          tags$img(src = ltm_app_asset("loading.gif"), height = "90px"),
          tags$span("Checking local cache or fetching GEE data. Please wait.")
        )
      })

      shinyjs::delay(1000, {
        tryCatch({
          date_error <- FALSE
          # Validate dates for Sentinel L1C / L2A
          if ((input$proc_level %in% c("L1","L1C")) && (input$start_date < as.Date("2015-01-01"))) {
            shinyalert::shinyalert(
              title = "Error in start date",
              text = "The start date cannot be earlier than 2015-01-01 for L1/L1C Sentinel-2 data",
              type = "error"
            )
            output$loading <- renderUI({ NULL })
            show_workflow_step("fetch")
            date_error <- TRUE
          }
          if ((input$proc_level %in% c("L2","L2A")) && (input$start_date < as.Date("2017-01-01"))) {
            shinyalert::shinyalert(
              title = "Error in start date",
              text = "The start date cannot be earlier than 2017-01-01 for L2/L2A Sentinel-2 data",
              type = "error"
            )
            output$loading <- renderUI({ NULL })
            show_workflow_step("fetch")
            date_error <- TRUE
          }

          if(!date_error) {
            selected_tree_id <- NULL
            requested_buffer_radius <- NULL
            cache_status_detail <- NULL
            location_ready <- TRUE
            cache_hit <- FALSE

            coord_request <- target_request()

            if (inherits(coord_request, "error")) {
              shinyalert::shinyalert(
                title = "Coordinate source error",
                text = conditionMessage(coord_request),
                type = "error"
              )
              output$loading <- renderUI({ NULL })
              show_workflow_step("target")
              location_ready <- FALSE
            }

            if (isTRUE(location_ready)) {
              lat <- coord_request$lat
              lon <- coord_request$lon
              selected_tree_id <- coord_request$tree_id
              request_key <- ltm_current_request_key(
                coord_request = coord_request,
                start_date = input$start_date,
                end_date = input$end_date,
                spi = input$spi,
                proc_level = input$proc_level
              )

              if (is.null(request_key)) {
                shinyalert::shinyalert(
                  title = "Data request error",
                  text = "Complete the target point and data settings before fetching data.",
                  type = "error"
                )
                output$loading <- renderUI({ NULL })
                show_workflow_step("fetch")
                location_ready <- FALSE
              }
            }

            if (isTRUE(location_ready)) {
              ## Check if it is necessary to clean break analysis outputs if a
              # new set of coordinates have appeared including the nulling the
              ## breaks in the plot and the tables in the bottom part.
              ###########################################################################

              last_loc <- ltm_read_location()
              current_loc <- paste(round(lat, 6), round(lon, 6), sep = ",")

              # If location changed, reset break results
              if (is.null(last_loc) || (!identical(current_loc, last_loc))) {
                # Clear break analysis objects & outputs
                breakResults(NULL)
                output$break_analysis_status <- renderText("")
              }

              # 3) Update the stored location
              ltm_store_location(lat, lon)

              ###########################################################################

              cached_file <- ltm_check_cache(
                lat, lon,
                input$start_date, input$end_date,
                input$spi, input$proc_level,
                tree_id = selected_tree_id,
                buffer_radius_m = requested_buffer_radius
              )

              cached_data <- NULL
              if (!is.null(cached_file)) {
                cached_data <- tryCatch(
                  ltm_read_cached_spidf(
                    cached_file,
                    metadata_tags = list(
                      lat = lat,
                      lon = lon,
                      start_date = input$start_date,
                      end_date = input$end_date,
                      spi = input$spi,
                      proc_level = input$proc_level,
                      tree_id = selected_tree_id,
                      buffer_radius_m = requested_buffer_radius
                    )
                  ),
                  error = function(error) {
                    warning(
                      "Could not read cached file '",
                      cached_file,
                      "': ",
                      error$message
                    )
                    cache_status_detail <<- paste(
                      "Cached file could not be used:",
                      basename(cached_file),
                      "-",
                      error$message
                    )
                    NULL
                  }
                )

                if (!is.null(cached_data)) {
                  tryCatch(
                    ltm_save_to_cache(
                      cached_data,
                      metadata_tags = list(
                        lat = lat,
                        lon = lon,
                        start_date = input$start_date,
                        end_date = input$end_date,
                        spi = input$spi,
                        proc_level = input$proc_level,
                        tree_id = selected_tree_id,
                        buffer_radius_m = requested_buffer_radius
                      )
                    ),
                    error = function(error) {
                      warning("Could not refresh normalized cache file: ", error$message)
                      NULL
                    }
                  )

                  output$status <- renderText(paste("[OK] Data loaded from cache:", cached_file))
                  spidf_data(cached_data)
                  fetched_request_key(request_key)
                  breakResults(NULL)
                  output$break_analysis_status <- renderText("")
                  selectedCoord(coord_request)
                  output$loading <- renderUI({NULL})
                  hide_workflow_step("fetch")
                  show_workflow_step("preprocess")
                  cache_hit <- TRUE
                }
              }

              if (!isTRUE(cache_hit)) {
                if (ltm_check_gee_status() != "CONNECTED") {
                  output$status <- renderText("Initializing GEE connection...")
                  if (!isTRUE(ltm_start_gee(params()$gee$gee_username))) {
                    stop(
                      paste(
                        c(
                          "Google Earth Engine could not be initialized and no usable cache file was found.",
                          cache_status_detail
                        ),
                        collapse = " "
                      ),
                      call. = FALSE
                    )
                  }
                }

                shinyjs::delay(1000, {
                  tryCatch({
                    df <- ltm_s2_get_data_point(
                      lat        = lat,
                      lon        = lon,
                      start_date = format(input$start_date, "%Y-%m-%d"),
                      end_date   = format(input$end_date,   "%Y-%m-%d"),
                      spi        = input$spi,
                      proc_level = input$proc_level,
                      tree_id = selected_tree_id
                    )

                    file_name <- tryCatch(
                      ltm_save_to_cache(
                        df,
                        metadata_tags = list(
                          lat = lat,
                          lon = lon,
                          start_date = input$start_date,
                          end_date = input$end_date,
                          spi = input$spi,
                          proc_level = input$proc_level,
                          tree_id = selected_tree_id,
                          buffer_radius_m = requested_buffer_radius
                        )
                      ),
                      error = function(error) {
                        warning("Could not write cache file: ", error$message)
                        NULL
                      }
                    )

                    if (is.null(file_name)) {
                      output$status <- renderText(
                        paste(
                          c(
                            "[OK] Data fetched from GEE, but the cache file could not be written.",
                            cache_status_detail
                          ),
                          collapse = " "
                        )
                      )
                    } else {
                      output$status <- renderText(
                        paste(
                          c(
                            paste("[OK] Data fetched and cached at:", file_name),
                            cache_status_detail
                          ),
                          collapse = " "
                        )
                      )
                    }

                    spidf_data(df)
                    fetched_request_key(request_key)
                    breakResults(NULL)
                    output$break_analysis_status <- renderText("")
                    selectedCoord(coord_request)

                    output$loading <- renderUI({NULL})
                    hide_workflow_step("fetch")
                    show_workflow_step("preprocess")
                  }, error = function(error) {
                    shinyalert::shinyalert(
                      title = "Error fetching time series",
                      text = error$message,
                      type = "error"
                    )
                    output$status <- renderText(paste("[ERROR]", error$message))
                    output$loading <- renderUI({NULL})
                    show_workflow_step("fetch")
                    NULL
                  })
                })
              }
            }
          }
        }, error = function(error) {
          shinyalert::shinyalert(
            title = "Error while loading time series",
            text = error$message,
            type = "error"
          )
          output$status <- renderText(paste("[ERROR]", error$message))
          output$loading <- renderUI({ NULL })
          show_workflow_step("fetch")
          NULL
        })
      })
    })

    require_current_data <- function(action_label) {
      state <- workflow_state()

      if (state$data_stale) {
        shinyalert::shinyalert(
          title = "Action Required",
          text = paste("Fetch data again for the current target and settings before", action_label, "."),
          type = "error"
        )
        show_workflow_step("fetch")
        return(FALSE)
      }

      if (!state$data_ready) {
        shinyalert::shinyalert(
          title = "Action Required",
          text = paste("Fetch time series data before", action_label, "."),
          type = "error"
        )
        show_workflow_step("fetch")
        return(FALSE)
      }

      TRUE
    }

    observeEvent(input$run_preprocessing, {
      req(spidf_data())

      if (!isTRUE(require_current_data("preprocessing"))) {
        return(NULL)
      }

      tryCatch({
        preprocessed <- ltm_regularize_spidf(
          spidf_data(),
          method = input$regularize_method,
          use_cloud_mask = input$use_cloud_mask
        )
        preprocessed <- ltm_apply_moving_quantile(
          preprocessed,
          quant = as.numeric(input$quant),
          win_size = input$win_size
        )
        preprocessed <- ltm_apply_whitaker(
          preprocessed,
          lambda = input$lambda,
          quantile_thresh = input$quantile_thresh,
          use_weights = input$use_weights
        )

        spidf_data(preprocessed)
        breakResults(NULL)
        output$break_analysis_status <- renderText("")
        output$status <- renderText("[OK] Preprocessing complete.")
        hide_workflow_step("preprocess")
        show_workflow_step("breaks")
      }, error = function(error) {
        shinyalert::shinyalert(
          title = "Error while preprocessing",
          text = error$message,
          type = "error"
        )
        output$status <- renderText(paste("[ERROR]", error$message))
        show_workflow_step("preprocess")
        NULL
      })
    })

    # ---- Regularize logic ----
    observeEvent(input$regularize, {
      req(spidf_data())

      if (!isTRUE(require_current_data("regularizing the time series"))) {
        return(NULL)
      }

      spidf_data(
        ltm_regularize_spidf(
          spidf_data(),
          method         = input$regularize_method,
          use_cloud_mask = input$use_cloud_mask
        )
      )
      breakResults(NULL)
      output$break_analysis_status <- renderText("")
      output$status <- renderText("[OK] Time series regularized.")
      show_workflow_step("preprocess")
    })

    # ---- Moving window quantile ----
    observeEvent(input$apply_mov_quantile, {

      req(spidf_data())

      if (!isTRUE(require_current_data("applying the moving window analysis"))) {
        return(NULL)
      }

      if (!isTRUE(is_regularized(spidf_data()))) {
        shinyalert::shinyalert(
          title = "Action Required",
          text  = "Please regularize/interpolate the time series before
        applying the moving window analysis.",
          type  = "error"
        )
        show_workflow_step("preprocess")
        return(NULL)
      }

      # Perform the moving window analysis with quantiles
      spidf_data(
        ltm_apply_moving_quantile(
          spidf_data(),
          quant    = as.numeric(input$quant),
          win_size = input$win_size
        )
      )

      breakResults(NULL)
      output$break_analysis_status <- renderText("")
      output$status <- renderText("[OK] Moving window quantile applied.")
      show_workflow_step("preprocess")
    })

    # ---- Whittaker smoother ----
    observeEvent(input$apply_whittaker, {
      req(spidf_data())

      if (!isTRUE(require_current_data("applying the Whittaker smoother"))) {
        return(NULL)
      }

      spidf_data(
        ltm_apply_whitaker(
          spidf_data(),
          lambda          = input$lambda,
          quantile_thresh = input$quantile_thresh,
          use_weights     = input$use_weights
        )
      )
      breakResults(NULL)
      output$break_analysis_status <- renderText("")
      output$status <- renderText("[OK] Whittaker smoother applied.")

      if (isTRUE(workflow_state()$preprocess_ready)) {
        hide_workflow_step("preprocess")
        show_workflow_step("breaks")
      } else {
        show_workflow_step("preprocess")
      }
    })

    # ---- Main plot ----
    output$plot_spidf <- renderPlot({
      req(spidf_data())

      # Retrieve the break results if available
      br_obj <- breakResults()   # This reactiveVal stores a list with df_breaks
      df_breaks <- NULL
      if (!is.null(br_obj)) {
        df_breaks <- br_obj$df_breaks
      }

      coords <- selectedCoord()
      tree_id <- ""
      if (!is.null(coords$tree_id)) {
        tree_id <- coords$tree_id
      }

      ltm_plot_spidf_ts(
        spidf_obj = spidf_data(),
        tree_id   = tree_id,
        df_breaks = df_breaks
      )
    })


    ## Download button for time series data

    output$download_data_ui <- renderUI({

      df <- spidf_data()#breakResults()
      req(df)

      downloadButton("download_data_csv", "Download time series to CSV")
    })

    output$download_data_csv <- downloadHandler(
      filename = function() {
        paste0("TimeSeriesData_",format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        # Safely retrieve the breaks data frame
        df <- spidf_data()#breakResults()
        req(df)  # Ensure breakResults() is not NULL
        #df <- br$df_breaks

        # Write the CSV
        utils::write.csv(df, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )


    # ---- Small Leaflet map showing the selected lat/lon ----
    output$location_map <- renderLeaflet({
      coords <- selectedCoord()
      req(coords)
      popup_label <- if (!is.null(coords$status_label)) {
        paste("Fetched location:", coords$status_label)
      } else {
        "Fetched location"
      }

      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        setView(lng = coords$lon, lat = coords$lat, zoom = 13) %>%
        addMarkers(lng = coords$lon, lat = coords$lat, popup = popup_label) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    #################################################

    observeEvent(input$edit_params, {
      showModal(modalDialog(
        title = "Edit parameters (JSON)",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_params", "Save Changes")
        ),
        textAreaInput(
          inputId = "json_editor",
          label = NULL,
          value = "",  # initially blank, will update below
          width = "100%",
          height = "500px"
        )
      ))

      shinyjs::delay(100, {
        updateTextAreaInput(
          session,
          inputId = "json_editor",
          value = as.character(jsonlite::toJSON(params(), pretty = TRUE, auto_unbox = TRUE))
        )
      })
    })

    observeEvent(input$save_params, {
      tryCatch({
        # input$json_editor is a JSON string typed/pasted by the user
        new_params <- jsonlite::parse_json(
          input$json_editor,
          simplifyVector = TRUE
        )

        # Save to the user-specific config file, not the working directory
        ltm_write_params(new_params, config_path)

        # Update reactive app state
        params(new_params)
        tree_info <- ltm_load_tree_list_state(new_params, config_path = config_path)
        tree_state(tree_info)
        ltm_update_tree_choices(session, tree_info, selected = "---")
        set_coordinate_source(ltm_coord_source_manual())
        clear_coordinate_highlight()

        removeModal()

        save_message <- paste0(
          "[OK] Parameters saved: ",
          normalizePath(config_path, mustWork = FALSE)
        )
        if (!is.null(tree_info$message) && nzchar(tree_info$message)) {
          save_message <- paste(save_message, tree_info$message, sep = "\n")
        }

        output$param_save_status <- renderText(save_message)

      }, error = function(e) {
        output$param_save_status <- renderText(
          paste("[ERROR] Error parsing or saving JSON:", e$message)
        )
      })
    })


    #################################################


    # ---- Break Analysis Logic ----
    observeEvent(input$analyze_breaks, {

      message("[DEBUG] Analyze_breaks triggered.")
      message("[DEBUG] Break_methods: ", paste(input$break_methods, collapse=", "))
      message("[DEBUG] ts_names: ", paste(input$ts_names, collapse=", "))

      req(spidf_data())
      if (!isTRUE(workflow_state()$preprocess_ready)) {
        shinyalert::shinyalert(
          title = "Action Required",
          text = "Run preprocessing for the current data before breakpoint analysis.",
          type = "error"
        )
        show_workflow_step("preprocess")
        return(NULL)
      }

      message("[DEBUG] spidf_data rows: ", nrow(spidf_data()))  # or however your spidf is structured

      if (length(input$break_methods) == 0 || length(input$ts_names) == 0) {
        #showNotification("No break methods or time-series types selected.", type = "error")
        #message("[DEBUG] => no break methods or no timeseries, returning NULL.")

        shinyalert::shinyalert(
          title = "Action Required",
          text  = "No break methods or time-series types selected.",
          type  = "error"
        )

        show_workflow_step("breaks")
        return(NULL)
      }

      if(!all(c("spi_smooth", "spi_mov_smooth") %in% colnames(spidf_data()))){

        #showNotification("Time series smoothing has to be performed first", type = "error")

        shinyalert::shinyalert(
          title = "Action Required",
          text  = "Time series smoothing (and maybe moving window analysis) have to be performed first",
          type  = "error"
        )

        show_workflow_step("preprocess")
        return(NULL)

      }

      output$loading <- renderUI({
        tagList(
          tags$img(src = ltm_app_asset("loading.gif"), height = "90px"),
          tags$span(paste0("Performing break analysis. ",
                           "This may take a few seconds to some minutes... Please wait \n",
                           ifelse("mcp" %in% input$break_methods,
                                  "(Bayesian Change Points Model is very slow!)", "")
          ))
        )
      })

      req(spidf_data())

      # If the user didn't select any methods or ts_name, just exit
      if (length(input$break_methods) == 0 || length(input$ts_names) == 0) {
        #showNotification("No break methods or time-series types selected.", type = "warning")

        shinyalert::shinyalert(
          title = "Action Required",
          text  = "No break methods or time-series types were selected",
          type  = "error"
        )

        show_workflow_step("breaks")
        return(NULL)
      }

      validator_args <- tryCatch(
        ltm_collect_shiny_break_validator_args(
          lt_fun_label = input$lt_fun,
          lt_window = input$lt_window,
          lt_thresh_change = input$lt_thresh_change,
          st_window = input$st_window,
          st_thresh_change = input$st_thresh_change,
          st_fun_label = input$st_fun,
          trend_window = input$trend_window,
          trend_post_pct_thresh = input$trend_post_pct_thresh,
          trend_alpha = input$trend_alpha,
          trend_deficit_tol = input$trend_deficit_tol,
          trend_min_prop_below = input$trend_min_prop_below,
          trend_avg_deficit_thresh = input$trend_avg_deficit_thresh
        ),
        error = function(error) {
          shinyalert::shinyalert(
            title = "Invalid Validator Settings",
            text = conditionMessage(error),
            type = "error"
          )
          show_workflow_step("breaks")
          set_collapse_panel("workflow_validator_settings_body", "show")
          NULL
        }
      )

      if (is.null(validator_args)) {
        return(NULL)
      }

      shinyjs::delay(1000, {

        tsb_obj <- ltm_ts_breaks(spidf_data())

        # Common parameters for all detection functions
        season_adj    <- TRUE
        s_window      <- input$s_window
        thresh_date   <- input$break_thresh_date

        ##
        ## Loop through all time series and algorithms for break detection selected
        ##
        ##
        for (tsn in input$ts_names) {
          for (method in input$break_methods) {

            run_obj <- switch(
              method,
              cpm = do.call(
                ltm_cpm_detect_breaks,
                c(
                  list(
                    spidf = spidf_data(),
                    season_adj = season_adj,
                    ts_name = tsn,
                    s_window = s_window,
                    cpm_method = params()$cpm$cpm_method,
                    ARL0 = params()$cpm$ARL0,
                    thresh_date = thresh_date
                  ),
                  validator_args
                )
              ),
              ed = do.call(
                ltm_ed_detect_breaks,
                c(
                  list(
                    spidf = spidf_data(),
                    ts_name = tsn,
                    season_adj = season_adj,
                    s_window = s_window,
                    sig_lvl = params()$ed$sig.lvl,
                    R = params()$ed$R,
                    k = params()$ed$k,
                    min_size = params()$ed$min_size,
                    alpha = params()$ed$alpha,
                    thresh_date = thresh_date
                  ),
                  validator_args
                )
              ),
              bfast = do.call(
                ltm_bfast01_detect_breaks,
                c(
                  list(
                    spidf = spidf_data(),
                    ts_name = tsn,
                    formula = stats::as.formula(params()$bfast$formula),
                    s_window = s_window,
                    test = params()$bfast$test,
                    level = params()$bfast$level,
                    aggregate = all,
                    trim = NULL,
                    bandwidth = params()$bfast$bandwidth,
                    functional = params()$bfast$functional,
                    order = params()$bfast$order,
                    thresh_date = thresh_date
                  ),
                  validator_args
                )
              ),
              mcp = do.call(
                ltm_mcp_detect_breaks,
                c(
                  list(
                    spidf = spidf_data(),
                    ts_name = tsn,
                    season_adj = season_adj,
                    s_window = s_window,
                    thresh_date = thresh_date,
                    sample = params()$mcp$sample,
                    n_chains = params()$mcp$n_chains,
                    n_cores = params()$mcp$n_cores,
                    n_adapt = params()$mcp$n_adapt,
                    n_iter = params()$mcp$n_iter,
                    downsample = params()$mcp$downsample
                  ),
                  validator_args
                )
              ),
              stc = do.call(
                ltm_strucchange_detect_breaks,
                c(
                  list(
                    spidf = spidf_data(),
                    ts_name = tsn,
                    season_adj = season_adj,
                    s_window = s_window,
                    h = params()$stc$h,
                    breaks = params()$stc$breaks,
                    thresh_date = thresh_date
                  ),
                  validator_args
                )
              ),
              wbs = do.call(
                ltm_wbs_detect_breaks,
                c(
                  list(
                    spidf = spidf_data(),
                    ts_name = tsn,
                    season_adj = season_adj,
                    s_window = s_window,
                    thresh_date = thresh_date,
                    num_intervals = params()$wbs$num_intervals
                  ),
                  validator_args
                )
              ),
              NULL
            )

            if (is.null(run_obj)) {
              next
            }

            run_obj <- ltm_apply_validator_call_labels(run_obj, validator_args)

            tsb_obj <- ltm_add_runs(tsb_obj, run_obj)
          }
        }

        # Convert the ts_breaks objects into a more readable data frame
        df_breaks <- as.data.frame(tsb_obj)
        message("[DEBUG] Analysis done. Setting breakResults.")


        # Set the break analysis reactive value
        breakResults(list(
          tsb_obj   = tsb_obj,
          df_breaks = df_breaks
        ))

        # Remove the ongoing analysis panel
        output$loading <- renderUI({ NULL })

        # Make an output appear once finishing the analysis
        output$break_analysis_status <- renderText("[OK] Break analysis complete.")
        show_workflow_step("breaks")
      })

    })

    # --------------------------------------------- #
    # ---- Table output with all break results ----
    # --------------------------------------------- #

    output$break_results_table <- renderTable({
      br <- breakResults()
      req(br)

      df <- br$df_breaks

      # Format as dates to avoid conversion to integers
      if ("break_date" %in% colnames(df)) {
        df$break_date <- format(df$break_date, "%Y-%m-%d")
      }

      display_cols <- c(
        "algorithm",
        "run_id",
        "data_type",
        "has_breaks",
        "has_valid_breaks_lt",
        "has_valid_breaks_st",
        "has_valid_breaks_st_trend",
        "break_date",
        "break_magn"
      )
      df <- df[, intersect(display_cols, names(df)), drop = FALSE]

      # Replace values for easier reading
      # Replace time series data names
      replacements <-
        c("spi" = "Original time series (daily)",
          "spi_mov_wind" = "Moving window series",
          "spi_smooth" = "Smoothed series",
          "spi_mov_smooth" = "Moving window smoothed" )
      df$data_type <- dplyr::recode(df$data_type, !!!replacements)

      # Replace model names
      da_replacements <-
        c(
          "cpm"   = "Change Point Model (cpm)",
          "ed"    = "Energy Divisive (ed)",
          "bfast" = "Break Detection Seasonal & Trend (bfast)",
          "mcp"   = "Bayesian Multiple Change Points Model (mcp)",
          "stc"   = "Structural Changes (stc)",
          "wbs"   = "Wild Binary Segmentation (wbs)"
        )
      df$algorithm <- dplyr::recode(df$algorithm, !!!da_replacements)

      bool_cols <- intersect(
        c(
          "has_breaks",
          "has_valid_breaks_lt",
          "has_valid_breaks_st",
          "has_valid_breaks_st_trend"
        ),
        names(df)
      )
      for (col_name in bool_cols) {
        df[[col_name]] <- ifelse(df[[col_name]], "Yes", "No")
      }

      rename_map <- c(
        algorithm = "Algorithm",
        run_id = "Run ID",
        data_type = "Data type",
        has_breaks = "Breaks found?",
        has_valid_breaks_lt = "Long-term valid?",
        has_valid_breaks_st = "Short-term valid?",
        has_valid_breaks_st_trend = "Short-term trend valid?",
        break_date = "Break date",
        break_magn = "Break change"
      )
      colnames(df) <- unname(rename_map[names(df)])

      df
    })

    ## Download button

    output$download_breaks_ui <- renderUI({
      br <- breakResults()
      req(br)

      downloadButton("download_breaks_csv", "Download breaks to CSV")
    })

    output$download_breaks_csv <- downloadHandler(
      filename = function() {
        paste0("BreakResults_",format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        # Safely retrieve the breaks data frame
        br <- breakResults()
        req(br)  # Ensure breakResults() is not NULL
        df <- br$df_breaks

        # Write the CSV
        utils::write.csv(df, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )


    output$valid_breaks_plots <- renderPlot({
      # 1) Access the results
      br <- breakResults()
      req(br)  # makes sure breakResults() is not NULL
      df <- br$df_breaks

      # 2) Call the function

      if(any(df$has_valid_breaks_lt == TRUE)){
        plot_valid_breaks(df)
      }else{
        return(NULL)
      }
    })


    output$break_summary_ui <- renderUI({

      br <- breakResults()
      req(br)

      txt <- ltm_summarize_break_df(br$df_breaks)
      div(
        style = "background-color: #f9f9f9; margin:20px; border: 1px solid #ccc;
             padding: 15px; border-radius: 8px; font-size: 14px;
             font-family: monospace; white-space: pre-wrap;",
        txt
      )
    })

  }

  app <- shiny::shinyApp(ui = ui, server = server)
  app
}


#' Run the LargeTreeMonitoring Shiny app
#'
#' Launches the packaged Shiny application for analysing satellite-image time
#' series and breakpoints.
#'
#' @param config_path Optional path to a JSON parameter file. When omitted, the
#'   package uses the user-specific configuration file in the standard R user
#'   config directory, creating it from the default template when needed.
#' @param ... Arguments passed to [shiny::runApp()].
#' @export
#'
run_ltm_app <- function(config_path = NULL, ...) {
  shiny::runApp(ltm_app(config_path = config_path), ...)
}



