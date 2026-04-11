
ltm_default_params_path <- function() {
  system.file(
    "extdata", "params-default.json",
    package = "LargeTreeMonitoring",
    mustWork = TRUE
  )
}

ltm_default_csv_path <- function() {
  system.file(
    "extdata", "_SAMPLE_TREE_LIST_.csv",
    package = "LargeTreeMonitoring",
    mustWork = TRUE
  )
}

ltm_config_dir <- function() {
  path <- tools::R_user_dir("LargeTreeMonitoring", which = "config")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

ltm_params_path <- function() {
  file.path(ltm_config_dir(), "params.json")
}

ltm_init_params <- function(overwrite = FALSE) {
  target <- ltm_params_path()

  if (overwrite || !file.exists(target)) {
    file.copy(ltm_default_params_path(), target, overwrite = overwrite)
  }

  target
}

ltm_read_params <- function(path = ltm_params_path()) {
  jsonlite::read_json(path, simplifyVector = TRUE)
}

ltm_write_params <- function(params, path = ltm_params_path()) {
  jsonlite::write_json(
    params,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  invisible(path)
}

ltm_is_writable_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.exists(path) && file.access(path, mode = 2) == 0
}

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

ltm_app_resource_prefix <- "ltm-www"

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

ltm_app_asset <- function(file_name) {
  paste0(ltm_app_resource_prefix, "/", file_name)
}

ltm_has_path_value <- function(path) {
  !is.null(path) && length(path) == 1L && !is.na(path) && nzchar(path)
}

ltm_selected_tree_id <- function(location_id) {
  if (!ltm_has_path_value(location_id) || identical(location_id, "---")) {
    return(NULL)
  }

  as.character(location_id)
}

ltm_validator_fun_choices <- function() {
  c("Median", "90% percentile")
}

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

ltm_collect_shiny_break_validator_args <- function(
    lt_fun_label,
    lt_window,
    lt_thresh_change,
    st_window,
    st_thresh_change,
    st_fun_label,
    trend_window,
    trend_post_pct_thresh,
    trend_alpha
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
    )
  )

  attr(out, "call_labels") <- list(
    lt_fun = lt_fun_call,
    st_fun = st_fun_call
  )

  out
}

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

ltm_is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", path)
}

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

ltm_update_tree_choices <- function(session, tree_state, selected = "---") {
  shiny::updateSelectInput(
    session = session,
    inputId = "location_id",
    choices = tree_state$choices,
    selected = selected
  )
}

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
      .resizable-sidebar {
        resize: horizontal;
        overflow: auto;
        min-width: 200px;
        max-width: 450px;
        padding-right: 10px;
        border-right: 1px solid #ccc;
        background-color: #4a5e68;
        color: #1b4040;
      }
      .main-panel {
        padding-left: 20px;
      }
      .form-group, .shiny-input-container {
        margin-bottom: 8px;
        font-size: 13px;
      }
      .shiny-input-label,
      .control-label,
      .checkbox label,
      .radio label {
        color: #1b4040 !important;
        font-weight: 500;
      }
      .form-control, .selectize-input {
        background-color: #e0ebeb !important;
        color: #2c3e50 !important;
        border: 1px solid #95a5a6;
        font-size: 13px;
        height: 32px;
      }
      .form-control:focus,
      .selectize-input:focus {
        border-color: #6ba292;
        box-shadow: none;
      }
      .selectize-dropdown-content {
        background-color: #f4f6f6;
        color: #2c3e50;
      }
      .btn {
        font-size: 13px;
        padding: 6px 10px;
        background-color: #6ba292;
        color: #ffffff;
        border: none;
        font-weight: bold;
        margin-top: 4px;
      }
      .btn:hover {
        background-color: #5a8d7c;
      }
      .ltm-banner {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        background-color: #354b5e;
        color: #ffffff;
        padding: 10px 20px;
        z-index: 1000;
        border-bottom: 1px solid #ccc;
      }
      .ltm-banner h2 {
        margin: 3px;
        padding: 3px;
        font-size: 25px;
        color: #ffffff;
      }
      body {
        padding-top: 70px;
      }

      .scrollable-sidebar {
        position: fixed;
        top: 70px; /* height of your banner */
        bottom: 0;
        left: 0;
        width: 350px;
        overflow-y: auto;
        overflow-x: hidden;
        z-index: 999;
      }

      .main-panel {
        margin-left: 330px; /* leave space for fixed sidebar */
        padding: 20px;
      }

    "))
    ),

    shiny::div(
      class = "ltm-banner",
      shiny::div(style = "display: flex; align-items: center;",
          tags$img(src = ltm_app_asset("LTM_logo-v1.png"), height = "60px", style = "margin-right: 5px;"),
          h2("Large Tree Monitoring")
      )
    ),


    shiny::fluidRow(
      shiny::div(
        class = "resizable-sidebar scrollable-sidebar",
        shiny::sidebarPanel(
          width = 12,  # use full width inside the resizable container

          # Google Earth Engine input
          #textInput("user_name", "GEE User Name", value = ""),

          # Location or Tree ID from pre-existing list
          shiny::selectInput("location_id", "Select Location ID",
                      choices = tree_ids, selectize = TRUE, selected = "---"),
          uiOutput("tree_list_warning"),

          shiny::numericInput("latitude", "Latitude", value = 41.720898),
          shiny::numericInput("longitude", "Longitude", value = -8.747039),

          shiny::actionButton("choose_coords", "",
                       icon = shiny::icon("map-location-dot"),
                       style = "margin-bottom: 8px;"),

          shiny::dateInput("start_date", "Start Date", value = "2015-01-01"),
          shiny::dateInput("end_date", "End Date", value = "2024-12-31"),
          shiny::selectInput("spi", "Spectral Index", choices = c("NDVI", "EVI", "EVI2", "NBR", "NDRE")),
          shiny::selectInput("proc_level", "Processing Level", choices = c("L1C","L2A")),
          shiny::actionButton("fetch_data", "Fetch Data"),

          # Data regularization
          shiny::hr(),
          shiny::selectInput("regularize_method", "Regularization Method",
                      choices = c("linear", "spline", "stine",
                                  "mean", "kalman", "locf", "nocb")),
          shiny::checkboxInput("use_cloud_mask", "Use Cloud Mask", value = TRUE),

          shiny::actionButton("regularize", "Regularize/interpolate Time Series"),

          # Moving window analysis
          shiny::hr(),

          shiny::selectInput("quant", "Moving Window Quantile",
                      choices = c(0.95, 0.9, 0.75)),
          shiny::numericInput("win_size", "Window Size (days)",
                       value = 15, min = 5, max = 180),

          shiny::actionButton("apply_mov_quantile", "Apply Moving Window"),

          # Whittaker time series smoothing
          shiny::hr(),
          shiny::numericInput("lambda", "Whittaker Lambda",
                       value = 20000, min = 1),
          shiny::numericInput("quantile_thresh", "Whittaker Quantile Threshold",
                       value = 0.35, min = 0.01, max = 1),
          shiny::checkboxInput("use_weights", "Use Weights in Whittaker Smoothing", value = TRUE),

          shiny::actionButton("apply_whittaker", "Apply Whittaker Smoother"),


          # Break Detection component
          # -- NEW: Break Detection UI inputs --
          shiny::hr(),
          shiny::checkboxGroupInput(
            "ts_names", "Select Time Series Type(s)",
            choices = c(

              "Original time series (daily)" = "spi",
              "Moving window series"         = "spi_mov_wind",
              "Smoothed series"              = "spi_smooth",
              "Moving window smoothed"       = "spi_mov_smooth"),
            selected = c("spi")  # Default selection
          ),

          shiny::numericInput("s_window", "Window size (STL decomposition)",
                       value = 30, min = 5, max = 365),

          shiny::checkboxGroupInput(
            inputId = "break_methods",
            label   = "Break-Detection Methods",

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

          shiny::dateInput("break_thresh_date", "Break Threshold Date",
                    value = "2016-01-01"),
          shiny::div(style = "font-weight: 600; margin-top: 8px;", "Long-term validator"),
          shiny::numericInput(
            "lt_window",
            "Long-term window size (observations per side, 0 uses full series)",
            value = 0,
            min = 0,
            max = 365,
            step = 1
          ),
          shiny::numericInput("lt_thresh_change", "Long-term % change threshold",
                       value = -10, max = 0),

          shiny::radioButtons(
            inputId = "lt_fun",
            label   = "Long-term aggregation function",
            choices = ltm_validator_fun_choices(),
            selected = "Median"),

          shiny::div(style = "font-weight: 600; margin-top: 8px;", "Short-term validator"),
          shiny::numericInput(
            "st_window",
            "Short-term window size (observations per side, 0 disables)",
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
          ),
          shiny::radioButtons(
            inputId = "st_fun",
            label   = "Short-term aggregation function",
            choices = ltm_validator_fun_choices(),
            selected = "Median"
          ),

          shiny::div(style = "font-weight: 600; margin-top: 8px;", "Short-term trend validator"),
          shiny::numericInput(
            "trend_window",
            "Trend window size (observations per side, 0 disables)",
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

          shiny::actionButton("analyze_breaks", "Analyze Breaks"),

          ## Parameter editing
          shiny::hr(),
          shiny::p("Configure parameters"),
          shiny::actionButton("edit_params", "Edit parameters"),
          shiny::verbatimTextOutput("param_save_status")


        )
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



  server <- function(input, output, session) {

    params <- shiny::reactiveVal(startup$params)
    tree_state <- shiny::reactiveVal(startup$tree_state)
    spidf_data <- reactiveVal(NULL)

    # A new reactiveVal to store the actual lat/lon used in the last fetch
    selectedCoord <- reactiveVal(NULL)

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

        updateNumericInput(session, "latitude", value = coords$lat)
        updateNumericInput(session, "longitude", value = coords$lon)

        selectedCoord(list(lat = coords$lat, lon = coords$lon))
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
            date_error <- TRUE
          }
          if ((input$proc_level %in% c("L2","L2A")) && (input$start_date < as.Date("2017-01-01"))) {
            shinyalert::shinyalert(
              title = "Error in start date",
              text = "The start date cannot be earlier than 2017-01-01 for L2/L2A Sentinel-2 data",
              type = "error"
            )
            output$loading <- renderUI({ NULL })
            date_error <- TRUE
          }

          if(!date_error) {
            selected_tree_id <- ltm_selected_tree_id(input$location_id)
            requested_buffer_radius <- NULL
            cache_status_detail <- NULL
            location_ready <- TRUE
            cache_hit <- FALSE

            # Use either chosen location or manual lat/lon
            if (!is.null(selected_tree_id)) {
              tree_info <- tree_state()
              tree_list <- tree_info$data

              if (is.null(tree_list)) {
                shinyalert::shinyalert(
                  title = "Location list unavailable",
                  text = "The configured tree list could not be loaded.",
                  type = "error"
                )
                output$loading <- renderUI({ NULL })
                location_ready <- FALSE
              }

              if (isTRUE(location_ready)) {
                loc_row <- tree_list[
                  as.character(tree_list[[tree_info$tree_ids_col]]) == selected_tree_id,
                  ,
                  drop = FALSE
                ]

                if (nrow(loc_row) == 1) {
                  lat <- loc_row[[tree_info$lat_col]][1]
                  lon <- loc_row[[tree_info$lon_col]][1]

                } else {
                  shinyalert::shinyalert(
                    title = "Error in start date",
                    text = "Selected location ID not found in list",
                    type = "error"
                  )

                  output$loading <- renderUI({ NULL })
                  location_ready <- FALSE
                }
              }
            } else {
              lat <- input$latitude
              lon <- input$longitude
            }

            if (isTRUE(location_ready)) {
              # Store the lat/lon used (for the map, etc.)
              selectedCoord(list(lat = lat, lon = lon))


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
                  output$loading <- renderUI({NULL})
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

                    output$loading <- renderUI({NULL})
                  }, error = function(error) {
                    shinyalert::shinyalert(
                      title = "Error fetching time series",
                      text = error$message,
                      type = "error"
                    )
                    output$status <- renderText(paste("[ERROR]", error$message))
                    output$loading <- renderUI({NULL})
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
          NULL
        })
      })
    })

    # ---- Regularize logic ----
    observeEvent(input$regularize, {
      req(spidf_data())
      spidf_data(
        ltm_regularize_spidf(
          spidf_data(),
          method         = input$regularize_method,
          use_cloud_mask = input$use_cloud_mask
        )
      )
      output$status <- renderText("[OK] Time series regularized.")
    })

    # ---- Moving window quantile ----
    observeEvent(input$apply_mov_quantile, {

      req(spidf_data())

      if (!isTRUE(is_regularized(spidf_data()))) {
        shinyalert::shinyalert(
          title = "Action Required",
          text  = "Please regularize/interpolate the time series before
        applying the moving window analysis.",
          type  = "error"
        )
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

      output$status <- renderText("[OK] Moving window quantile applied.")
    })

    # ---- Whittaker smoother ----
    observeEvent(input$apply_whittaker, {
      req(spidf_data())
      spidf_data(
        ltm_apply_whitaker(
          spidf_data(),
          lambda          = input$lambda,
          quantile_thresh = input$quantile_thresh,
          use_weights     = input$use_weights
        )
      )
      output$status <- renderText("[OK] Whittaker smoother applied.")
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

      ltm_plot_spidf_ts(
        spidf_obj = spidf_data(),
        tree_id   = input$location_id,
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

      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        setView(lng = coords$lon, lat = coords$lat, zoom = 13) %>%
        addMarkers(lng = coords$lon, lat = coords$lat, popup = "Tree Location") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # A reactive to hold the aggregated results (ts_breaks object + data frame)
    breakResults <- reactiveVal(NULL)


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
      message("[DEBUG] spidf_data rows: ", nrow(spidf_data()))  # or however your spidf is structured

      if (length(input$break_methods) == 0 || length(input$ts_names) == 0) {
        #showNotification("No break methods or time-series types selected.", type = "error")
        #message("[DEBUG] => no break methods or no timeseries, returning NULL.")

        shinyalert::shinyalert(
          title = "Action Required",
          text  = "No break methods or time-series types selected.",
          type  = "error"
        )

        return(NULL)
      }

      if(!all(c("spi_smooth", "spi_mov_smooth") %in% colnames(spidf_data()))){

        #showNotification("Time series smoothing has to be performed first", type = "error")

        shinyalert::shinyalert(
          title = "Action Required",
          text  = "Time series smoothing (and maybe moving window analysis) have to be performed first",
          type  = "error"
        )

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
          trend_alpha = input$trend_alpha
        ),
        error = function(error) {
          shinyalert::shinyalert(
            title = "Invalid Validator Settings",
            text = conditionMessage(error),
            type = "error"
          )
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



