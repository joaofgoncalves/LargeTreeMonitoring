# Run the LargeTreeMonitoring Shiny app

Launches the packaged Shiny application for analysing satellite-image
time series and breakpoints.

## Usage

``` r
run_ltm_app(config_path = NULL, ...)
```

## Arguments

- config_path:

  Optional path to a JSON parameter file. When omitted, the package uses
  the user-specific configuration file in the standard R user config
  directory, creating it from the default template when needed.

- ...:

  Arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
