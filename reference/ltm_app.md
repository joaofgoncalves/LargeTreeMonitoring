# Create the LargeTreeMonitoring Shiny app

Creates the packaged Shiny application for analysing satellite-image
time series and breakpoints.

## Usage

``` r
ltm_app(config_path = NULL)
```

## Arguments

- config_path:

  Optional path to a JSON parameter file. When omitted, the package uses
  the user-specific configuration file in the standard R user config
  directory, creating it from the default template when needed.

## Value

A `shiny.appobj` that can be launched with
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
