# Convert a list of Earth Engine features to a data frame

Converts a list of Earth Engine feature objects into a data frame by
extracting their properties. Ensures that a `system.id` field is present
when available and standardizes column names.

## Usage

``` r
ltm_ee_features_to_df(features)
```

## Arguments

- features:

  A list of Earth Engine feature objects.

## Value

A data frame where each row corresponds to a feature and columns
correspond to feature properties. Returns an empty data frame if input
is `NULL` or empty.
