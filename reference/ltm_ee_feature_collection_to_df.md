# Convert an Earth Engine FeatureCollection to a data frame

Retrieves a FeatureCollection from Google Earth Engine, checks its size
against a maximum threshold, and converts its features into a data
frame.

## Usage

``` r
ltm_ee_feature_collection_to_df(x_fc, max_features = 10000L)
```

## Arguments

- x_fc:

  An `ee$FeatureCollection` object.

- max_features:

  Integer. Maximum number of features allowed to be retrieved. Defaults
  to 10000.

## Value

A data frame where each row corresponds to a feature and columns
correspond to feature properties.
