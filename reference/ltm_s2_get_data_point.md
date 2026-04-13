# Extract a Sentinel-2 spectral index time series for a point or buffer

Retrieves a Sentinel-2 time series from Google Earth Engine for a target
location, computes the selected spectral index, applies a binary cloud
mask, and returns the result as an object of class `spidf`.

## Usage

``` r
ltm_s2_get_data_point(
  lat,
  lon,
  start_date,
  end_date,
  spi = "NDVI",
  proc_level = "L2A",
  crs_code = "EPSG:4326",
  rm_duplicates = TRUE,
  tree_id = NULL,
  use_buffer = FALSE,
  buffer_radius_m = NULL,
  cloud_mask_threshold = 0.5
)
```

## Arguments

- lat:

  Numeric. Latitude of the target location.

- lon:

  Numeric. Longitude of the target location.

- start_date:

  Character or Date. Start date of the extraction period.

- end_date:

  Character or Date. End date of the extraction period.

- spi:

  Character. Spectral index to compute. Must be one of
  "NDVI","EVI","NBR","NDRE","EVI2"

- proc_level:

  Character. Sentinel-2 processing level to use. Must be one of
  "L1","L1C","L2","L2A"

- crs_code:

  Character. CRS code passed to Earth Engine during reduction.

- rm_duplicates:

  Logical. If `TRUE`, duplicate observations are removed with
  `ltm_remove_duplicates()`.

- tree_id:

  Optional identifier associated with the target tree or location.
  Stored as an attribute in the output.

- use_buffer:

  Logical. If `TRUE`, values are extracted from a buffer around the
  point.

- buffer_radius_m:

  Numeric. Buffer radius in meters. Required when `use_buffer = TRUE`.

- cloud_mask_threshold:

  Numeric. Threshold used to binarize the mean cloud mask; values
  greater than or equal to this threshold are treated as cloudy.

## Value

An object of class `spidf` with columns `id`, `ti`, `masked_vals`,
`spi`, and `cloud_mask`, plus metadata stored as attributes.
