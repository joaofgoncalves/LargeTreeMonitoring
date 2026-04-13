# Get metadata from an `spidf` object

These S3 generics and methods read metadata attributes stored on `spidf`
objects returned by
[`ltm_s2_get_data_point()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_s2_get_data_point.md)
and preprocessing helpers. Accessors are grouped by metadata type:
`get_latitude()`, `get_longitude()`, `get_start_date()`,
`get_end_date()`, `get_range_start()`, `get_range_end()`,
`get_tree_id()`, `get_use_buffer()`, `get_buffer_radius_m()`,
`get_cloud_mask_threshold()`, `get_spi()`, `get_proc_level()`,
`get_crs_code()`, `is_regularized()`, `get_regularize_method()`,
`has_mov_window()`, `get_mov_window_quantile()`,
`get_mov_window_size_days()`, `is_whit_smoothed()`, `get_whit_lambda()`,
`get_whit_quantile_threshold()`, and `get_whit_weights_used()`.

## Usage

``` r
get_latitude(x)

# S3 method for class 'spidf'
get_latitude(x)

get_longitude(x)

# S3 method for class 'spidf'
get_longitude(x)

get_start_date(x)

# S3 method for class 'spidf'
get_start_date(x)

get_end_date(x)

# S3 method for class 'spidf'
get_end_date(x)

get_spi(x)

# S3 method for class 'spidf'
get_spi(x)

get_proc_level(x)

# S3 method for class 'spidf'
get_proc_level(x)

get_crs_code(x)

# S3 method for class 'spidf'
get_crs_code(x)

is_regularized(x)

# S3 method for class 'spidf'
is_regularized(x)

get_regularize_method(x)

# S3 method for class 'spidf'
get_regularize_method(x)

has_mov_window(x)

# S3 method for class 'spidf'
has_mov_window(x)

is_whit_smoothed(x)

# S3 method for class 'spidf'
is_whit_smoothed(x)

get_mov_window_quantile(x)

# S3 method for class 'spidf'
get_mov_window_quantile(x)

get_mov_window_size_days(x)

# S3 method for class 'spidf'
get_mov_window_size_days(x)

get_whit_lambda(x)

# S3 method for class 'spidf'
get_whit_lambda(x)

get_whit_quantile_threshold(x)

# S3 method for class 'spidf'
get_whit_quantile_threshold(x)

get_whit_weights_used(x)

# S3 method for class 'spidf'
get_whit_weights_used(x)

get_range_start(x)

# S3 method for class 'spidf'
get_range_start(x)

get_range_end(x)

# S3 method for class 'spidf'
get_range_end(x)

get_tree_id(x)

# S3 method for class 'spidf'
get_tree_id(x)

get_use_buffer(x)

# S3 method for class 'spidf'
get_use_buffer(x)

get_buffer_radius_m(x)

# S3 method for class 'spidf'
get_buffer_radius_m(x)

get_cloud_mask_threshold(x)

# S3 method for class 'spidf'
get_cloud_mask_threshold(x)
```

## Arguments

- x:

  An object, typically of class `spidf`.

## Value

The requested metadata attribute. Date accessors return `Date` values
where the method coerces the stored attribute; logical state checkers
return logical values; remaining accessors return the stored numeric or
character metadata value.

## See also

Other metadata helpers:
[`ltm_copy_metadata()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_copy_metadata.md),
[`set_latitude()`](https://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
