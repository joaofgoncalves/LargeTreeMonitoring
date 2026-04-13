# Set metadata on an `spidf` object

These S3 generics and methods update metadata attributes stored on
`spidf` objects. Setters return the modified object and do not perform
full consistency checks across attributes or data columns. Date setters
coerce values with
[`zoo::as.Date()`](https://rdrr.io/pkg/zoo/man/yearmon.html), numeric
setters coerce with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html), integer setters
coerce with [`as.integer()`](https://rdrr.io/r/base/integer.html), and
logical setters coerce with
[`as.logical()`](https://rdrr.io/r/base/logical.html) where shown by the
method implementation.

## Usage

``` r
set_latitude(x, value)

# S3 method for class 'spidf'
set_latitude(x, value)

set_longitude(x, value)

# S3 method for class 'spidf'
set_longitude(x, value)

set_start_date(x, value)

# S3 method for class 'spidf'
set_start_date(x, value)

set_end_date(x, value)

# S3 method for class 'spidf'
set_end_date(x, value)

set_spi(x, value)

# S3 method for class 'spidf'
set_spi(x, value)

set_proc_level(x, value)

# S3 method for class 'spidf'
set_proc_level(x, value)

set_crs_code(x, value)

# S3 method for class 'spidf'
set_crs_code(x, value)

set_regularized(x, value)

# S3 method for class 'spidf'
set_regularized(x, value)

set_regularize_method(x, value)

# S3 method for class 'spidf'
set_regularize_method(x, value)

set_mov_window(x, value)

# S3 method for class 'spidf'
set_mov_window(x, value)

set_whit_smoothed(x, value)

# S3 method for class 'spidf'
set_whit_smoothed(x, value)

set_mov_window_quantile(x, value)

# S3 method for class 'spidf'
set_mov_window_quantile(x, value)

set_mov_window_size_days(x, value)

# S3 method for class 'spidf'
set_mov_window_size_days(x, value)

set_whit_lambda(x, value)

# S3 method for class 'spidf'
set_whit_lambda(x, value)

set_whit_quantile_threshold(x, value)

# S3 method for class 'spidf'
set_whit_quantile_threshold(x, value)

set_whit_weights_used(x, value)

# S3 method for class 'spidf'
set_whit_weights_used(x, value)

set_range_start(x, value)

# S3 method for class 'spidf'
set_range_start(x, value)

set_range_end(x, value)

# S3 method for class 'spidf'
set_range_end(x, value)

set_tree_id(x, value)

# S3 method for class 'spidf'
set_tree_id(x, value)

set_use_buffer(x, value)

# S3 method for class 'spidf'
set_use_buffer(x, value)

set_buffer_radius_m(x, value)

# S3 method for class 'spidf'
set_buffer_radius_m(x, value)

set_cloud_mask_threshold(x, value)

# S3 method for class 'spidf'
set_cloud_mask_threshold(x, value)
```

## Arguments

- x:

  An object, typically of class `spidf`.

- value:

  Replacement metadata value.

## Value

The modified object `x`.

## See also

Other metadata helpers:
[`get_latitude()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md),
[`ltm_copy_metadata()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/ltm_copy_metadata.md)
