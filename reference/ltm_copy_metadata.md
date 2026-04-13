# Copy `spidf` metadata attributes

Copies the metadata attributes used by LargeTreeMonitoring from one
object to another. This includes spatial coordinates, requested and
available date ranges, spectral-index metadata, buffer settings, and
preprocessing flags. The function does not validate that `x` and `from`
contain compatible rows or time-series values.

## Usage

``` r
ltm_copy_metadata(x, from)
```

## Arguments

- x:

  Object that will receive metadata attributes.

- from:

  Object, normally an `spidf`, from which metadata attributes are read
  through the package accessors.

## Value

Object `x` with copied attributes.

## See also

Other metadata helpers:
[`get_latitude()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_accessors.md),
[`set_latitude()`](http://joaogoncalves.cc/LargeTreeMonitoring/reference/spidf_setters.md)
