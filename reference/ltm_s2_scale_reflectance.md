# Scale Sentinel-2 reflectance bands and retain QA information

Applies a scaling factor to Sentinel-2 reflectance bands (multiplying by
0.0001) and converts them to floating point values. The function
preserves the `QA60` quality band and copies all image properties,
including `system:time_start`, to the output image.

## Usage

``` r
ltm_s2_scale_reflectance(img)
```

## Arguments

- img:

  An `ee$Image` object representing a Sentinel-2 image.

## Value

An `ee$Image` with scaled reflectance bands, the original `QA60` band,
and preserved metadata.
