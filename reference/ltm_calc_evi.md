# Compute EVI and attach a binary cloud mask

Computes the enhanced vegetation index (EVI) from the `NIR`, `Red`, and
`Blue` bands of a Sentinel-2 image and adds a binary cloud mask derived
from the `QA60` band. The output preserves the original image properties
and acquisition time.

## Usage

``` r
ltm_calc_evi(img)
```

## Arguments

- img:

  An `ee$Image` object representing a Sentinel-2 image.

## Value

An `ee$Image` with two bands: `EVI` and `cloud_mask`.
