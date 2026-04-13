# Compute EVI2 and attach a binary cloud mask

Computes the two-band enhanced vegetation index (EVI2) from the `NIR`
and `Red` bands of a Sentinel-2 image and adds a binary cloud mask
derived from the `QA60` band. The output preserves the original image
properties and acquisition time.

## Usage

``` r
ltm_calc_evi2(img)
```

## Arguments

- img:

  An `ee$Image` object representing a Sentinel-2 image.

## Value

An `ee$Image` with two bands: `EVI2` and `cloud_mask`.
