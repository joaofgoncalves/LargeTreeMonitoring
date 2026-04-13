# Compute NDRE and attach a binary cloud mask

Computes the normalized difference red-edge index (NDRE) from the `NIR`
and `RE1` bands of a Sentinel-2 image and adds a binary cloud mask
derived from the `QA60` band. The output preserves the original image
properties and acquisition time.

## Usage

``` r
ltm_calc_ndre(img)
```

## Arguments

- img:

  An `ee$Image` object representing a Sentinel-2 image.

## Value

An `ee$Image` with two bands: `NDRE` and `cloud_mask`.
