# Mask clouds and cirrus in a Sentinel-2 image

Applies a cloud mask to a Sentinel-2 image using the `QA60` quality
band. Pixels flagged as clouds or cirrus (bits 10 and 11) are masked
out, retaining only clear-sky observations.

## Usage

``` r
ltm_s2_mask_clouds(img)
```

## Arguments

- img:

  An `ee$Image` object representing a Sentinel-2 image.

## Value

An `ee$Image` with cloud- and cirrus-contaminated pixels masked.
