# Derive a binary cloud mask from a Sentinel-2 image

Extracts cloud information from the `QA60` quality band of a Sentinel-2
image and creates a binary cloud mask. Pixels flagged as clouds or
cirrus (bits 10 and 11) are assigned a value of 1, and clear-sky pixels
are assigned a value of 0.

## Usage

``` r
ltm_s2_clouds(img)
```

## Arguments

- img:

  An `ee$Image` object representing a Sentinel-2 image.

## Value

An `ee$Image` containing a single band named `"cloud_mask"` with binary
values (1 = cloud/cirrus, 0 = clear), preserving the original image
properties and timestamp.
