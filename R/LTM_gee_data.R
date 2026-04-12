
#' Scale Sentinel-2 reflectance bands and retain QA information
#'
#' Applies a scaling factor to Sentinel-2 reflectance bands (multiplying by
#' 0.0001) and converts them to floating point values. The function preserves
#' the `QA60` quality band and copies all image properties, including
#' `system:time_start`, to the output image.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` with scaled reflectance bands, the original `QA60` band, and
#' preserved metadata.
#'
#' @keywords internal
#'
ltm_s2_scale_reflectance <- function(img) {

  refl_bands <- c("Blue", "Green", "Red", "RE1", "RE2", "RE3",
                  "NIR", "RE4", "SWIR1", "SWIR2")

  scaled_refl <- img$select(refl_bands)$multiply(0.0001)$toFloat()

  out <- scaled_refl$addBands(img$select("QA60"))

  out <- out$copyProperties(img, img$propertyNames())$
    set("system:time_start", img$get("system:time_start"))

  return(ee$Image(out))
}


#' Get a Sentinel-2 image collection from Google Earth Engine
#'
#' Returns a harmonized Sentinel-2 image collection from Google Earth Engine
#' for the requested processing level and renames the selected bands to
#' standardized names used by the package.
#'
#' @param proc_level Character. Sentinel-2 processing level to use. Supported
#'   values are `"L2A"` or `"L2"` for surface reflectance, and `"L1C"` or
#'   `"L1"` for top-of-atmosphere reflectance.
#'
#' @return
#' An `ee$ImageCollection` with selected and renamed Sentinel-2 bands.
#'
#' @keywords internal
#'
ltm_get_s2_imgcol <- function(proc_level = "L2A") {

  ## ----------------------------------------------------------------------- ##
  ## Sentinel-2a/b mission ----
  ## ----------------------------------------------------------------------- ##

  if (proc_level %in% c("L2A", "L2")) {

    # Harmonized Sentinel-2 MSI: MultiSpectral Instrument, Level-2A
    imCol <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED") %>%
      ee$ImageCollection$select(c("B2", "B3", "B4", "B5", "B6", "B7",
                                  "B8", "B8A", "B11", "B12", "QA60"),
                                # Renamed bands
                                c("Blue", "Green", "Red", "RE1", "RE2", "RE3",
                                  "NIR", "RE4", "SWIR1", "SWIR2", "QA60"))
    return(imCol)

  } else if (proc_level %in% c("L1C", "L1")) {

    # Harmonized Sentinel-2 MSI: MultiSpectral Instrument, Level-1C
    imCol <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>%
      ee$ImageCollection$select(c("B2","B3","B4","B5","B6","B7",
                                  "B8","B8A","B11","B12","QA60"),
                                # Renamed bands
                                c("Blue", "Green", "Red","RE1","RE2",
                                  "RE3","NIR","RE4","SWIR1", "SWIR2","QA60"))
    return(imCol)

  } else {
    stop("Sentinel-2 - Processing level value in proc_level is not supported")
  }
}


#' Mask clouds and cirrus in a Sentinel-2 image
#'
#' Applies a cloud mask to a Sentinel-2 image using the `QA60` quality band.
#' Pixels flagged as clouds or cirrus (bits 10 and 11) are masked out,
#' retaining only clear-sky observations.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` with cloud- and cirrus-contaminated pixels masked.
#'
#' @keywords internal
#'
ltm_s2_mask_clouds <- function(img){

  # Select quality layer
  qa <- img$select("QA60")

  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11

  # Both flags should be set to zero, indicating clear conditions.
  mask <- ((qa$bitwiseAnd(cloudBitMask))$eq(0))$And(qa$bitwiseAnd(cirrusBitMask)$eq(0))

  return(ee$Image(img$updateMask(mask)))
}


#' Derive a binary cloud mask from a Sentinel-2 image
#'
#' Extracts cloud information from the `QA60` quality band of a Sentinel-2
#' image and creates a binary cloud mask. Pixels flagged as clouds or cirrus
#' (bits 10 and 11) are assigned a value of 1, and clear-sky pixels are
#' assigned a value of 0.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` containing a single band named `"cloud_mask"` with binary
#'   values (1 = cloud/cirrus, 0 = clear), preserving the original image
#'   properties and timestamp.
#'
#' @keywords internal
#'
ltm_s2_clouds <- function(img){

  # Select QA60 band
  qa <- img$select("QA60")

  # Define bit masks for clouds and cirrus
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11

  # Identify cloud pixels (clouds or cirrus)
  cloud_mask <- qa$bitwiseAnd(cloudBitMask)$neq(0)$Or(qa$bitwiseAnd(cirrusBitMask)$neq(0))

  # Create binary mask: 1 for cloud pixels, 0 for clear pixels
  binary_cloud_mask <- cloud_mask$rename("cloud_mask")$uint8()

  # Preserve original image properties and timestamp
  return(binary_cloud_mask$copyProperties(img, img$propertyNames())$set('system:time_start',
                                                                        img$get('system:time_start')))

}


#' Compute NDVI and attach a binary cloud mask
#'
#' Computes the normalized difference vegetation index (NDVI) from the `NIR`
#' and `Red` bands of a Sentinel-2 image and adds a binary cloud mask derived
#' from the `QA60` band. The output preserves the original image properties and
#' acquisition time.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` with two bands: `NDVI` and `cloud_mask`.
#'
#' @keywords internal
#'
ltm_calc_ndvi <- function(img) {

  ndvi <- img$normalizedDifference(c('NIR', 'Red'))$rename('NDVI')
  ndvi <- ndvi$copyProperties(img, img$propertyNames())$set('system:time_start',
                                                            img$get('system:time_start'))

  qa <- img$select("QA60")
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  cloud_mask <- qa$bitwiseAnd(cloudBitMask)$neq(0)$Or(qa$bitwiseAnd(cirrusBitMask)$neq(0))
  binary_cloud_mask <- cloud_mask$rename("cloud_mask")$uint8()
  binary_cloud_mask <- binary_cloud_mask$copyProperties(img, img$propertyNames())$set('system:time_start',
                                                                                      img$get('system:time_start'))

  return(ee$Image(ndvi)$addBands(ee$Image(binary_cloud_mask)))
}


#' Compute NBR and attach a binary cloud mask
#'
#' Computes the normalized burn ratio (NBR) from the `NIR` and `SWIR2` bands of
#' a Sentinel-2 image and adds a binary cloud mask derived from the `QA60`
#' band. The output preserves the original image properties and acquisition
#' time.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` with two bands: `NBR` and `cloud_mask`.
#'
#' @keywords internal
#'
ltm_calc_nbr <- function(img) {

  nbr <- img$normalizedDifference(c('NIR', 'SWIR2'))$rename('NBR')
  nbr <- nbr$copyProperties(img, img$propertyNames())$set('system:time_start', img$get('system:time_start'))

  qa <- img$select("QA60")
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  cloud_mask <- qa$bitwiseAnd(cloudBitMask)$neq(0)$Or(qa$bitwiseAnd(cirrusBitMask)$neq(0))
  binary_cloud_mask <- cloud_mask$rename("cloud_mask")$uint8()
  binary_cloud_mask <- binary_cloud_mask$copyProperties(img, img$propertyNames())$set('system:time_start',
                                                                                      img$get('system:time_start'))

  return(ee$Image(nbr)$addBands(ee$Image(binary_cloud_mask)))
}


#' Compute NDRE and attach a binary cloud mask
#'
#' Computes the normalized difference red-edge index (NDRE) from the `NIR` and
#' `RE1` bands of a Sentinel-2 image and adds a binary cloud mask derived from
#' the `QA60` band. The output preserves the original image properties and
#' acquisition time.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` with two bands: `NDRE` and `cloud_mask`.
#'
#' @keywords internal
#'
ltm_calc_ndre <- function(img) {

  ndre <- img$normalizedDifference(c('NIR', 'RE1'))$rename('NDRE')
  ndre <- ndre$copyProperties(img, img$propertyNames())$set('system:time_start', img$get('system:time_start'))

  qa <- img$select("QA60")
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  cloud_mask <- qa$bitwiseAnd(cloudBitMask)$neq(0)$Or(qa$bitwiseAnd(cirrusBitMask)$neq(0))
  binary_cloud_mask <- cloud_mask$rename("cloud_mask")$uint8()
  binary_cloud_mask <- binary_cloud_mask$copyProperties(img, img$propertyNames())$set('system:time_start',
                                                                                      img$get('system:time_start'))

  return(ee$Image(ndre)$addBands(ee$Image(binary_cloud_mask)))
}


#' Compute EVI and attach a binary cloud mask
#'
#' Computes the enhanced vegetation index (EVI) from the `NIR`, `Red`, and
#' `Blue` bands of a Sentinel-2 image and adds a binary cloud mask derived from
#' the `QA60` band. The output preserves the original image properties and
#' acquisition time.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` with two bands: `EVI` and `cloud_mask`.
#'
#' @keywords internal
ltm_calc_evi <- function(img){

  evi <- ee$Image(img$expression(
    '2.5 * ((NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1))',
    list(
      NIR = img$select('NIR'),
      Red = img$select('Red'),
      Blue = img$select('Blue')
    ))
  )$rename('EVI')
  evi <- evi$copyProperties(img, img$propertyNames())$set('system:time_start', img$get('system:time_start'))

  qa <- img$select("QA60")
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  cloud_mask <- qa$bitwiseAnd(cloudBitMask)$neq(0)$Or(qa$bitwiseAnd(cirrusBitMask)$neq(0))
  binary_cloud_mask <- cloud_mask$rename("cloud_mask")$uint8()
  binary_cloud_mask <- binary_cloud_mask$copyProperties(img, img$propertyNames())$set('system:time_start',
                                                                                      img$get('system:time_start'))

  return(ee$Image(evi)$addBands(ee$Image(binary_cloud_mask)))
}


#' Compute EVI2 and attach a binary cloud mask
#'
#' Computes the two-band enhanced vegetation index (EVI2) from the `NIR` and
#' `Red` bands of a Sentinel-2 image and adds a binary cloud mask derived from
#' the `QA60` band. The output preserves the original image properties and
#' acquisition time.
#'
#' @param img An `ee$Image` object representing a Sentinel-2 image.
#'
#' @return
#' An `ee$Image` with two bands: `EVI2` and `cloud_mask`.
#'
#' @keywords internal
#'
ltm_calc_evi2 <- function(img){

  evi2 <- ee$Image(img$expression(
    '2.4 * ((NIR - Red) / (NIR + Red + 1.0))',
    list(
      NIR = img$select('NIR'),
      Red = img$select('Red')
    ))
  )$rename('EVI2')
  evi2 <- evi2$copyProperties(img, img$propertyNames())$set('system:time_start', img$get('system:time_start'))

  qa <- img$select("QA60")
  cloudBitMask <- ee$Number(1 %<<% 10)  # 1 << 10
  cirrusBitMask <- ee$Number(1 %<<% 11) # 1 << 11
  cloud_mask <- qa$bitwiseAnd(cloudBitMask)$neq(0)$Or(qa$bitwiseAnd(cirrusBitMask)$neq(0))
  binary_cloud_mask <- cloud_mask$rename("cloud_mask")$uint8()
  binary_cloud_mask <- binary_cloud_mask$copyProperties(img, img$propertyNames())$set('system:time_start',
                                                                                      img$get('system:time_start'))

  return(ee$Image(evi2)$addBands(ee$Image(binary_cloud_mask)))
}


#' Get the time range of a time series
#'
#' Computes the minimum and maximum dates from the `ti` column of a data
#' frame or `spidf` object and returns them as a named vector.
#'
#' @param x A data frame or `spidf` object containing a `ti` column with
#'   date-like values.
#'
#' @return
#' A named vector with elements `start_date` and `end_date` of class `Date`.
#' Returns `NA` values if the input contains no valid dates.
#'
#' @keywords internal
#'
get_timerange <- function(x) {
  #stopifnot(inherits(x, "spidf"))

  if (!"ti" %in% names(x)) {
    stop("The object does not contain a 'ti' column.")
  }

  # Convert to Date
  ti_dates <- as.Date(x$ti)

  # Handle empty or invalid cases
  if (length(ti_dates) == 0 || all(is.na(ti_dates))) {
    return(c(start_date = NA_character_, end_date = NA_character_))
  }

  range_dates <- range(ti_dates, na.rm = TRUE)

  return(c(
    start_date = as.Date(format(range_dates[1], "%Y-%m-%d")),
    end_date   = as.Date(format(range_dates[2], "%Y-%m-%d"))
  ))
}


#' Convert a list of Earth Engine features to a data frame
#'
#' Converts a list of Earth Engine feature objects into a data frame by
#' extracting their properties. Ensures that a `system.id` field is present
#' when available and standardizes column names.
#'
#' @param features A list of Earth Engine feature objects.
#'
#' @return
#' A data frame where each row corresponds to a feature and columns correspond
#'   to feature properties. Returns an empty data frame if input is `NULL` or empty.
#'
#' @export
#'
ltm_ee_features_to_df <- function(features) {
  if (is.null(features) || length(features) == 0L) {
    return(data.frame())
  }

  rows <- lapply(features, function(feature) {
    properties <- feature$properties

    if (is.null(properties)) {
      properties <- list()
    }

    if (is.null(properties[["system:id"]]) && !is.null(feature$id)) {
      properties[["system:id"]] <- feature$id
    }

    properties
  })

  out <- dplyr::bind_rows(rows)
  names(out) <- make.names(names(out), unique = TRUE)
  out
}


#' Convert an Earth Engine FeatureCollection to a data frame
#'
#' Retrieves a FeatureCollection from Google Earth Engine, checks its size
#' against a maximum threshold, and converts its features into a data frame.
#'
#' @param x_fc An `ee$FeatureCollection` object.
#' @param max_features Integer. Maximum number of features allowed to be
#'   retrieved. Defaults to 10000.
#'
#' @return
#' A data frame where each row corresponds to a feature and columns correspond
#'   to feature properties.
#'
#' @export
#'
ltm_ee_feature_collection_to_df <- function(x_fc, max_features = 10000L) {
  feature_count <- try(
    ee$FeatureCollection(x_fc)$size()$getInfo(),
    silent = TRUE
  )

  if (!inherits(feature_count, "try-error") &&
      is.numeric(feature_count) &&
      length(feature_count) == 1L &&
      feature_count > max_features) {
    stop(
      "Too many features returned from GEE (",
      feature_count,
      "). Increase max_features if you really intend to download that many rows.",
      call. = FALSE
    )
  }

  feature_collection_info <- try(
    ee$FeatureCollection$getInfo(x_fc),
    silent = TRUE
  )

  if (inherits(feature_collection_info, "try-error")) {
    stop("An error occurred while getting point spectral data from GEE", call. = TRUE)
  }

  ltm_ee_features_to_df(feature_collection_info$features)
}


#' Extract a Sentinel-2 spectral index time series for a point or buffer
#'
#' Retrieves a Sentinel-2 time series from Google Earth Engine for a target
#' location, computes the selected spectral index, applies a binary cloud mask,
#' and returns the result as an object of class `spidf`.
#'
#' @param lat Numeric. Latitude of the target location.
#' @param lon Numeric. Longitude of the target location.
#' @param start_date Character or Date. Start date of the extraction period.
#' @param end_date Character or Date. End date of the extraction period.
#' @param spi Character. Spectral index to compute. Must be one of
#'   "NDVI","EVI","NBR","NDRE","EVI2"
#' @param proc_level Character. Sentinel-2 processing level to use. Must be one
#'   of "L1","L1C","L2","L2A"
#' @param crs_code Character. CRS code passed to Earth Engine during reduction.
#' @param rm_duplicates Logical. If `TRUE`, duplicate observations are removed
#'   with `ltm_remove_duplicates()`.
#' @param tree_id Optional identifier associated with the target tree or
#'   location. Stored as an attribute in the output.
#' @param use_buffer Logical. If `TRUE`, values are extracted from a buffer
#'   around the point.
#' @param buffer_radius_m Numeric. Buffer radius in meters. Required when
#'   `use_buffer = TRUE`.
#' @param cloud_mask_threshold Numeric. Threshold used to binarize the mean cloud
#'   mask; values greater than or equal to this threshold are treated as cloudy.
#'
#' @return
#' An object of class `spidf` with columns `id`, `ti`, `masked_vals`, `spi`,
#' and `cloud_mask`, plus metadata stored as attributes.
#'
#' @export
#'
ltm_s2_get_data_point <- function(lat, lon, start_date, end_date, spi = "NDVI",
                                  proc_level = "L2A", crs_code = "EPSG:4326",
                                  rm_duplicates = TRUE, tree_id = NULL,
                                  use_buffer = FALSE, buffer_radius_m = NULL,
                                  cloud_mask_threshold = 0.5){


  if(!(spi %in% SPECTRAL_INDICES_LIST)){
    stop("The spi is not listed as a valid spectral index. Available indices are: ",
         paste(SPECTRAL_INDICES_LIST, collapse=", "))
  }

  if(!(proc_level %in% PROC_LEVELS_LIST)){
    stop("The proc_level is not listed as a valid spectral index. Available indices are: ",
         paste(PROC_LEVELS_LIST, collapse=", "))
  }

  if(ltm_check_gee_status() != "CONNECTED"){
    stop("Google Earth Engine (GEE) has not been initialized. Please run ltm_start_gee()
         before calling this function.")
  }

  if (use_buffer) {
    if (!is.numeric(buffer_radius_m) || is.null(buffer_radius_m) || buffer_radius_m <= 0) {
      stop("When use_buffer=TRUE you must set buffer_radius_m to a positive number (meters).")
    }
  }

  if(!use_buffer && !is.null(buffer_radius_m) ){
    buffer_radius_m <- NULL
  }

  # Create the point geometry to extract data
  target_point <- ee$Geometry$Point(c(lon, lat))
  # Use either point or buffer
  target_geom  <- if(use_buffer) target_point$buffer(buffer_radius_m) else target_point


  # Load the Sentinel-2 L2A image collection (surface reflectance product)
  s2_collection <-
    ltm_get_s2_imgcol(proc_level) %>%
    ee$ImageCollection$filterBounds(target_point) %>%
    ee$ImageCollection$filterDate(start_date, end_date) %>%
    ee$ImageCollection$map(ltm_s2_scale_reflectance)


  dts <- try(rgee::ee_get_date_ic(s2_collection, time_end = FALSE), silent = TRUE)

  if(inherits(dts, "try-error")){
    stop("An error occurred while getting image dates from GEE", call. = FALSE)
  }

  if(spi == "NDVI"){
    # Map the NDVI extraction function over the image collection
    s2_collection <- s2_collection %>%
      ee$ImageCollection$map(ltm_calc_ndvi)
  }
  else if(spi == "EVI"){
    # Map the EVI extraction function over the image collection
    s2_collection <- s2_collection %>%
      ee$ImageCollection$map(ltm_calc_evi)
  }
  else if(spi == "NBR"){
    # Map the EVI extraction function over the image collection
    s2_collection <- s2_collection %>%
      ee$ImageCollection$map(ltm_calc_nbr)
  }
  else if(spi == "NDRE"){
    # Map the EVI extraction function over the image collection
    s2_collection <- s2_collection %>%
      ee$ImageCollection$map(ltm_calc_ndre)
  }
  else if(spi == "EVI2"){
    # Map the EVI extraction function over the image collection
    s2_collection <- s2_collection %>%
      ee$ImageCollection$map(ltm_calc_evi2)
  }
  else{
    stop("Spectral index defined in spi parameter does not exist")
  }

  ## Spectral index

  # Map using reduceRegion instead of sample

  s2_spi_features <- s2_collection$map(function(img) {
    img_val <- img$reduceRegion(
      reducer   = ee$Reducer$mean(),
      geometry  = target_geom,
      scale     = 10,
      crs       = crs_code,
      bestEffort = TRUE,
      maxPixels  = 1E12,
      tileScale  = 1
    )
    ee$Feature(NULL)$copyProperties(img, img$propertyNames())$set(img_val)
  })

  # Filter out any features where 'cloud_mask' is null
  spi_values <- ee$FeatureCollection(
    s2_spi_features$filter(
      ee$Filter$notNull(list("cloud_mask",spi))
      )
    )

  # Convert the feature collection directly to a plain table. Geometry is not
  # used here, so avoiding sf/geojson conversion also avoids noisy GDAL warnings.
  spi_values_list <- ltm_ee_feature_collection_to_df(spi_values)

  required_spi_cols <- c(spi, "cloud_mask", "system.id")
  missing_spi_cols <- setdiff(required_spi_cols, names(spi_values_list))
  if (length(missing_spi_cols) > 0L) {
    for (column_name in missing_spi_cols) {
      spi_values_list[[column_name]] <- vector(mode = "logical", length = nrow(spi_values_list))
    }
  }
  spi_values_list <- spi_values_list[, required_spi_cols, drop = FALSE]


  # binarize cloud mask (mean cloud fraction)
  if (nrow(spi_values_list) > 0) {
    spi_values_list <- spi_values_list %>%
      dplyr::mutate(
        cloud_mask = as.integer(.data$cloud_mask >= cloud_mask_threshold)
      )
  }

  dt <- dplyr::left_join(dts,spi_values_list[,c(spi,"cloud_mask","system.id")],
                  by=c("id"="system.id"))

  colnames(dt) <- c("id","ti","spi","cloud_mask")

  dt <- dt %>%
    dplyr::mutate(masked_vals = ifelse(.data$cloud_mask == 1, NA_real_, .data$spi)) %>%
    dplyr::select(1, 2, 5, 3, 4) # id, ti, masked_vals, spi, cloud_mask


  # Assign metadata as attributes
  attr(dt, "lat") <- lat
  attr(dt, "lon") <- lon
  attr(dt, "start_date") <- as.Date(start_date)
  attr(dt, "end_date") <- as.Date(end_date)

  attr(dt, "tree_id") <- tree_id

  attr(dt, "use_buffer") <- use_buffer
  attr(dt, "buffer_radius_m") <- buffer_radius_m
  attr(dt, "cloud_mask_threshold") <- cloud_mask_threshold

  attr(dt, "range_start") <- as.Date(get_timerange(dt)[1])
  attr(dt, "range_end") <- as.Date(get_timerange(dt)[2])

  attr(dt, "spi") <- spi
  attr(dt, "proc_level") <- proc_level
  attr(dt, "crs_code") <- crs_code

  attr(dt, "regularized") <- FALSE

  attr(dt, "mov_window") <- FALSE
  attr(dt, "mov_window_quantile") <- NA
  attr(dt, "mov_window_size_days") <- NA

  attr(dt, "whit_smoothing") <- FALSE
  attr(dt, "whit_lambda") <- NA
  attr(dt, "whit_quantile_threshold") <- NA
  attr(dt, "whit_weights_used") <- NA

  # Assign class
  class(dt) <- c("spidf", class(dt))

  if(rm_duplicates){
    dt <- ltm_remove_duplicates(dt)
  }

  return(dt)
}

#' Print method for `spidf` (spectral index dataframe) objects
#'
#' Displays a summary of a spectral index time series stored as an `spidf`
#' object, including spatial location, requested and available time ranges,
#' spectral index type, processing level, coordinate system, and preprocessing
#' status flags. The underlying data frame is then printed using the default
#' method.
#'
#' @param x An object of class `spidf`.
#' @param ... Additional arguments passed to the default `print` method.
#'
#' @return
#' Invisibly returns `x`.
#'
#' @export
#'
print.spidf <- function(x, ...) {
  cat("Spectral Index Data Frame (spidf):\n")
  cat(" Location (lon/lat): [", get_longitude(x), ", ", get_latitude(x), "]\n", sep = "")
  cat(" Period asked: ", format(get_start_date(x)), " to ", format(get_end_date(x)), "\n", sep = "")
  cat(" Period available: ", format(get_range_start(x)), " to ", format(get_range_end(x)), "\n", sep = "")
  cat(" Spectral Index: ", get_spi(x), "\n", sep = "")
  cat(" Processing Level: ", get_proc_level(x), "\n", sep = "")
  cat(" Coordinate System: ", get_crs_code(x), "\n", sep = "")
  cat(" Is the time series regularized?: ", ifelse(isTRUE(is_regularized(x)), "Yes", "No"), "\n", sep = "")
  cat(" Moving window?: ", ifelse(isTRUE(has_mov_window(x)), "Yes", "No"), "\n", sep = "")
  cat(" Whittaker smoothing?: ", ifelse(isTRUE(is_whit_smoothed(x)), "Yes", "No"), "\n\n", sep = "")

  # Call default data.frame print method
  NextMethod("print", x, ...)
}


## -------------------------------------------------------------
## GET Functions
## -------------------------------------------------------------

#' Get metadata from an `spidf` object
#'
#' These S3 generics and methods read metadata attributes stored on
#' `spidf` objects returned by [ltm_s2_get_data_point()] and preprocessing
#' helpers. Accessors are grouped by metadata type:
#' `get_latitude()`, `get_longitude()`, `get_start_date()`, `get_end_date()`,
#' `get_range_start()`, `get_range_end()`, `get_tree_id()`, `get_use_buffer()`,
#' `get_buffer_radius_m()`, `get_cloud_mask_threshold()`, `get_spi()`,
#' `get_proc_level()`, `get_crs_code()`, `is_regularized()`,
#' `get_regularize_method()`, `has_mov_window()`,
#' `get_mov_window_quantile()`, `get_mov_window_size_days()`,
#' `is_whit_smoothed()`, `get_whit_lambda()`,
#' `get_whit_quantile_threshold()`, and `get_whit_weights_used()`.
#'
#' @param x An object, typically of class `spidf`.
#'
#' @return The requested metadata attribute. Date accessors return `Date`
#'   values where the method coerces the stored attribute; logical state
#'   checkers return logical values; remaining accessors return the stored
#'   numeric or character metadata value.
#' @family metadata helpers
#' @aliases spidf_accessors
#' @rdname spidf_accessors
#' @export
get_latitude <- function(x) UseMethod("get_latitude")
#' @rdname spidf_accessors
#' @export
get_latitude.spidf <- function(x) attr(x, "lat")

#' @rdname spidf_accessors
#' @export
get_longitude <- function(x) UseMethod("get_longitude")
#' @rdname spidf_accessors
#' @export
get_longitude.spidf <- function(x) attr(x, "lon")

#' @rdname spidf_accessors
#' @export
get_start_date <- function(x) UseMethod("get_start_date")
#' @rdname spidf_accessors
#' @export
get_start_date.spidf <- function(x) attr(x, "start_date")

#' @rdname spidf_accessors
#' @export
get_end_date <- function(x) UseMethod("get_end_date")
#' @rdname spidf_accessors
#' @export
get_end_date.spidf <- function(x) attr(x, "end_date")

#' @rdname spidf_accessors
#' @export
get_spi <- function(x) UseMethod("get_spi")
#' @rdname spidf_accessors
#' @export
get_spi.spidf <- function(x) attr(x, "spi")

#' @rdname spidf_accessors
#' @export
get_proc_level <- function(x) UseMethod("get_proc_level")
#' @rdname spidf_accessors
#' @export
get_proc_level.spidf <- function(x) attr(x, "proc_level")

#' @rdname spidf_accessors
#' @export
get_crs_code <- function(x) UseMethod("get_crs_code")
#' @rdname spidf_accessors
#' @export
get_crs_code.spidf <- function(x) attr(x, "crs_code")

#' @rdname spidf_accessors
#' @export
is_regularized <- function(x) UseMethod("is_regularized")
#' @rdname spidf_accessors
#' @export
is_regularized.spidf <- function(x) attr(x, "regularized")

#' @rdname spidf_accessors
#' @export
get_regularize_method <- function(x) UseMethod("get_regularize_method")
#' @rdname spidf_accessors
#' @export
get_regularize_method.spidf <- function(x) attr(x, "regularize_method")

#' @rdname spidf_accessors
#' @export
has_mov_window <- function(x) UseMethod("has_mov_window")
#' @rdname spidf_accessors
#' @export
has_mov_window.spidf <- function(x) attr(x, "mov_window")

#' @rdname spidf_accessors
#' @export
is_whit_smoothed <- function(x) UseMethod("is_whit_smoothed")
#' @rdname spidf_accessors
#' @export
is_whit_smoothed.spidf <- function(x) attr(x, "whit_smoothing")

#' @rdname spidf_accessors
#' @export
get_mov_window_quantile <- function(x) UseMethod("get_mov_window_quantile")
#' @rdname spidf_accessors
#' @export
get_mov_window_quantile.spidf <- function(x) attr(x, "mov_window_quantile")

#' @rdname spidf_accessors
#' @export
get_mov_window_size_days <- function(x) UseMethod("get_mov_window_size_days")
#' @rdname spidf_accessors
#' @export
get_mov_window_size_days.spidf <- function(x) attr(x, "mov_window_size_days")

#' @rdname spidf_accessors
#' @export
get_whit_lambda <- function(x) UseMethod("get_whit_lambda")
#' @rdname spidf_accessors
#' @export
get_whit_lambda.spidf <- function(x) attr(x, "whit_lambda")

#' @rdname spidf_accessors
#' @export
get_whit_quantile_threshold <- function(x) UseMethod("get_whit_quantile_threshold")
#' @rdname spidf_accessors
#' @export
get_whit_quantile_threshold.spidf <- function(x) attr(x, "whit_quantile_threshold")

#' @rdname spidf_accessors
#' @export
get_whit_weights_used <- function(x) UseMethod("get_whit_weights_used")
#' @rdname spidf_accessors
#' @export
get_whit_weights_used.spidf <- function(x) attr(x, "whit_weights_used")

#' @rdname spidf_accessors
#' @export
get_range_start <- function(x) UseMethod("get_range_start")
#' @rdname spidf_accessors
#' @export
get_range_start.spidf <- function(x) as.Date(attr(x, "range_start"))

#' @rdname spidf_accessors
#' @export
get_range_end <- function(x) UseMethod("get_range_end")
#' @rdname spidf_accessors
#' @export
get_range_end.spidf <- function(x) as.Date(attr(x, "range_end"))

#' @rdname spidf_accessors
#' @export
get_tree_id <- function(x) UseMethod("get_tree_id")
#' @rdname spidf_accessors
#' @export
get_tree_id.spidf <- function(x) attr(x, "tree_id")

#' @rdname spidf_accessors
#' @export
get_use_buffer <- function(x) UseMethod("get_use_buffer")
#' @rdname spidf_accessors
#' @export
get_use_buffer.spidf <- function(x) attr(x, "use_buffer")

#' @rdname spidf_accessors
#' @export
get_buffer_radius_m <- function(x) UseMethod("get_buffer_radius_m")
#' @rdname spidf_accessors
#' @export
get_buffer_radius_m.spidf <- function(x) attr(x, "buffer_radius_m")

#' @rdname spidf_accessors
#' @export
get_cloud_mask_threshold <- function(x) UseMethod("get_cloud_mask_threshold")
#' @rdname spidf_accessors
#' @export
get_cloud_mask_threshold.spidf <- function(x) attr(x, "cloud_mask_threshold")

## -------------------------------------------------------------
## SET Functions
## -------------------------------------------------------------

#' Set metadata on an `spidf` object
#'
#' These S3 generics and methods update metadata attributes stored on `spidf`
#' objects. Setters return the modified object and do not perform full
#' consistency checks across attributes or data columns. Date setters coerce
#' values with [as.Date()], numeric setters coerce with [as.numeric()], integer
#' setters coerce with [as.integer()], and logical setters coerce with
#' [as.logical()] where shown by the method implementation.
#'
#' @param x An object, typically of class `spidf`.
#' @param value Replacement metadata value.
#'
#' @return The modified object `x`.
#' @family metadata helpers
#' @aliases spidf_setters
#' @rdname spidf_setters
#' @export
set_latitude <- function(x, value) UseMethod("set_latitude")
#' @rdname spidf_setters
#' @export
set_latitude.spidf <- function(x, value) {
  attr(x, "lat") <- value
  x
}

#' @rdname spidf_setters
#' @export
set_longitude <- function(x, value) UseMethod("set_longitude")
#' @rdname spidf_setters
#' @export
set_longitude.spidf <- function(x, value) {
  attr(x, "lon") <- value
  x
}

#' @rdname spidf_setters
#' @export
set_start_date <- function(x, value) UseMethod("set_start_date")
#' @rdname spidf_setters
#' @export
set_start_date.spidf <- function(x, value) {
  attr(x, "start_date") <- as.Date(value)
  x
}

#' @rdname spidf_setters
#' @export
set_end_date <- function(x, value) UseMethod("set_end_date")
#' @rdname spidf_setters
#' @export
set_end_date.spidf <- function(x, value) {
  attr(x, "end_date") <- as.Date(value)
  x
}

#' @rdname spidf_setters
#' @export
set_spi <- function(x, value) UseMethod("set_spi")
#' @rdname spidf_setters
#' @export
set_spi.spidf <- function(x, value) {
  attr(x, "spi") <- value
  x
}

#' @rdname spidf_setters
#' @export
set_proc_level <- function(x, value) UseMethod("set_proc_level")
#' @rdname spidf_setters
#' @export
set_proc_level.spidf <- function(x, value) {
  attr(x, "proc_level") <- value
  x
}

#' @rdname spidf_setters
#' @export
set_crs_code <- function(x, value) UseMethod("set_crs_code")
#' @rdname spidf_setters
#' @export
set_crs_code.spidf <- function(x, value) {
  attr(x, "crs_code") <- value
  x
}

#' @rdname spidf_setters
#' @export
set_regularized <- function(x, value) UseMethod("set_regularized")
#' @rdname spidf_setters
#' @export
set_regularized.spidf <- function(x, value) {
  attr(x, "regularized") <- as.logical(value)
  x
}

#' @rdname spidf_setters
#' @export
set_regularize_method <- function(x, value) UseMethod("set_regularize_method")
#' @rdname spidf_setters
#' @export
set_regularize_method.spidf <- function(x, value) {
  attr(x, "regularize_method") <- value
  x
}

#' @rdname spidf_setters
#' @export
set_mov_window <- function(x, value) UseMethod("set_mov_window")
#' @rdname spidf_setters
#' @export
set_mov_window.spidf <- function(x, value) {
  attr(x, "mov_window") <- as.logical(value)
  x
}

#' @rdname spidf_setters
#' @export
set_whit_smoothed <- function(x, value) UseMethod("set_whit_smoothed")
#' @rdname spidf_setters
#' @export
set_whit_smoothed.spidf <- function(x, value) {
  attr(x, "whit_smoothing") <- as.logical(value)
  x
}

#' @rdname spidf_setters
#' @export
set_mov_window_quantile <- function(x, value) UseMethod("set_mov_window_quantile")
#' @rdname spidf_setters
#' @export
set_mov_window_quantile.spidf <- function(x, value) {
  attr(x, "mov_window_quantile") <- as.numeric(value)
  x
}

#' @rdname spidf_setters
#' @export
set_mov_window_size_days <- function(x, value) UseMethod("set_mov_window_size_days")
#' @rdname spidf_setters
#' @export
set_mov_window_size_days.spidf <- function(x, value) {
  attr(x, "mov_window_size_days") <- as.integer(value)
  x
}

#' @rdname spidf_setters
#' @export
set_whit_lambda <- function(x, value) UseMethod("set_whit_lambda")
#' @rdname spidf_setters
#' @export
set_whit_lambda.spidf <- function(x, value) {
  attr(x, "whit_lambda") <- as.numeric(value)
  x
}

#' @rdname spidf_setters
#' @export
set_whit_quantile_threshold <- function(x, value) UseMethod("set_whit_quantile_threshold")
#' @rdname spidf_setters
#' @export
set_whit_quantile_threshold.spidf <- function(x, value) {
  attr(x, "whit_quantile_threshold") <- as.numeric(value)
  x
}

#' @rdname spidf_setters
#' @export
set_whit_weights_used <- function(x, value) UseMethod("set_whit_weights_used")
#' @rdname spidf_setters
#' @export
set_whit_weights_used.spidf <- function(x, value) {
  attr(x, "whit_weights_used") <- as.logical(value)
  x
}

#' @rdname spidf_setters
#' @export
set_range_start <- function(x, value) UseMethod("set_range_start")
#' @rdname spidf_setters
#' @export
set_range_start.spidf <- function(x, value) {
  attr(x, "range_start") <- as.Date(value)
  x
}

#' @rdname spidf_setters
#' @export
set_range_end <- function(x, value) UseMethod("set_range_end")
#' @rdname spidf_setters
#' @export
set_range_end.spidf <- function(x, value) {
  attr(x, "range_end") <- as.Date(value)
  x
}

#' @rdname spidf_setters
#' @export
set_tree_id <- function(x, value) UseMethod("set_tree_id")
#' @rdname spidf_setters
#' @export
set_tree_id.spidf <- function(x, value) {
  attr(x, "tree_id") <- value
  x
}

#' @rdname spidf_setters
#' @export
set_use_buffer <- function(x, value) UseMethod("set_use_buffer")
#' @rdname spidf_setters
#' @export
set_use_buffer.spidf <- function(x, value) {
  attr(x, "use_buffer") <- as.logical(value)
  x
}

#' @rdname spidf_setters
#' @export
set_buffer_radius_m <- function(x, value) UseMethod("set_buffer_radius_m")
#' @rdname spidf_setters
#' @export
set_buffer_radius_m.spidf <- function(x, value) {
  attr(x, "buffer_radius_m") <- as.numeric(value)
  x
}

#' @rdname spidf_setters
#' @export
set_cloud_mask_threshold <- function(x, value) UseMethod("set_cloud_mask_threshold")
#' @rdname spidf_setters
#' @export
set_cloud_mask_threshold.spidf <- function(x, value) {
  attr(x, "cloud_mask_threshold") <- as.numeric(value)
  x
}

