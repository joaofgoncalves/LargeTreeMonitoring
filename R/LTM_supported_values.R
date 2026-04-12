
#' Supported Sentinel-2 spectral indices
#'
#' Character vector containing the spectral index names accepted by
#' [ltm_s2_get_data_point()] and related workflows. The package currently
#' supports normalized difference vegetation index (`"NDVI"`), enhanced
#' vegetation index (`"EVI"`), normalized burn ratio (`"NBR"`), normalized
#' difference red-edge index (`"NDRE"`), and two-band enhanced vegetation index
#' (`"EVI2"`).
#'
#' @format A character vector.
#' @family package constants
#' @export
SPECTRAL_INDICES_LIST <- c("NDVI","EVI","NBR","NDRE","EVI2")

#' Supported Sentinel-2 processing levels
#'
#' Character vector containing the Sentinel-2 processing-level labels accepted
#' by [ltm_s2_get_data_point()]. `"L1"` and `"L1C"` select top-of-atmosphere
#' reflectance, while `"L2"` and `"L2A"` select surface reflectance.
#'
#' @format A character vector.
#' @family package constants
#' @export
PROC_LEVELS_LIST <- c("L1","L1C","L2","L2A")

#' Supported time-series columns for break detection
#'
#' Character vector containing column names that break-detection wrappers accept
#' through their `ts_name` argument. These identify the raw or preprocessed
#' spectral-index series stored in an `spidf` object.
#'
#' @format A character vector.
#' @family package constants
#' @export
VALID_DATA_TYPES <- c("spi", "spi_mov_wind", "spi_smooth", "spi_mov_smooth")

