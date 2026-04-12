

# Legacy location used before the package refactor. Keep it as a read-only
# fallback so existing caches remain usable after upgrading.
#' Get the legacy cache directory
#'
#' @return Character scalar path to the historical `ltm_cache` directory under
#'   the current working directory.
#' @keywords internal
#' @noRd
ltm_legacy_cache_dir <- function() {
  file.path(getwd(), "ltm_cache")
}

#' Normalize an optional cache tag
#'
#' @param value Candidate tag value.
#' @return Character scalar tag, or `NULL` when `value` is missing, empty, or
#'   entirely `NA`.
#' @keywords internal
#' @noRd
ltm_normalize_cache_tag <- function(value) {
  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    return(NULL)
  }

  value <- as.character(value[[1]])
  if (!nzchar(value)) {
    return(NULL)
  }

  value
}

#' Normalize an optional cache radius
#'
#' @param value Candidate buffer-radius value.
#' @return Numeric scalar radius, or `NULL` when `value` is missing or not
#'   finite.
#' @keywords internal
#' @noRd
ltm_normalize_cache_radius <- function(value) {
  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    return(NULL)
  }

  value <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(value)) {
    return(NULL)
  }

  value
}

#' Build cache metadata fallback variants
#'
#' @param tree_id Optional tree identifier.
#' @param buffer_radius_m Optional buffer radius in meters.
#' @return List of metadata-tag variants used to search both current and older
#'   cache file names.
#' @keywords internal
#' @noRd
ltm_cache_metadata_variants <- function(tree_id = NULL, buffer_radius_m = NULL) {
  tree_id <- ltm_normalize_cache_tag(tree_id)
  buffer_radius_m <- ltm_normalize_cache_radius(buffer_radius_m)

  variants <- list(
    list(tree_id = tree_id, buffer_radius_m = buffer_radius_m)
  )

  # Dropping tree_id is safe because it only affects metadata/labelling.
  if (!is.null(tree_id)) {
    variants[[length(variants) + 1L]] <- list(
      tree_id = NULL,
      buffer_radius_m = buffer_radius_m
    )
  }

  # Older caches may also exist without buffer radius encoded in the file name.
  if (!is.null(buffer_radius_m)) {
    variants[[length(variants) + 1L]] <- list(
      tree_id = tree_id,
      buffer_radius_m = NULL
    )
  }

  if (!is.null(tree_id) && !is.null(buffer_radius_m)) {
    variants[[length(variants) + 1L]] <- list(
      tree_id = NULL,
      buffer_radius_m = NULL
    )
  }

  variant_keys <- vapply(
    variants,
    function(variant) {
      paste(
        if (is.null(variant$tree_id)) "<none>" else variant$tree_id,
        if (is.null(variant$buffer_radius_m)) "<none>" else format(variant$buffer_radius_m, trim = TRUE),
        sep = "::"
      )
    },
    character(1)
  )

  variants[!duplicated(variant_keys)]
}

#' Read a cache-related attribute with a default
#'
#' @param x Object carrying attributes.
#' @param name Character scalar attribute name.
#' @param default Value returned when the attribute is absent, empty, or all
#'   `NA`.
#' @return Attribute value or `default`.
#' @keywords internal
#' @noRd
ltm_cache_attr <- function(x, name, default = NULL) {
  value <- attr(x, name, exact = TRUE)

  if (is.null(value) || length(value) == 0L || all(is.na(value))) {
    default
  } else {
    value
  }
}

#' Upgrade a cached data frame to the current `spidf` structure
#'
#' @param x Cached object read from disk.
#' @param metadata_tags List of request metadata used to rebuild missing
#'   attributes.
#' @return An object of class `spidf`.
#' @keywords internal
#' @noRd
ltm_upgrade_cached_spidf <- function(x, metadata_tags) {
  if (!is.data.frame(x)) {
    stop("Cached object is not a data frame.", call. = FALSE)
  }

  out <- x
  requested_spi <- as.character(metadata_tags$spi)

  if (!"ti" %in% names(out)) {
    stop("Cached object does not contain a 'ti' column.", call. = FALSE)
  }

  if (!"spi" %in% names(out)) {
    if (requested_spi %in% names(out)) {
      names(out)[names(out) == requested_spi] <- "spi"
    } else {
      stop("Cached object does not contain a 'spi' column.", call. = FALSE)
    }
  }

  out$ti <- as.Date(out$ti)
  if (nrow(out) > 0L && any(is.na(out$ti))) {
    stop("Cached object contains invalid dates in column 'ti'.", call. = FALSE)
  }

  out$spi <- suppressWarnings(as.numeric(out$spi))
  if (nrow(out) > 0L && all(is.na(out$spi))) {
    stop("Cached object contains invalid values in column 'spi'.", call. = FALSE)
  }

  if (!"id" %in% names(out)) {
    out$id <- seq_len(nrow(out))
  }

  if (!"cloud_mask" %in% names(out)) {
    out$cloud_mask <- integer(nrow(out))
  } else {
    out$cloud_mask <- suppressWarnings(as.integer(out$cloud_mask))
    out$cloud_mask[is.na(out$cloud_mask)] <- 0L
  }

  if (!"masked_vals" %in% names(out)) {
    out$masked_vals <- ifelse(out$cloud_mask == 1L, NA_real_, out$spi)
  } else {
    out$masked_vals <- suppressWarnings(as.numeric(out$masked_vals))
    if (all(is.na(out$masked_vals)) && !all(is.na(out$spi))) {
      out$masked_vals <- ifelse(out$cloud_mask == 1L, NA_real_, out$spi)
    }
  }

  primary_columns <- c("id", "ti", "masked_vals", "spi", "cloud_mask")
  out <- out[, unique(c(primary_columns, names(out))), drop = FALSE]

  range_dates <- get_timerange(out)
  requested_buffer_radius <- ltm_normalize_cache_radius(metadata_tags$buffer_radius_m)
  cached_buffer_radius <- ltm_normalize_cache_radius(
    ltm_cache_attr(out, "buffer_radius_m", requested_buffer_radius)
  )
  cached_tree_id <- ltm_normalize_cache_tag(
    ltm_cache_attr(out, "tree_id", metadata_tags$tree_id)
  )

  attr(out, "lat") <- as.numeric(metadata_tags$lat)
  attr(out, "lon") <- as.numeric(metadata_tags$lon)
  attr(out, "start_date") <- as.Date(metadata_tags$start_date)
  attr(out, "end_date") <- as.Date(metadata_tags$end_date)
  attr(out, "range_start") <- as.Date(range_dates[["start_date"]])
  attr(out, "range_end") <- as.Date(range_dates[["end_date"]])
  attr(out, "spi") <- requested_spi
  attr(out, "proc_level") <- as.character(metadata_tags$proc_level)
  attr(out, "crs_code") <- ltm_cache_attr(out, "crs_code", "EPSG:4326")
  attr(out, "tree_id") <- cached_tree_id
  attr(out, "use_buffer") <- isTRUE(
    ltm_cache_attr(out, "use_buffer", !is.null(cached_buffer_radius))
  )
  attr(out, "buffer_radius_m") <- cached_buffer_radius
  attr(out, "cloud_mask_threshold") <- ltm_cache_attr(out, "cloud_mask_threshold", NA_real_)
  attr(out, "regularized") <- isTRUE(ltm_cache_attr(out, "regularized", FALSE))
  attr(out, "regularize_method") <- ltm_cache_attr(out, "regularize_method", NA_character_)
  attr(out, "mov_window") <- isTRUE(ltm_cache_attr(out, "mov_window", FALSE))
  attr(out, "mov_window_quantile") <- ltm_cache_attr(out, "mov_window_quantile", NA_real_)
  attr(out, "mov_window_size_days") <- ltm_cache_attr(out, "mov_window_size_days", NA_integer_)
  attr(out, "whit_smoothing") <- isTRUE(ltm_cache_attr(out, "whit_smoothing", FALSE))
  attr(out, "whit_lambda") <- ltm_cache_attr(out, "whit_lambda", NA_real_)
  attr(out, "whit_quantile_threshold") <- ltm_cache_attr(out, "whit_quantile_threshold", NA_real_)
  attr(out, "whit_weights_used") <- ltm_cache_attr(out, "whit_weights_used", NA)

  class(out) <- unique(c("spidf", "data.frame"))
  out
}

#' Validate a cached `spidf` object for app use
#'
#' @param x Candidate cached object.
#' @return The validated `spidf` object.
#' @keywords internal
#' @noRd
ltm_validate_spidf_for_app <- function(x) {
  if (!inherits(x, "spidf")) {
    stop("Cached object is not of class 'spidf'.", call. = FALSE)
  }

  required_cols <- c("id", "ti", "masked_vals", "spi", "cloud_mask")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop(
      "Cached object is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(get_latitude(x))
  invisible(get_longitude(x))
  invisible(get_start_date(x))
  invisible(get_end_date(x))
  invisible(get_range_start(x))
  invisible(get_range_end(x))
  invisible(get_spi(x))
  invisible(get_proc_level(x))
  invisible(get_crs_code(x))
  invisible(is_regularized(x))
  invisible(has_mov_window(x))
  invisible(is_whit_smoothed(x))

  x
}

#' Read and upgrade a cached `spidf` object
#'
#' @param cache_file Character scalar path to an RDS cache file.
#' @param metadata_tags List of request metadata.
#' @return A validated `spidf` object.
#' @keywords internal
#' @noRd
ltm_read_cached_spidf <- function(cache_file, metadata_tags) {
  raw_object <- readRDS(cache_file)
  ltm_validate_spidf_for_app(
    ltm_upgrade_cached_spidf(raw_object, metadata_tags = metadata_tags)
  )
}

#' Build candidate cache file paths for a request
#'
#' @param lat Numeric latitude.
#' @param lon Numeric longitude.
#' @param start_date Start date of the request.
#' @param end_date End date of the request.
#' @param spi Spectral index label.
#' @param proc_level Sentinel-2 processing-level label.
#' @param tree_id Optional tree identifier.
#' @param buffer_radius_m Optional buffer radius in meters.
#' @return Character vector of candidate cache file paths.
#' @keywords internal
#' @noRd
ltm_cache_lookup_files <- function(lat, lon, start_date, end_date, spi,
                                   proc_level, tree_id = NULL,
                                   buffer_radius_m = NULL) {
  metadata_variants <- ltm_cache_metadata_variants(
    tree_id = tree_id,
    buffer_radius_m = buffer_radius_m
  )

  candidate_files <- character(0)

  for (variant in metadata_variants) {
    cache_file <- ltm_cache_file_name(
      lat = lat,
      lon = lon,
      start_date = start_date,
      end_date = end_date,
      spi = spi,
      proc_level = proc_level,
      tree_id = variant$tree_id,
      buffer_radius_m = variant$buffer_radius_m
    )

    candidate_files <- c(
      candidate_files,
      cache_file,
      file.path(ltm_legacy_cache_dir(), basename(cache_file))
    )
  }

  unique(candidate_files)
}

#' Build the cache file name for a Sentinel-2 request
#'
#' Constructs the canonical RDS cache path for a Sentinel-2 time-series request.
#' Coordinates are rounded to five decimal places, dates are formatted as
#' `YYYY-MM-DD`, and optional tree and buffer metadata are included when
#' supplied.
#'
#' @param lat Numeric latitude used in the request.
#' @param lon Numeric longitude used in the request.
#' @param start_date Start date of the request; coercible to `Date`.
#' @param end_date End date of the request; coercible to `Date`.
#' @param spi Character scalar spectral-index label.
#' @param proc_level Character scalar Sentinel-2 processing-level label.
#' @param tree_id Optional tree identifier included in the cache name.
#' @param buffer_radius_m Optional buffer radius in meters included in the cache
#'   name.
#'
#' @return Character scalar path ending in `.rds` under [ltm_cache_dir()].
#' @family cache and configuration helpers
#' @export
ltm_cache_file_name <- function(lat, lon, start_date, end_date, spi,
                                proc_level, tree_id=NULL, buffer_radius_m=NULL) {

  tree_id <- ltm_normalize_cache_tag(tree_id)
  buffer_radius_m <- ltm_normalize_cache_radius(buffer_radius_m)
  cache_dir <- ltm_cache_dir()
  start_date <- format(as.Date(start_date), "%Y-%m-%d")
  end_date   <- format(as.Date(end_date),   "%Y-%m-%d")


  name_parts <- c("ltm",
                  round(lat, 5), round(lon, 5),
                  start_date, end_date,
                  spi, proc_level)

  if(!is.null(tree_id)){
    name_parts    <- c(name_parts, tree_id)
  }

  if(!is.null(buffer_radius_m)){
    name_parts    <- c(name_parts, paste0("bf",round(buffer_radius_m,1)))
  }


  file.path(cache_dir, paste0(paste(name_parts, collapse = "_"), ".rds"))
}

#' Look up a cached Sentinel-2 request
#'
#' Searches the current cache location and legacy fallback names for a cached
#' `spidf` object matching a Sentinel-2 request. The lookup tolerates older file
#' names that did not encode optional tree identifiers or buffer radii.
#'
#' @inheritParams ltm_cache_file_name
#'
#' @return Character scalar path to the first matching cache file, or `NULL`
#'   when no cache file is found.
#' @family cache and configuration helpers
#' @export
ltm_check_cache <- function(lat, lon, start_date, end_date, spi,
                            proc_level, tree_id=NULL, buffer_radius_m=NULL) {
  candidate_files <- ltm_cache_lookup_files(
    lat = lat,
    lon = lon,
    start_date = start_date,
    end_date = end_date,
    spi = spi,
    proc_level = proc_level,
    tree_id = tree_id,
    buffer_radius_m = buffer_radius_m
  )

  for (cache_file in candidate_files) {
    if (file.exists(cache_file)) {
      return(cache_file)
    }
  }

  return(NULL)
}


#' Save an `spidf` object to the package cache
#'
#' Writes a spectral-index data frame to an RDS file in the cache directory.
#' The cache file name is derived either from `metadata_tags` or from metadata
#' attributes stored on `spidf_ts`.
#'
#' @param spidf_ts Object to cache, normally an object of class `spidf`.
#' @param metadata_tags Optional list containing `lat`, `lon`, `start_date`,
#'   `end_date`, `spi`, `proc_level`, `tree_id`, and `buffer_radius_m`. When
#'   omitted, those values are read from attributes on `spidf_ts`.
#'
#' @return Invisibly returns the character scalar path to the written cache
#'   file.
#' @family cache and configuration helpers
#' @export
ltm_save_to_cache <- function(spidf_ts, metadata_tags = NULL){
  # Makes the name for the cache file

  if(!is.null(metadata_tags)){
    file_name <- ltm_cache_file_name(
      lat        = metadata_tags$lat,
      lon        = metadata_tags$lon,
      start_date = metadata_tags$start_date,
      end_date   = metadata_tags$end_date,
      spi        = metadata_tags$spi,
      proc_level = metadata_tags$proc_level,
      tree_id    = metadata_tags$tree_id,
      buffer_radius_m = metadata_tags$buffer_radius_m
    )
  }else{ # Get from object metadata
    file_name <- ltm_cache_file_name(
      lat        = attr(spidf_ts,"lat"),
      lon        = attr(spidf_ts,"lon"),
      start_date = attr(spidf_ts,"start_date"),
      end_date   = attr(spidf_ts,"end_date"),
      spi        = attr(spidf_ts,"spi"),
      proc_level = attr(spidf_ts,"proc_level"),
      tree_id         = attr(spidf_ts,"tree_id"),
      buffer_radius_m = attr(spidf_ts,"buffer_radius_m")
    )
  }

  cache_dir <- dirname(file_name)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Writes the file to the main cache folder
  readr::write_rds(spidf_ts, file_name)
  invisible(file_name)
}

