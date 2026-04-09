

# Legacy location used before the package refactor. Keep it as a read-only
# fallback so existing caches remain usable after upgrading.
ltm_legacy_cache_dir <- function() {
  file.path(getwd(), "ltm_cache")
}


# Function to generate cache file name
ltm_cache_file_name <- function(lat, lon, start_date, end_date, spi, 
                                proc_level, tree_id=NULL, buffer_radius_m=NULL) {
  
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

# Check cache function
ltm_check_cache <- function(lat, lon, start_date, end_date, spi, 
                            proc_level, tree_id=NULL, buffer_radius_m=NULL) {
  
  cache_file <- ltm_cache_file_name(
    lat = lat,
    lon = lon,
    start_date = start_date,
    end_date = end_date,
    spi = spi,
    proc_level = proc_level,
    tree_id = tree_id,
    buffer_radius_m = buffer_radius_m
  )

  if (file.exists(cache_file)) {
    return(cache_file)
  }

  legacy_dir <- ltm_legacy_cache_dir()
  legacy_file <- file.path(legacy_dir, basename(cache_file))
  if (file.exists(legacy_file)) {
    return(legacy_file)
  }

  return(NULL)
}


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

