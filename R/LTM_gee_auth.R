
#' Initialize Google Earth Engine for LargeTreeMonitoring
#'
#' Starts a Google Earth Engine session through [rgee::ee_Initialize()] and
#' records the connection status in the `LTM_GEE_STATUS` option. Functions that
#' retrieve Sentinel-2 data use this option to confirm that Earth Engine has
#' been initialized before making server-side requests.
#'
#' @param user_name Optional character scalar passed to the `user` argument of
#'   [rgee::ee_Initialize()]. Use this when multiple Earth Engine credentials
#'   are configured.
#' @param ... Additional arguments passed to [rgee::ee_Initialize()].
#'
#' @return Logical scalar. Returns `TRUE` when initialization succeeds and
#'   `FALSE` when initialization raises an error. As a side effect, sets option
#'   `LTM_GEE_STATUS` to either `"CONNECTED"` or `"NOT_CONNECTED"`.
#' @family Google Earth Engine helpers
#' @export
ltm_start_gee <- function(user_name = NULL, ...){
  
  out <- try(rgee::ee_Initialize(user = user_name, ...), silent = TRUE)
  
  if(inherits(out, "try-error")){
    options(LTM_GEE_STATUS = "NOT_CONNECTED")
    return(FALSE)
  }
  else{
    options(LTM_GEE_STATUS = "CONNECTED")
    return(TRUE)
  }
}

#' Check the stored Google Earth Engine connection status
#'
#' Reads the `LTM_GEE_STATUS` option set by [ltm_start_gee()]. This helper does
#' not contact Earth Engine; it reports only the status known to the current R
#' session.
#'
#' @return If Earth Engine was initialized through [ltm_start_gee()], returns
#'   the stored status string, typically `"CONNECTED"` or `"NOT_CONNECTED"`.
#'   If no status has been recorded, warns and returns `FALSE`.
#' @family Google Earth Engine helpers
#' @export
ltm_check_gee_status <- function() {
  
  status <- getOption("LTM_GEE_STATUS")
  
  if (is.null(status)) {
    warning("GEE has not been initialized yet.")
    return(FALSE)
  }
  return(status)
}

