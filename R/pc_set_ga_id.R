#' Set or retrieve the Google Analytics viewId as an environment variable
#'
#' @param ga_id A numeric vector of length one corresponding to a Google Analyitcs viewId.
#'
#' @return The Google Analytics viewId currently recorded as environment variable.
#' @export
#'
#' @examples
#'
#' pc_set_ga_id(ga_id = 1234567)
#'
#' # If set, this function can be used to retrieve the id.
#'
#' my_ga_id <- pc_set_ga_id()
#' my_ga_id
pc_set_ga_id <- function(ga_id = NULL) {
  if(is.null(ga_id)) {
    ga_id <- Sys.getenv("popularitychecker_ga_id")
  } else {
    Sys.setenv(popularitychecker_ga_id = ga_id)
  }
  if (ga_id=="") usethis::ui_todo("Set Google Analytics view ID with `pc_set_ga_id()`")
  ga_id
}
