#' Set or retrieve the domain you are currently working with as an environment variable
#'
#' @param domain A plain domain name, such as europeandatajournalism.eu
#'
#' @return The domain currently recorded as an environment variable.
#' @export
#'
#' @examples
#'
#' pc_set_domain(domain = "europeandatajournalism.eu")
#'
#' # If set, this function can be used to retrieve the id.
#'
#' my_domain <- pc_set_domain()
#' my_domain
pc_set_domain <- function(domain = NULL) {
  if(is.null(domain)) {
    domain <- Sys.getenv("popularitychecker_domain")
  } else {
    Sys.setenv(popularitychecker_domain = domain)
  }
  if (domain=="") usethis::ui_todo("Set domain `pc_set_domain()`")
  domain
}
