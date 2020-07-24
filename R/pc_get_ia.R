#' Gets an Archive.org Wayback Machine URL
#'
#' See httpss://archive.org/help/wayback_api.php; and httpss://github.com/hrbrmstr/wayback for an R implementation.
#'
#' @param url A charachter vector of length one, a url.
#'
#' @return A url linking to the version on the Internet Archive
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
pc_get_ia_url <- function(url) {
  ia_available <- httr::GET("https://archive.org/wayback/available", query = list(url = url))
  httr::stop_for_status(ia_available)
  ia_available_text <- httr::content(ia_available, as = "text", encoding = "UTF-8")
  ia_available_list <- jsonlite::fromJSON(txt = ia_available_text)

  if (is.null(ia_available_list$archived_snapshots$closest$available)) {
    httr::GET(url = paste0("https://web.archive.org/save/", url))
    ia_available <- httr::GET("https://archive.org/wayback/available", query = list(url = url))
    httr::stop_for_status(ia_available)
    ia_available_text <- httr::content(ia_available, as = "text", encoding = "UTF-8")
    ia_available_list <- jsonlite::fromJSON(txt = ia_available_text)
  }
  ia_available_list$archived_snapshots$closest$url
}
