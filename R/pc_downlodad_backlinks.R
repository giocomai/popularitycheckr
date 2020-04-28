#' Downloads backlinks stored in Google Drive
#'
#' @param date_range A vector of length two, defaults to c(Sys.Date()-32, Sys.Date()-1).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pc_downlodad_backlinks()
#' }
pc_downlodad_backlinks <- function(date_range = c(Sys.Date()-32, Sys.Date()-1)) {

  yearmonths <- format(seq(date_range[1], date_range[2], by="month"), "%Y-%m")

  df <- purrr::map_dfr(.x = yearmonths,
                       .f = function(x) {
                         googlesheets4::read_sheet(ss = pc_find_dribble(year = stringr::str_extract(string = x,
                                                                                                    pattern = "[[:digit:]]{4}"),
                                                                        type = "raw_data",
                                                                        content = "ga_backlinks"),
                                                   sheet = x)
                       })

  df %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(date>=as.Date(date_range[1]), date<=as.Date(date_range[2])) %>%
    dplyr::arrange(date)
}
