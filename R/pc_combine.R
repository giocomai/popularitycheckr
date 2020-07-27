#' Combine whitelisted sources and referrers with additional data extracted from a website
#'
#' @param ... Named functions
#' @param url_column Name of the column to be used for the input url.
#' @param wait Seconds to wait between each write call. Used to prevent Google Drive API errors.
#'
#' @return Nothing, used for its side effects (fills relevant column in the `combine` spreadsheet)
#' @export
#'
#' @examples
#' \dontrun{
#' pc_combine(url = "https://www.europeandatajournalism.eu/eng/News/Data-news/The-price-of-coastal-flood-mitigation-in-Europe",
#'            title = function(url) {
#'              pc_extract_from_web(url = url,
#'                                  container = "h1")
#'            },
#'            date = function(url) {
#'              pc_extract_from_web(url = url,
#'                                  container = "div",
#'                                  container_class = "date") %>%
#'                stringr::str_remove(pattern = "[[:alpha:]]+") %>%
#'                anytime::anydate()}
#' )
#' }
#'
pc_combine <- function(...,
                       url_column = "landingPagePath",
                       sheet = "combine",
                       wait = 0.1) {
  dots <- list(...)
  current_spreadsheet <- pc_find_dribble(type = "inputs", content = "combine")

  purrr::walk(.x = seq_along(dots),
              .f = function(x) {
                current_sheet_df <- googlesheets4::read_sheet(ss = current_spreadsheet,
                                                              sheet = sheet,
                                                              col_types = "c")
                current_field <- names(dots)[x]
                current_function <- dots[[x]]
                if (is.element(el = current_field, set = colnames(current_sheet_df))==FALSE) {
                  col <- dplyr::enquo(current_field)
                  empty_df <- tibble::tibble(!!dplyr::quo_name(current_field) := "")
                  current_column <- LETTERS[ncol(current_sheet_df)+1]
                  googlesheets4::range_write(ss = current_spreadsheet,
                                             data = empty_df,
                                             sheet = sheet,
                                             col_names = TRUE,
                                             range = current_column)
                  current_sheet_df[[current_field]] <- NA
                } else {
                  current_column <- LETTERS[which(colnames(current_sheet_df)==current_field)]
                }

                purrr::walk(.x = unique(current_sheet_df[[url_column]][(is.na(current_sheet_df[[current_field]])|current_sheet_df[[current_field]]=="")&(is.na(current_sheet_df[[url_column]])==FALSE)]),
                            .f = function(y) {
                              current_input <- current_function(y)
                              cells_to_fill <- paste0(current_column, which(current_sheet_df[[url_column]]==y&(is.na(current_sheet_df[[current_field]])|current_sheet_df[[current_field]]==""))+1)
                              if(is.na(current_input)==FALSE&as.character(current_input)!="") {
                                purrr::walk(.x = cells_to_fill,
                                            .f = function(z) {
                                              Sys.sleep(time = wait)
                                              googlesheets4::range_write(ss = current_spreadsheet,
                                                                         data = tibble::tibble(!!dplyr::quo_name(current_field) := current_input),
                                                                         sheet = sheet,
                                                                         range = z,
                                                                         col_names = FALSE)
                                            })
                              }
                            })

              })
}
