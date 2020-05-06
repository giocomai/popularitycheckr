#' Combine whitelisted sources and referrers with additional data extracted from a website
#'
#' @param ... Named functions
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
pc_combine <- function(...) {
  dots <- list(...)
  current_spreadsheet <- pc_find_dribble(type = "inputs", content = "combine")
  current_sheet_df <- googlesheets4::read_sheet(ss = current_spreadsheet,
                                                sheet = "combine")

  purrr::walk(.x = seq_along(dots),
              .f = function(x) {
                current_field <- names(dots)[x]
                current_function <- dots[[x]]
                if (is.element(el = current_field, set = colnames(current_sheet_df))==FALSE) {
                  col <- dplyr::enquo(current_field)
                  empty_df <- tibble::tibble(!!dplyr::quo_name(current_field) := "")
                  current_column <- LETTERS[ncol(current_sheet_df)+1]
                  googlesheets4::range_write(ss = current_spreadsheet,
                                             data = empty_df,
                                             sheet = "combine",
                                             col_names = TRUE,
                                             range = current_column)
                  current_sheet_df[[current_field]] <- NA
                } else {
                  current_column <- LETTERS[which(colnames(current_sheet_df)==current_field)]
                }

                purrr::walk(.x = current_sheet_df[["landingPagePath"]][is.na(current_sheet_df[[current_field]])|current_sheet_df[[current_field]]==""],
                            .f = function(y) {
                              current_input <- current_function(y)
                              cells_to_fill <- paste0(current_column, which(current_sheet_df[["landingPagePath"]]==y)+1)
                              purrr::walk(.x = cells_to_fill,
                                          .f = function(z) {
                                            googlesheets4::range_write(ss = current_spreadsheet,
                                                                       data = tibble::tibble(!!dplyr::quo_name(current_field) := current_input),
                                                                       range = z,
                                                                       col_names = FALSE)
                                          })
                            })

              })
}
