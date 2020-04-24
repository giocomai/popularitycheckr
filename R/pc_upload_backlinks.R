#' Uploads ga backlinks to Google Drive
#'
#' @param backlinks
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pc_set_ga_id(1234567)
#'
#' pc_get_backlinks() %>%
#'   pc_upload_backlinks()
#'
#' }
pc_upload_backlinks <- function(backlinks) {
  df <- backlinks
  local_yearmonths <- stringr::str_extract(string = df$date,
                                           pattern = "[[:digit:]]{4}-[[:digit:]]{2}") %>%
    unique()

  purrr::walk(.x = local_yearmonths,
              .f = function(x) {
                current_month_df <- df %>%
                  dplyr::mutate(yearmonth = stringr::str_extract(string = date,
                                                                 pattern = "[[:digit:]]{4}-[[:digit:]]{2}")) %>%
                  dplyr::filter(yearmonth == x) %>%
                  dplyr::select(-yearmonth)

                current_spreadsheet <- pc_find_dribble(year = stringr::str_extract(string = x, pattern = "[[:digit:]]{4}"))

                remote_sheet_pre <- googlesheets4::read_sheet(ss = current_spreadsheet, sheet = x)

                if (nrow(remote_sheet_pre)==0) {
                  googlesheets4::write_sheet(data = df,
                                             ss = current_spreadsheet,
                                             sheet = x)
                } else {
                  new_df <- current_month_df %>% dplyr::mutate(date = as.character(date))%>%
                    dplyr::anti_join(y = remote_sheet_pre %>% dplyr::mutate(date = as.character(date)), by = c("date", "fullReferrer", "landingPagePath")) %>%
                    dplyr::mutate(date = as.Date(date))
                  if (nrow(new_df)>0) {
                    googlesheets4::sheet_append(ss = current_spreadsheet, data = new_df, sheet = x)
                  }
                }
              })
  backlinks
}

