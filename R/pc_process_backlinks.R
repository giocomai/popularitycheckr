#' Process backlinks
#'
#' Uploads backliks to relevant google drive spreadsheet, consdering custom blacklist and whitelists.
#'
#' @param backlinks A data frame with backlinks, typically generated with `pc_get_backlinks()` or retrieved with `pc_downlodad_backlinks()`
#' @param include_domain Logical, defaults to TRUE. Include the base domain in the `landingPagePath` column for clarity, assuming https protocol.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pc_process_backlinks(pc_get_backlinks())
#' }
pc_process_backlinks <- function(backlinks, include_domain = TRUE) {
  df <- backlinks %>%
    dplyr::group_by(source, fullReferrer, landingPagePath) %>%
    dplyr::tally() %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(blacklist_source = "",
                  remove_referrer = "",
                  whitelist_source = "",
                  keep_referrer = "")

  if (include_domain == TRUE) {
    df <- df %>%
      dplyr::mutate(landingPagePath = paste0("https://", pc_set_domain(), landingPagePath))
  }

  current_spreadsheet <- pc_find_dribble(type = "inputs", content = "process")

  remote_sheet_pre <- googlesheets4::read_sheet(ss = current_spreadsheet)

  if (nrow(remote_sheet_pre)==0) {
    googlesheets4::write_sheet(data = df,
                               ss = current_spreadsheet,
                               sheet = "process")
  } else {
    new_df <- df %>%
      dplyr::anti_join(y = remote_sheet_pre,
                       by = c("fullReferrer", "landingPagePath")) %>%
      dplyr::mutate(date = as.Date(date))
    if (nrow(new_df)>0) {
      googlesheets4::sheet_append(ss = current_spreadsheet,
                                  data = new_df,
                                  sheet = "process")
    }
  }

}

pc_process_wb <- function() {
  current_spreadsheet <- pc_find_dribble(type = "inputs", content = "process")

  current_sheet_names <- googlesheets4::sheet_names(current_spreadsheet)

  if (is.element(el = whitelist_source, set = current_sheet_names)) {
    whitelist_source_df <- googlesheets4::read_sheet(ss = current_spreadsheet, sheet = "whitelist_source")
  } else {

  }


}
