pc_process_backlinks <- function(backlinks) {
  df <- backlinks %>%
    dplyr::group_by(source, fullReferrer, landingPagePath) %>%
    dplyr::tally() %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(blacklist_source = "",
                  remove_this = "",
                  whitelist_source = "",
                  keep_this = "")

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
