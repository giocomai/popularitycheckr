#' Process backlinks
#'
#' Uploads backliks to relevant google drive spreadsheet, consdering custom blacklist and whitelists.
#'
#' @param backlinks A data frame with backlinks, typically generated with `pc_get_backlinks()` or retrieved with `pc_downlodad_backlinks()`
#' @param lists A characther vector giving the names of the categories for manual categorisation. Defaults to `c("blacklist_source","blacklist_referrer","whitelist_source","whitelist_referrer")`.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pc_prepare_backlinks(pc_get_backlinks())
#' }
pc_process_backlinks <- function(backlinks = NULL,
                                 lists = c("blacklist_source",
                                           "blacklist_referrer",
                                           "whitelist_source",
                                           "whitelist_referrer")) {
  if (is.null(backlinks)==FALSE) {
    df <- backlinks %>%
      dplyr::group_by(source, fullReferrer, landingPagePath) %>%
      dplyr::tally() %>%
      dplyr::arrange(dplyr::desc(n))

    df[,lists] <- ""

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



}

#' Process whitelists/blacklists or other custom lists
#'
#' Takes manual inputs, records them in lists in other sheets of the same worksheet, and removes them from the "process" sheet.
#'
#' @param lists A characther vector giving the names of the categories for manual categorisation. Defaults to `c("blacklist_source","blacklist_referrer","whitelist_source","whitelist_referrer")`.
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' pc_process_lists()
#' }
pc_process_lists <- function(lists = c("blacklist_source",
                                       "blacklist_referrer",
                                       "whitelist_source",
                                       "whitelist_referrer")) {
  current_spreadsheet <- pc_find_dribble(type = "inputs", content = "process")

  current_sheet_names <- googlesheets4::sheet_names(current_spreadsheet)

  purrr::walk(.x = lists,
              .f = function(x) {
                current_process_df_pre <- googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                    sheet = "process")
                source_lines <- which(is.na(current_process_df_pre[[x]])==FALSE)
                if (stringr::str_detect(string = x, pattern = "source")) {
                  source_df_new <- tibble::tibble(source = unique(current_process_df_pre$source[source_lines])) %>%
                    dplyr::arrange(source)
                } else if (stringr::str_detect(string = x, pattern = "referrer")) {
                  source_df_new <- tibble::tibble(source = unique(current_process_df_pre$fullReferrer[source_lines])) %>%
                    dplyr::arrange(source)
                }

                if (is.element(el = x, set = current_sheet_names)) {
                  source_df_pre <- googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                       sheet = x)
                  source_df_up <- dplyr::bind_rows(source_df_pre, source_df_new) %>%
                    dplyr::distinct(source) %>%
                    dplyr::arrange(source)
                } else {
                  googlesheets4::sheet_add(ss = current_spreadsheet, sheet = x)
                  source_df_pre <- tibble::tibble(source = NULL)
                  source_df_up <- source_df_new
                }
                if (isTRUE(dplyr::all_equal(source_df_pre, source_df_up))==FALSE) {
                  googlesheets4::sheet_write(data = source_df_up,
                                             ss = current_spreadsheet,
                                             sheet = x)

                  if (stringr::str_detect(string = x, pattern = "source")) {
                    lines_to_remove <- which(is.element(el = current_process_df_pre[["source"]], set = source_df_up[[1]]))
                  } else if (stringr::str_detect(string = x, pattern = "referrer")) {
                    lines_to_remove <- which(is.element(el = current_process_df_pre[["fullReferrer"]], set = source_df_up[[1]]))
                  }

                  ### line by line option: slower and more risky in case of contemporary edits?
                  # purrr::walk(.x = rev(source_lines+1),
                  #             .f = function(x) {
                  #   googlesheets4::range_delete(ss = current_spreadsheet,
                  #                               sheet = "process",
                  #                               range = googlesheets4::cell_rows(x))
                  # })
                  if (length(lines_to_remove)>0) {
                    googlesheets4::sheet_write(data = current_process_df_pre %>%
                                                 dplyr::slice(-lines_to_remove),
                                               ss = current_spreadsheet,
                                               sheet = "process")
                  }
                }
              })
}


#' Move whitelisted backlinks to a seprate spreadsheet to be combined with other data
#'
#' @param backlinks A data frame with backlinks, typically generated with `pc_get_backlinks()` or retrieved with `pc_downlodad_backlinks()`
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pc_process_move_whitelisted(pc_get_backlinks())
#' googledrive::drive_browse(pc_find_dribble(type = "inputs", content = "combine"))
#' }
pc_process_move_whitelisted <- function(backlinks) {
  current_spreadsheet_process <- pc_find_dribble(type = "inputs", content = "process")
  current_sheet_names_process <- googlesheets4::sheet_names(current_spreadsheet_process)

  whitelist_source <- googlesheets4::read_sheet(ss = current_spreadsheet_process,
                                                sheet = "whitelist_source")

  whitelist_referrer <- googlesheets4::read_sheet(ss = current_spreadsheet_process,
                                                  sheet = "whitelist_referrer")

  backlinks_new <- backlinks %>%
    dplyr::select(source, fullReferrer, landingPagePath)
    dplyr::filter(is.element(el = source, set = whitelist_source[[1]]) | is.element(el = fullReferrer, set = whitelist_referrer[[1]]))

  current_spreadsheet_combine <- pc_find_dribble(type = "inputs", content = "combine")
  current_sheet_names_combine <- googlesheets4::sheet_names(current_spreadsheet_combine)

  combine_sheet_pre <- googlesheets4::read_sheet(ss = current_spreadsheet_combine,
                                                 sheet = "combine")

  if (nrow(combine_sheet_pre)==0) {
    googlesheets4::write_sheet(data = backlinks_new,
                               ss = current_spreadsheet_combine,
                               sheet = "combine")
  } else {
    backlinks_up <- backlinks_new %>%
      dplyr::anti_join(y = combine_sheet_pre,
                       by = c("source", "fullReferrer", "landingPagePath"))

    googlesheets4::sheet_append(ss = current_spreadsheet_combine,
                                data = backlinks_up,
                                sheet = "combine")
  }
}
