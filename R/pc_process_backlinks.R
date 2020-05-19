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
      dplyr::summarise(sessions = sum(sessions)) %>%
      dplyr::arrange(dplyr::desc(sessions), fullReferrer)

    df[,lists] <- ""

    current_spreadsheet <- pc_find_dribble(type = "inputs", content = "process")

    remote_sheet_pre <- googlesheets4::read_sheet(ss = current_spreadsheet)

    if (nrow(remote_sheet_pre)==0) {
      googlesheets4::write_sheet(data = df,
                                 ss = current_spreadsheet,
                                 sheet = "process")
    } else {
      sources_to_exclude <- purrr::map_dfr(.x = lists[stringr::str_detect(lists, "source")],
                                           .f = function(x) {
                                             googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                       sheet = x)
                                           }) %>%
        dplyr::rename(source = 1)

      referrers_to_exclude <- purrr::map_dfr(.x = lists[stringr::str_detect(lists, "referrer")],
                                             .f = function(x) {
                                               googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                         sheet = x)
                                             }) %>%
        dplyr::rename(fullReferrer = 1)

      new_df <- df %>%
        dplyr::anti_join(y = remote_sheet_pre,
                         by = c("fullReferrer", "landingPagePath")) %>%
        dplyr::anti_join(y = referrers_to_exclude, by = "fullReferrer") %>%
        dplyr::anti_join(y = sources_to_exclude, by = "source")
      if (nrow(new_df)>0) {
        googlesheets4::sheet_append(ss = current_spreadsheet,
                                    data = new_df,
                                    sheet = 1)
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
                                                                    sheet = 1)
                select_lines <- which(is.na(current_process_df_pre[[x]])==FALSE)

                if (length(select_lines)==0) {
                  return(NULL)
                }

                if (stringr::str_detect(string = x, pattern = "source")) {
                  select_df_new <- tibble::tibble(source = unique(current_process_df_pre$source[select_lines])) %>%
                    dplyr::arrange(source)
                } else if (stringr::str_detect(string = x, pattern = "referrer")) {
                  select_df_new <- tibble::tibble(source = unique(current_process_df_pre$fullReferrer[select_lines])) %>%
                    dplyr::arrange(source)
                }

                if (is.element(el = x, set = current_sheet_names)) {
                  select_df_pre <- googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                       sheet = x)
                  select_df_up <- dplyr::bind_rows(select_df_pre, select_df_new) %>%
                    dplyr::distinct(source, .keep_all = TRUE) %>%
                    dplyr::arrange(source)
                } else {
                  googlesheets4::sheet_add(ss = current_spreadsheet, sheet = x)
                  select_df_pre <- tibble::tibble(source = NULL)
                  select_df_up <- select_df_new
                }
                if (isTRUE(dplyr::all_equal(select_df_pre, select_df_up))==FALSE) {
                  googlesheets4::sheet_write(data = select_df_up,
                                             ss = current_spreadsheet,
                                             sheet = x)
                }
                if (stringr::str_detect(string = x, pattern = "source")) {
                  lines_to_remove <- which(is.element(el = current_process_df_pre[["source"]], set = select_df_up[[1]]))
                } else if (stringr::str_detect(string = x, pattern = "referrer")) {
                  lines_to_remove <- which(is.element(el = current_process_df_pre[["fullReferrer"]], set = select_df_up[[1]]))
                }

                ### line by line option: slower and more risky in case of contemporary edits?
                # purrr::walk(.x = rev(select_lines+1),
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
              })
}


#' Move whitelisted backlinks to a separate spreadsheet to be combined with other data
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
    dplyr::select(source, fullReferrer, landingPagePath) %>%
    dplyr::filter(is.element(el = source, set = whitelist_source[[1]]) | is.element(el = fullReferrer, set = whitelist_referrer[[1]]))

  current_spreadsheet_combine <- pc_find_dribble(type = "inputs", content = "combine")
  current_sheet_names_combine <- googlesheets4::sheet_names(current_spreadsheet_combine)

  combine_sheet_main <- googlesheets4::read_sheet(ss = current_spreadsheet_combine,
                                                  sheet = 1)
  combine_sheet_pre <- purrr::map_dfr(.x = current_sheet_names_combine,
                                      .f = function(x) {
                                        googlesheets4::read_sheet(ss = current_spreadsheet_combine,
                                                                  sheet = x)
                                      })


  if (nrow(combine_sheet_pre)==0) {
    googlesheets4::write_sheet(data = backlinks_new,
                               ss = current_spreadsheet_combine,
                               sheet = 1)
  } else {
    backlinks_up <- backlinks_new %>%
      dplyr::distinct() %>%
      dplyr::anti_join(y = combine_sheet_pre,
                       by = c("source", "fullReferrer", "landingPagePath"))

    googlesheets4::range_write(ss = current_spreadsheet_combine,
                               data = backlinks_up,
                               range = paste0(LETTERS[which(colnames(combine_sheet_main)=="source")],nrow(combine_sheet_main)+2),
                               sheet = 1,col_names = FALSE)
  }
}




#' Process manual inputs
#'
#' Takes manual inputs, records them in lists in other sheets of the same worksheet, and removes them from the "combine" (or other) sheet.
#'
#' @param lists A characther vector giving the names of the categories for manual categorisation. Defaults to `c("blacklist_source","blacklist_referrer","whitelist_source","whitelist_referrer")`.
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' pc_process_inputs()
#' }
pc_process_inputs <- function(columns = c("public",
                                          "dismiss"),
                              spreadsheet = "combine") {
  if (googledrive::is_dribble(spreadsheet)==FALSE) {
    current_spreadsheet <- pc_find_dribble(type = "inputs", content = spreadsheet)
  } else {
    current_spreadsheet <- spreadsheet
  }

  current_sheet_names <- googlesheets4::sheet_names(current_spreadsheet)

  purrr::walk(.x = columns,
              .f = function(x) {
                current_process_df_pre <- googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                    sheet = 1)
                select_df_new <- current_process_df_pre %>%
                  dplyr::slice(which(is.na(current_process_df_pre[[x]])==FALSE))

                if (nrow(select_df_new)==0) {
                  return(NULL)
                }

                if (is.element(el = x, set = current_sheet_names)) {
                    googlesheets4::sheet_append(ss = current_spreadsheet,
                                                data = select_df_new,
                                                sheet = x)
                } else {
                  googlesheets4::sheet_add(ss = current_spreadsheet, sheet = x)
                  select_df_pre <- tibble::tibble(source = NULL)
                  select_df_up <- select_df_new
                  googlesheets4::sheet_write(data = select_df_up,
                                             ss = current_spreadsheet,
                                             sheet = x)
                }


                current_process_df_post <- current_process_df_pre %>%
                  dplyr::slice(-which(is.na(current_process_df_pre[[x]])==FALSE))


                if (nrow(current_process_df_pre)-nrow(current_process_df_post)>0) {
                  ### line by line option: slower and more risky in case of contemporary edits?
                  purrr::walk(.x = rev(which(is.na(current_process_df_pre[[x]])==FALSE)+1),
                              .f = function(x) {
                                googlesheets4::range_delete(ss = current_spreadsheet,
                                                            sheet = 1,
                                                            range = googlesheets4::cell_rows(x))
                              })

                  # googlesheets4::sheet_write(data = current_process_df_post,
                  #                            ss = current_spreadsheet,
                  #                            sheet = 1)
                }
              })
}

#' Removes elements already whitelisted or blacklisted
#'
#' @param lists
#' @param spreadsheet
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pc_clean_lists()
#' }
pc_clean_lists <- function(lists = c("blacklist_source",
                                     "blacklist_referrer",
                                     "whitelist_source",
                                     "whitelist_referrer"),
                           spreadsheet = "process") {

  if (googledrive::is_dribble(spreadsheet)==FALSE) {
    current_spreadsheet <- pc_find_dribble(type = "inputs", content = spreadsheet)
  } else {
    current_spreadsheet <- spreadsheet
  }

  sources_to_exclude <- purrr::map_dfr(.x = lists[stringr::str_detect(lists, "source")],
                                       .f = function(x) {
                                         googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                   sheet = x)
                                       })

  current_sheet_df <- googlesheets4::read_sheet(ss = current_spreadsheet, sheet = 1)

  purrr::walk(.x = rev(which(is.element(el = current_sheet_df$source, set = sources_to_exclude %>% dplyr::pull(1)))+1),
              .f = function(x) {
                googlesheets4::range_delete(ss = current_spreadsheet,
                                            sheet = 1,
                                            range = googlesheets4::cell_rows(x))
              })

  current_sheet_df <- googlesheets4::read_sheet(ss = current_spreadsheet, sheet = 1)

  referrers_to_exclude <- purrr::map_dfr(.x = lists[stringr::str_detect(lists, "referrer")],
                                         .f = function(x) {
                                           googlesheets4::read_sheet(ss = current_spreadsheet,
                                                                     sheet = x)
                                         })

  purrr::walk(.x = rev(which(is.element(el = current_sheet_df$fullReferrer, set = referrers_to_exclude %>% dplyr::pull(1)))+1),
              .f = function(x) {
                googlesheets4::range_delete(ss = current_spreadsheet,
                                            sheet = 1,
                                            range = googlesheets4::cell_rows(x))
              })

}
