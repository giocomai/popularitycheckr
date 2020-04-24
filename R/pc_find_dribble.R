#' Find dribbles used by popularitycheckr
#'
#' @param year A vector of length one. Defaults to NULL, to be used only if relevant.
#' @param type Type of data that the dribble refers to. Defaults to "raw_data".
#' @param content Describes what contents will be included in the given spreadsheet. Defaults to "ga_backlinks".
#' @param base_folder Defaults to "popularitycheckr". All files and folders will be included inside this folder.
#' @param create_if_missing Defaults to TRUE. If already existing, it just returns the relevant dribble.
#'
#' @return A dribble
#' @export
#'
#' @examples
#'
#' \dontrun{
#' pc_find_dribble(2020)
#' }
pc_find_dribble <- function(year = NULL,
                            type = "raw_data",
                            content = "ga_backlinks",
                            base_folder = "popularitycheckr",
                            create_if_missing = TRUE) {

  if (is.null(year)) {
    current_spreadsheet_name <- paste(pc_set_domain(), type, content, sep = "-")
  } else {
    current_spreadsheet_name <- paste(year, pc_set_domain(), type, content, sep = "-")
  }

  current_spreadsheet <- googledrive::as_dribble(x = fs::path(base_folder,
                                                              pc_set_domain(),
                                                              type,
                                                              content,
                                                              current_spreadsheet_name))

  if (nrow(current_spreadsheet)>0) {
    return(current_spreadsheet)
  }

  base_folder_dribble <- googledrive::as_dribble(x = paste0(base_folder, "/"))

  if (nrow(base_folder_dribble)==0) {
    base_folder_dribble <- googledrive::drive_mkdir(name = base_folder,
                                                    overwrite = FALSE)
  }

  base_domain_folder <- googledrive::as_dribble(x = paste0(fs::path(base_folder,
                                                                    pc_set_domain()),
                                                           "/"))

  if (nrow(base_domain_folder)==0) {
    base_domain_folder <- googledrive::drive_mkdir(name = pc_set_domain(),
                                                   path = base_folder_dribble,
                                                   overwrite = FALSE)
  }

  base_type_folder <- googledrive::as_dribble(x = paste0(fs::path(base_folder,
                                                                  pc_set_domain(),
                                                                  type),
                                                         "/"))

  if (nrow(base_type_folder)==0) {
    base_type_folder <- googledrive::drive_mkdir(name = type,
                                                 path = base_domain_folder,
                                                 overwrite = FALSE)
  }

  current_content_folder <- googledrive::as_dribble(x = paste0(fs::path(base_folder,
                                                                       pc_set_domain(),
                                                                       type,
                                                                       content),
                                                              "/"))

  if (nrow(current_content_folder)==0) {
    current_content_folder <- googledrive::drive_mkdir(name = content,
                                                      path = base_type_folder,
                                                      overwrite = FALSE)
  }

  if (is.null(year)==FALSE) {
    sheets_names <- paste(year,
                          stringr::str_pad(string = 1:12,
                                           width = 2,
                                           side = "left",
                                           pad = "0"),
                          sep = "-")
  } else {
    sheets_names = content
  }

  current_spreadsheet <- googlesheets4::gs4_create(
    name = current_spreadsheet_name,
    sheets = sheets_names) %>%
    googledrive::as_dribble()

  googledrive::drive_mv(file = current_spreadsheet,
                        path = current_content_folder,
                        overwrite = FALSE)

  current_spreadsheet
}
