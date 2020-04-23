#' Find dribbles used by popularitycheckr
#'
#' @param year A vector of length one.
#' @param type Type of data that the dribble refers to. Defaults to "raw_data".
#' @param source Source for the data. Defaults to "ga_backlinks".
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
pc_find_dribble <- function(year,
                            type = "raw_data",
                            source = "ga_backlinks",
                            base_folder = "popularitycheckr",
                            create_if_missing = TRUE) {

  base_folder_dribble <- googledrive::as_dribble(x = base_folder)

  if (nrow(base_folder_dribble)==0) {
    base_folder_dribble <- googledrive::drive_mkdir(name = base_folder,
                                                    overwrite = FALSE)
  }

  base_domain_folder <- googledrive::as_dribble(x = fs::path(base_folder, pc_set_domain()))

  if (nrow(base_domain_folder)==0) {
    base_domain_folder <- googledrive::drive_mkdir(name = pc_set_domain(),
                                                   path = base_folder_dribble,
                                                   overwrite = FALSE)
  }

  base_type_folder <- googledrive::as_dribble(x = fs::path(base_folder,
                                                           pc_set_domain(),
                                                           type))

  if (nrow(base_type_folder)==0) {
    base_type_folder <- googledrive::drive_mkdir(name = type,
                                                 path = base_domain_folder,
                                                 overwrite = FALSE)
  }

  current_source_folder <- googledrive::as_dribble(x = fs::path(base_folder,
                                                                pc_set_domain(),
                                                                type,
                                                                source))

  if (nrow(current_source_folder)==0) {
    current_source_folder <- googledrive::drive_mkdir(name = source,
                                                      path = base_type_folder,
                                                      overwrite = FALSE)
  }

  current_spreadsheet_name <- paste(year, pc_set_domain(), type, source, sep = "-")

  current_spreadsheet <- googledrive::as_dribble(x = fs::path(base_folder,
                                                              pc_set_domain(),
                                                              type,
                                                              source,
                                                              current_spreadsheet_name))

  if (nrow(current_spreadsheet)==0) {
    current_spreadsheet <- googledrive::drive_create(name = current_spreadsheet_name,
                                                     type = "spreadsheet",
                                                     path = current_source_folder,
                                                     overwrite = FALSE)
  }
  current_spreadsheet
}
