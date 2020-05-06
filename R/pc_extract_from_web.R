#' Facilitates extracting strings from web pages
#'
#' @param url A characther vector of length one. URL or path to local html file.
#' @param container Defaults to NULL. If provided, it must be an html element such as "div", "span", etc.
#' @param container_class Defaults to NULL. If provided, also `container` must be given (and `container_id` must be NULL). Only text found inside the provided combination of container/class will be extracted.
#' @param container_id Defaults to NULL. If provided, also `container` must be given (and `container_class` must be NULL). Only text found inside the provided combination of container/class will be extracted.
#' @param container_instance Defaults to NULL. If given, it must be an integer. If a given element is found more than once in the same page, it keeps only the relevant occurrence for further extraction.
#' @param subelement Defaults to NULL. If provided, also `container` must be given. Only text within elements of given type under the chosen combination of container/container_class will be extracted. When given, it will tipically be "p", to extract all p elements inside the selected div.
#' @param no_children Defaults to FALSE, i.e. by default all subelements of the selected combination (e.g. div with given class) are extracted. If TRUE, only text found under the given combination (but not its subelements) will be extracted. Corresponds to the xpath string `/node()[not(self::div)]`.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' title <- pc_extract_from_web(url = "https://www.europeandatajournalism.eu/eng/News/Data-news/The-price-of-coastal-flood-mitigation-in-Europe", container = "h1")
#' }
pc_extract_from_web <- function(url,
                                container = NULL,
                                container_class = NULL,
                                container_id = NULL,
                                container_instance = NULL,
                                subelement = NULL,
                                no_children = NULL) {
  temp <-  tryCatch(expr = xml2::read_html(url),
                    error = function(e) {
                      warning(paste("Could not read", url))
                      NA
                    })

  if (is.element("xml_node", set = class(temp))==TRUE) {
    if (is.null(container_class)==TRUE&is.null(container_id)==TRUE) {
      if (is.null(subelement)==TRUE) {
        temp <- temp %>%
          rvest::html_nodes(container) %>%
          rvest::html_text()
      } else {
        temp <- temp %>%
          rvest::html_nodes(container) %>%
          rvest::html_nodes(subelement) %>%
          rvest::html_text()
      }
    } else if (is.null(container_class)==FALSE&is.null(container_id)==TRUE) {
      if (is.null(subelement)==TRUE) {
        temp <- temp %>%
          rvest::html_nodes(xpath = paste0("//", container, "[@class='", container_class, "']")) %>%
          rvest::html_text()
      } else {
        temp <- temp %>%
          rvest::html_nodes(xpath = paste0("//", container, "[@class='", container_class, "']")) %>%
          rvest::html_nodes(subelement) %>%
          rvest::html_text() %>%
          paste(collapse = "\n")
      }

    } else if (is.null(container_class)==TRUE&is.null(container_id)==FALSE) {
      if (is.null(subelement)==TRUE) {
        temp <- temp %>%
          rvest::html_nodes(xpath = paste0("//", container, "[@id='", container_id, "']")) %>%
          rvest::html_text()
      } else {
        temp <- temp %>%
          rvest::html_nodes(xpath = paste0("//", container, "[@id='", container_id, "']")) %>%
          rvest::html_nodes(subelement) %>%
          rvest::html_text()
      }
    }
    if (length(temp)>1) {
      if (is.null(subelement)==TRUE) {
        if (is.null(container_instance)==FALSE) {
          text <- temp[container_instance]
        } else {
          text <- paste(temp, collapse = "\n")
        }

      } else {
        text <- paste(temp, collapse = "\n")
      }
    } else if (length(temp)==0) {
      text <- NA
    } else {
      text <- temp
    }
  }
  stringr::str_squish(text)
}
