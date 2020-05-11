pc_html_link <- function(url, text) {
  stringr::str_c("<a href='", url, "' target='_blank'>", text, "</a>")
}
