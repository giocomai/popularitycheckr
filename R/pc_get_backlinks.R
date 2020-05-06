#' Get referral backlinks to my website.
#'
#' @param date_range A characther vector of length two, providing the first and the last day to include in the data request, e.g. `c("2020-06-01", "2020-06-30")`.
#' @param sources_to_exclude A characther vector of domains to be excluded. By default, a vector of commonly found search engines, social media, and webmail providers. See popularitycheckr::sources_to_exclude
#' @param include_domain Logical, defaults to TRUE. Include the base domain in the `landingPagePath` column for clarity, assuming https protocol.
#' @param ga_id A Google Analytics viewId. Defaults to NULL. If not given, it uses the environment variable typically set with `pc_set_ga_id()`.
#'
#' @return A data frame (a tibble) of five columns: 'date', 'sessions', 'source', "fullReferrer", "landingPagePath",
#' @export
#'
#' @examples
#'
#' \dontrun{
#' pc_set_ga_id(1234567)
#'
#' backlinks <- pc_get_backlinks(date_range = c(Sys.Date()-32,
#' Sys.Date()-1))
#' backlinks
#'
#' }
pc_get_backlinks <- function(date_range = c(Sys.Date()-32, Sys.Date()-1),
                             sources_to_exclude = popularitycheckr::sources_to_exclude,
                             include_domain = TRUE,
                             ga_id = NULL) {
  if (is.null(ga_id)) {
    ga_id <- pc_set_ga_id()
  }

  fc <- googleAnalyticsR::filter_clause_ga4(list(googleAnalyticsR::dim_filter(dimension = "medium",
                                                                              expressions = "referral")))

  referrals <- googleAnalyticsR::google_analytics(viewId = ga_id,
                                                  date_range = date_range,
                                                  dimensions=c('source', 'medium', "fullReferrer", "landingPagePath", "date"),
                                                  metrics = c('sessions'),
                                                  dim_filters = fc,
                                                  anti_sample = TRUE)

  df <- referrals %>%
    dplyr::group_by(source, fullReferrer, landingPagePath, date) %>%
    dplyr::summarise(sessions = sum(sessions)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, sessions, dplyr::everything()) %>%
    dplyr::arrange(date, dplyr::desc(sessions))  %>%
    dplyr::filter(is.element(source, sources_to_exclude)==FALSE)

  if (include_domain == TRUE) {
    df <- df %>%
      dplyr::mutate(landingPagePath = paste0("https://", pc_set_domain(), landingPagePath))
  }

  df
}
