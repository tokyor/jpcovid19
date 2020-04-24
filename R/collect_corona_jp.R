#' Collect mobility data
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @param source Data source. Choose between "agoop", "docomo" or "kddi".
#' @details Data on the rate of decline in human mobility from
#' two data sources ("Agoop", "Docomo" or "KDDI") will be obtained from the website.
#' The data is updated on a daily basis and shows the values
#' for each of several areas, compared to the state before the declaration.
#' @seealso [https://corona.go.jp/dashboard/](https://corona.go.jp/dashboard/)
#' @export
collect_corona_go_jp <- function(source) {
  . <- data <- value <- type <- area <- text <- NULL
  rlang::arg_match(source,
                   c("agoop", "docomo", "kddi"))
  source <-
    dplyr::recode(source,
                  `agoop` = "reduction_rate_busy_quarter.json",
                  `docomo` = "reduction_rate.json",
                  `kddi` = "reduction_rate_tourist_site.json")
  paste0("https://data.corona.go.jp/converted-json/",
           source) %>%
    jsonlite::fromJSON() %>%
    purrr::modify_at(1,
                     ~ lubridate::as_date(.x)) %>%
    purrr::modify_at(c(4, 5, 6),
                     as.numeric)
}
