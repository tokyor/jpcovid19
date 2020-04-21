#' Collect mobility data
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @param source Data source. Choose between "agoop" or "docomo".
#' @details Data on the rate of decline in human mobility from
#' two data sources ("Agoop" or "Docomo") will be obtained from the website.
#' The data is updated on a daily basis and shows the values
#' for each of several areas, compared to the state before the declaration.
#' @seealso [https://corona.go.jp](https://corona.go.jp)
#' @export
collect_corona_go_jp <- function(source) {
  . <- data <- value <- type <- text <- NULL
  rlang::arg_match(source,
                   c("agoop", "docomo"))
  source <-
    dplyr::recode(source,
                  `docomo` = "docomo_zenkoku") # docomo_todohuken
  ts <-
    round(as.numeric(lubridate::now()), digits = 0)
  df_source <-
    paste0("https://corona.go.jp/toppage/json/",
           source,
           "_data.json?_=",
           ts) %>%
    jsonlite::fromJSON()
  d1 <-
    df_source %>%
    purrr::pluck("info") %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    purrr::set_names(c("owner", "update_time"))
  d2 <-
    df_source %>%
    purrr::pluck("data") %>%
    tidyr::unnest(cols = data) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(area = stringr::str_replace_all(area, "<br>", "_"),
                  value = as.numeric(value),
                  text = stringr::str_remove_all(text, "\u203b")) %>%
    dplyr::mutate(value = dplyr::if_else(type == "low", -value, value)) %>%
    purrr::discard(names(.) == "type") %>%
    tidyr::pivot_wider(
      names_from = text,
      values_from = value)
  merge(d1, d2)
}
