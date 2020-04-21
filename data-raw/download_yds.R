library(rvest)
x <- "https://ds.yahoo.co.jp/report/" %>%
  read_html()

download_files <-
  x %>%
  html_nodes(css = "div.df-MainContent__inner > a") %>%
  html_attr("href") %>%
  stringr::str_subset(".xlsx$")
download_files %>%
  purrr::walk(
    ~ download.file(.x,
                  destfile = here::here(glue::glue("data-raw/{file}",
                                        file = basename(.x))))
  )
