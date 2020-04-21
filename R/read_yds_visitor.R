#' Read Yahoo Data Solution's File
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' @param path The path to the downloaded file in excel format.
#' @param sheet Sheet to read. Either a string (the name of a sheet),
#' or an integer (the position of the sheet). It is NULL by default:
#' All sheets in the file except for the calendar data.
#' @param long The original data is date separated by variables,
#' and when *TRUE* is specified, it is converted to a format that
#' stores the date and observable in one column each.
#' @details Reads the "visitor" data provided by
#' Yahoo Data Solution in an easy-to-use format.
#' @seealso [https://ds.yahoo.co.jp/](https://ds.yahoo.co.jp/)
#' @export
read_yds_visitor <- function(path, sheet = NULL, long = FALSE) {
  sheets <-
    readxl::excel_sheets(path)
  if (!is.null(sheet)) {
    if (is.character(sheet)) {
      sheets <-
        which(grepl(paste0(sheet, collapse = "|"),
                    sheets))
    } else {
      sheets <-
        sheets[sheet]
    }
  }
  d <-
    sheets %>%
    stringr::str_subset("\u6bd4\u8f03\u65e5\u5bfe\u5fdc\u8868",
                        negate = TRUE) %>%
    purrr::map(
      function(.x) {
        d <-
          readxl::read_xlsx(path, sheet = .x) %>%
          purrr::modify_at(1,
                           ~ lubridate::as_date(.x))
        d %>%
          purrr::set_names(
            names(d) %>%
              purrr::modify_at(1,
                               ~ assign(.x,
                                        "\u65e5\u4ed8")))
      }
    ) %>%
    purrr::reduce(merge) %>%
    tibble::as_tibble()
  if (long == TRUE) {
    d <-
      d %>%
      tidyr::pivot_longer(seq.int(2, ncol(d)),
                          names_to = "area",
                          values_to = "value")
    d <-
      d %>%
      purrr::set_names(
        names(d) %>%
          purrr::modify_at(1,
                           ~ assign(.x, "date"))
      )
  }
  d
}
