#' Read Yahoo Data Solution File
#'
#' @param path The path to the downloaded file in excel format.
#' @param sheet Sheet to read. Either a string (the name of a sheet),
#' or an integer (the position of the sheet). It is NULL by default:
#' Reads all the sheets in the file (the entire period).
#' @param long The original data is date separated by variables,
#' and when *TRUE* is specified, it is converted to a format that
#' stores the date and observables (visitor) in one column each.
#' @details Reads the "Day-to-day transition of
#' Tokyo's 23 ward stay population estimates" data provided by
#' Yahoo Data Solution in an easy-to-use format.
#' @seealso [https://ds.yahoo.co.jp/](https://ds.yahoo.co.jp/)
#' @export
read_yds_tky23 <- function(path, sheet = NULL, long = FALSE) {
  if (is.null(sheet)) {
    sheet <-
      readxl::excel_sheets(path)
  }
  d <-
    sheet %>%
    purrr::map_dfr(
      function(.x) {
        d <-
          readxl::read_xlsx(path, sheet = .x) %>%
          tidyr::fill(1, .direction = "down")
        fix_colnames <-
          names(d) %>%
          purrr::modify_at(
            seq.int(3, ncol(d)),
            function(x) {
              x <-
                lubridate::as_date(as.numeric(x), origin = "1899-12-30")
              as.character(x)
            }
          )
        d %>%
          purrr::set_names(
            fix_colnames)
      })
  if (long == TRUE) {
    d <-
      d %>%
      tidyr::pivot_longer(seq.int(3, ncol(d)),
                          names_to = "date",
                          values_to = "visitors") %>%
      purrr::modify_at("date",
                       ~ lubridate::as_date(.x))
  }
  d
}
