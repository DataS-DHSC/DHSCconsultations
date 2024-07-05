#' Convert an un-nested data frame to a document-term matrix
#'
#' @param data data frame of un-nested words to be converted.
#' @param col_resp_id character string of column containing the response ids.
#' @param col_word character string of column containing the
#'   un-nested words (default = "word")
#'
#' @return document-term matrix representation of the un-nested data frame.
#' @export
#'
#' @importFrom rlang .data
#'
unnest_to_dtm <- function(data, col_resp_id, col_word = "word") {
  assert_data_frame(data)
  assert_string(col_resp_id)
  assert_string(col_word)

  return(
    data |>
      dplyr::count(
        dplyr::across(dplyr::all_of(c(col_resp_id, col_word))),
        sort = TRUE
      ) |>
      tidytext::cast_dtm(!!col_resp_id, !!col_word, value = .data$n)
  )
}
