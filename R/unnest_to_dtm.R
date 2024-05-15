
#' Convert an unnested data frame to a document-term matrix
#'
#' @param data data frame of unnested words to be converted.
#' @param col_doc_id character string of column containing the document ids.
#' @param col_word character string of column containing the
#'   unnested words (default = "word")
#'
#' @return document-term matrix representation of the unnested data frame.
#' @export
#'
#' @importFrom rlang .data
#'
unnest_to_dtm <- function(data, col_doc_id, col_word = "word") {
  stopifnot(
    "`data` must be a data frame" = is.data.frame(data),
    "`col_doc_id` must be a single character string" =
      (is.character(col_doc_id) || length(col_doc_id) == 1),
    "`col_word` must be a single character string" =
      (is.character(col_word) || length(col_word) == 1)
  )

  return(
    data |>
      dplyr::count(
        dplyr::across(dplyr::all_of(c(col_doc_id, col_word))),
        sort = TRUE
      ) |>
      tidytext::cast_dtm(!!col_doc_id, !!col_word, value = .data$n)
  )
}
