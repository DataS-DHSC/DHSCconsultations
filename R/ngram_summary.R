

#' Title
#'
#' @param data
#' @param text_cleaner
#' @param n
#' @param stop_words
#' @param glossary_words
#' @param col_id
#' @param cols_free_text
#'
#' @return
#' @export
#'
#' @examples
ngram_summary <- function(data, text_cleaner,
                          n = list("bi-gram" = 2, "tri-gram" = 3),
                          stop_words = NULL,
                          glossary_words = NULL,
                          col_id = NULL, cols_free_text = NULL) {

  if (is.null(stop_words)) stop_words <- tidytext::stop_words$word

  cols_free_text <- get_cols_free_text(data, col_id, cols_free_text)

  fn_n <- function(df, col, ngram_type) {
    unnest_ngrams(
      df,
      col,
      n[[ngram_type]],
      text_cleaner,
      glossary_words = glossary_words,
      stop_words = stop_words,
      col_word = ngram_type,
      n_min = 2
    )
  }

  ngram_list <- names(n)

  list_out <- ngram_list |>
    rlang::set_names(ngram_list) |>
    purrr::map(
      \(x) get_token_counts(data, cols_free_text, \(d, c) fn_n(d, c, x))
    )

  return(list_out)
}


#' Title
#'
#' @param ngram_list
#' @param plot_rows
#'
#' @return
#' @export
#'
view_ngram_summary <- function(ngram_list, plot_rows = 30) {
  for (token_type in names(ngram_list)) {
    view_token_counts(ngram_list[[token_type]], token_type, plot_rows)
  }
  return(invisible(ngram_list))
}


#' Title
#'
#' @param ngram_list
#' @param folder
#' @param plot_rows
#' @param prefix
#'
#' @return
#' @export
#'
write_ngram_summary <- function(
    ngram_list, folder, plot_rows = 30, prefix = basename(folder)
) {
  for (token_type in names(ngram_list)) {
    write_token_counts(
      ngram_list[[token_type]], folder, token_type, plot_rows, prefix
    )
  }
  return(invisible(ngram_list))
}
