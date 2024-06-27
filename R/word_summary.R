

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
summarise_words <- function(data, text_cleaner,
                            min_char = 2,
                            stop_words = NULL,
                            glossary_words = NULL,
                            col_id = NULL, cols_free_text = NULL) {

  if (is.null(stop_words)) stop_words <- tidytext::stop_words$word

  cols_free_text <- get_cols_free_text(data, col_id, cols_free_text)

  fn <- function(df, col) {
    unnest_words(
      df,
      col,
      text_cleaner,
      glossary_words = glossary_words,
      stop_words = stop_words,
      stem_word_exceptions = NULL,
      col_word = "word",
      min_char = min_char,
      use_stemming = FALSE
    )
  }

  df_out <- get_token_counts(data, cols_free_text, fn)

  return(df_out)
}


#' Title
#'
#' @param ngram_list
#' @param plot_rows
#'
#' @return
#' @export
#'
view_word_summary <- function(data) {
  view_token_counts(data, "word")
  return(invisible(data))
}


#' Title
#'
#' @param ngram_list
#' @param plot_rows
#'
#' @return
#' @export
#'
plot_word_summary <- function(data, plot_rows = 30) {
  plot_token_counts(data, plot_rows)
  return(invisible(data))
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
write_word_summary <- function(
    data, folder, plot_rows = 30, prefix = basename(folder)
) {
  write_token_counts(data, folder, "word", plot_rows, prefix)

  return(invisible(data))
}
