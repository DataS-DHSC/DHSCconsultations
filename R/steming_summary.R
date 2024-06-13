

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
stemming_summary <- function(data, text_cleaner,
                             stem_word_exceptions,
                             stop_words = NULL,
                             glossary_words = NULL,
                             col_id = NULL, cols_free_text = NULL) {

  if (is.null(stop_words)) stop_words <- tidytext::stop_words$word

  cols_free_text <- get_cols_free_text(data, col_id, cols_free_text)

  fn_s <- function(col, use_stemming) {
    data |>
      dplyr::select(dplyr::all_of(c(col))) |>
      unnest_words(
        col,
        clean_text_fn = text_cleaner,
        glossary_words = glossary_words,
        stop_words = stop_words,
        stem_word_exceptions = stem_word_exceptions,
        col_word = "word",
        min_char = 1,
        use_stemming = use_stemming
      )
  }

  fn <- function(col) {
    bind_cols(
      fn_s(col, TRUE) |>
        rename(stemmed_word = word),

      fn_s(col, FALSE) |>
        rename(original_word = word)
    )
  }

  df_out <- cols_free_text |>
    rlang::set_names(cols_free_text) |>
    purrr::map(fn) |>
    dplyr::bind_rows() |>
    dplyr::filter(
      stemmed_word != original_word
    ) |>
    dplyr::summarise(
      original_words = paste(sort(unique(original_word)), collapse = ", "),
      n = dplyr::n(),
      .by = stemmed_word
    ) |>
    dplyr::arrange(dplyr::desc(n), stemmed_word)

  return(df_out)
}
