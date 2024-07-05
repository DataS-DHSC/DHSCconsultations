#' Produce summary table of terms being stemmed ordered by frequency
#'
#' @param data data frame containing response text for all questions.
#' @param clean_text_fn function used to clean question text.
#' @param stop_words character vector of common words to be removed from
#'   un-nested values (set to NULL to ignore).
#' @param glossary_words character vector of multi-word terms to be replaced
#'   with single word equivalents (set to NULL to ignore).
#' @param stem_word_exceptions character vector of terms not to stem. Note
#'   these strings must match exactly.
#'
#' @return data frame of stemmed terms
#' @export
#'
summarise_stemming <- function(
    data,
    clean_text_fn,
    glossary_words,
    stop_words,
    stem_word_exceptions) {
  assert_data_frame(data)
  assert_function(clean_text_fn)
  assert_character(glossary_words, null.ok = TRUE)
  assert_character(stop_words, null.ok = TRUE)
  assert_character(stem_word_exceptions)

  cols_free_text <- names(data)

  fn_s <- function(col, use_stemming) {
    if (use_stemming) {
      swe <- stem_word_exceptions
    } else {
      swe <- NULL
    }

    data |>
      dplyr::select(dplyr::all_of(c(col))) |>
      unnest_words(
        col,
        clean_text_fn = clean_text_fn,
        glossary_words = glossary_words,
        stop_words = stop_words,
        stem_word_exceptions = swe,
        col_word = "word",
        min_char = 1
      )
  }

  fn <- function(col) {
    dplyr::bind_cols(
      fn_s(col, TRUE) |>
        dplyr::rename(stemmed_word = .data[["word"]]),
      fn_s(col, FALSE) |>
        dplyr::rename(original_word = .data[["word"]])
    )
  }

  df_out <- cols_free_text |>
    rlang::set_names(cols_free_text) |>
    purrr::map(fn) |>
    dplyr::bind_rows() |>
    dplyr::filter(
      .data[["stemmed_word"]] != .data[["original_word"]]
    ) |>
    dplyr::summarise(
      original_words = paste(
        sort(unique(.data[["original_word"]])),
        collapse = ", "
      ),
      n = dplyr::n(),
      .by = .data[["stemmed_word"]]
    ) |>
    dplyr::arrange(dplyr::desc(.data[["n"]]), .data[["stemmed_word"]])

  return(df_out)
}
