#' Unnest text responses to a question into words for analysis by LDA
#'
#' Note that the passed `clean_text_fn` should deal with stripping out
#' all punctuation, numbers, and changing case as required.
#'
#' @param data data frame containing response text and ids to a single question.
#' @param col_free_text character string of column containing the
#'   question text to un-nest.
#' @param clean_text_fn function used to clean question text.
#' @param glossary_words character vector of multi-word terms to be replaced
#'   with single word equivalents (set to NULL to ignore).
#' @param stop_words character vector of common words to be removed from
#'   un-nested values (set to NULL to ignore).
#' @param stem_word_exceptions character vector of terms not to stem (set to
#'   NULL to ignore). Note these strings must match exactly.
#' @param col_word character string for un-nested column (default = "word")
#' @param min_char minimum number of characters needed to be included in
#'   analysis (default = 2).
#'
#' @return data frame of un-nested words
#' @export
#'
#' @importFrom rlang :=
#'
unnest_words <- function(
    data,
    col_free_text,
    clean_text_fn,
    glossary_words,
    stop_words,
    stem_word_exceptions,
    col_word = "word",
    min_char = 2) {
  assert_data_frame(data)
  assert_string(col_free_text)
  assert_function(clean_text_fn)
  assert_character(glossary_words, null.ok = TRUE)
  assert_character(stop_words, null.ok = TRUE)
  assert_character(stem_word_exceptions, null.ok = TRUE)
  assert_string(col_word)
  assert_integerish(min_char, lower = 1, max.len = 1)

  # function to ensure consistency
  .format <- \(x) {
    x |>
      clean_text_fn() |>
      unique()
  }

  if (!is.null(stop_words)) {
    stop_words <- .format(stop_words)
  } else {
    stop_words <- character()
  }

  data <- data |>
    dplyr::filter(
      !is.na(.data[[col_free_text]])
    ) |>
    dplyr::mutate(
      "{col_free_text}" := .data[[col_free_text]] |> clean_text_fn()
    ) |>
    dplyr::filter(
      "{col_free_text}" != ""
    )

  if (!is.null(glossary_words)) {
    data <- data |>
      dplyr::mutate(
        "{col_free_text}" := .data[[col_free_text]] |>
          replace_glossary(.format(glossary_words))
      )
  }

  # strange bug with unnest_token not handling strings correctly
  data <- data |>
    tidytext::unnest_tokens(
      output = !!rlang::sym(col_word),
      input = !!rlang::sym(col_free_text),
      token = "words",
      format = "text",
      to_lower = FALSE,
      drop = TRUE,
      # below are passed to tokenizers::tokenize_words
      stopwords = stop_words,
      strip_punct = FALSE,
      strip_numeric = FALSE,
      simplify = FALSE
    )

  if (!is.null(stem_word_exceptions)) {
    stem_word_exceptions <- .format(stem_word_exceptions)

    data <- data |>
      dplyr::mutate(
        "{col_word}" := .data[[col_word]] |>
          stem_words(exceptions = stem_word_exceptions)
      )
  }

  data <- data |>
    dplyr::filter(
      nchar(.data[[col_word]]) >= min_char
    )

  return(data)
}
