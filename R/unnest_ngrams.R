#' Un-nest text responses to a question into n-grams for analysis
#'
#' Note that the passed `clean_text_fn` should deal with stripping out
#' all punctuation, numbers, and changing case as required.
#'
#' @param data data frame containing response text and ids to a single question.
#' @param col_free_text character string of column containing the
#'   question text to un-nest.
#' @param n integer giving number of words in n-gram (min = 2).
#' @param clean_text_fn function used to clean question text.
#' @param glossary_words character vector of multi-word terms to be replaced
#'   with single word equivalents (set to NULL to ignore).
#' @param stop_words character vector of common words to be removed from
#'   un-nested values (set to NULL to ignore).
#' @param col_word character string for un-nested column (default = "ngram")
#' @param n_min integer giving minimum number of non-stop words in n-gram to be
#'   included (default = `n`).
#'
#' @return data frame of un-nested n-grams.
#' @export
#'
#' @importFrom rlang :=
#'
unnest_ngrams <- function(
    data,
    col_free_text,
    n,
    clean_text_fn,
    glossary_words,
    stop_words,
    col_word = "ngram",
    n_min = n) {
  assert_data_frame(data)
  assert_string(col_free_text)
  assert_integerish(n, lower = 2, max.len = 1)
  assert_function(clean_text_fn)
  assert_character(glossary_words, null.ok = TRUE)
  assert_character(stop_words, null.ok = TRUE)
  assert_string(col_word)
  assert_integerish(n, lower = n, max.len = 1)

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
      token = "ngrams",
      format = "text",
      to_lower = FALSE,
      drop = TRUE,
      # below are passed to tokenizers::tokenize_words
      n = n,
      n_min = n,
      ngram_delim = " ",
      stopwords = ifelse(n_min == n, stop_words, character()),
      simplify = FALSE
    )

  if (length(stop_words) > 0) {
    data <- data |>
      dplyr::mutate(
        id = dplyr::row_number()
      ) |>
      tidyr::separate_longer_delim(
        .data[[col_word]],
        delim = " "
      ) |>
      dplyr::mutate(
        is_stop = .data[[col_word]] %in% stop_words
      ) |>
      dplyr::summarise(
        "{col_word}" := paste(.data[[col_word]], collapse = " "),
        n_stop = sum(.data[["is_stop"]]),
        .by = .data[["id"]]
      ) |>
      dplyr::filter(n - .data[["n_stop"]] >= n_min) |>
      dplyr::select(dplyr::all_of(col_word))
  }

  return(data)
}
