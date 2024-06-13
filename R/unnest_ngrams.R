
#' Unnest text responses to a question into ngrams for analysis
#'
#' Note that the passed `clean_text_fn` should deal with stripping out
#' all punctuation, numbers, and changing case as required.
#'
#' @param data data frame containing question text and response id only.
#' @param col_question character string of column containing the
#'   question text to unnest.
#' @param n integer giving number of words in n-gram.
#' @param clean_text_fn function used to clean question text.
#' @param glossary_words set of multi-word character strings to be replaced with
#'   single word equivalents.
#' @param stop_words set of common words to be removed from unnested values.
#' @param stem_word_exceptions set of character strings not to stem. Note
#'   these strings must match exactly.
#' @param col_word character string for unnested column (default = "word")
#' @param min_char minimum number of characters needed to be included in
#'   analysis (default = 2)
#' @param use_stemming logical indicating if stemming should be applied (if set
#'   to FALSE the value of `stem_word_exceptions` is ignored)
#'
#' @return data frame of unnested words
#' @export
#'
#'@importFrom rlang :=
#'
unnest_ngrams <- function(
    data,
    col_question,
    n,
    clean_text_fn,
    glossary_words,
    stop_words,
    col_word = "ngram",
    n_min = n
) {

  stopifnot(
    "`data` must be a data frame" = is.data.frame(data),
    "`col_question` must be a single character string" =
      (is.character(col_question) || length(col_question) == 1),
    "`clean_text_fn` must be a function" = is.function(clean_text_fn),
    "`glossary_words` must be set of character strings" =
      (is.null(glossary_words) || is.character(glossary_words) ||
         is.vector(glossary_words)),
    "`stop_words` must be set of character strings" =
      (is.null(stop_words) || is.character(stop_words) ||
         is.vector(stop_words)),
    "`col_word` must be a single character string" =
      (is.character(col_word) || length(col_word) == 1)
  )

  # function to ensure consistency
  .format <- \(x) x |> clean_text_fn() |> unique()

  if(!is.null(stop_words)) {
    stop_words <- .format(stop_words)
  }

  data <- data |>
    dplyr::filter(
      !is.na(.data[[col_question]])
    ) |>
    dplyr::mutate(
      "{col_question}" := .data[[col_question]] |> clean_text_fn()
    ) |>
    dplyr::filter(
      "{col_question}" != ""
    )

  if (!is.null(glossary_words)) {
    data <- data |>
      dplyr::mutate(
        "{col_question}" := .data[[col_question]] |>
          replace_glossary(.format(glossary_words))
      )
  }

  # strange bug with unnest_token not handling strings correctly
  data <- data |>
    tidytext::unnest_tokens(
      output = !!rlang::sym(col_word),
      input = !!rlang::sym(col_question),
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
        n_stop = sum(is_stop),
        .by = id
      ) |>
      filter(n - n_stop >= n_min) |>
      select(all_of(col_word))
  }

  return(data)
}
