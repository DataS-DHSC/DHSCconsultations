
#' Unnest text responses to a question into words for analysis by LDA
#'
#' Note that the passed `clean_text_fn` should deal with stripping out
#' all punctuation, numbers, and changing case as required.
#'
#' @param data data frame containing question text and response id only.
#' @param col_question character string of column containing the
#'   question text to unnest.
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
unnest_words <- function(
    data,
    col_question,
    clean_text_fn,
    glossary_words,
    stop_words,
    stem_word_exceptions = NULL,
    col_word = "word",
    min_char = 2,
    use_stemming = TRUE
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
    "`stem_word_exceptions` must be set of character strings" =
      (is.null(stem_word_exceptions) || is.character(stem_word_exceptions) ||
         is.vector(stem_word_exceptions)),
    "`col_word` must be a single character string" =
      (is.character(col_word) || length(col_word) == 1),
    "`min_char` must be a single integer" =
      (is.numeric(min_char) || length(min_char) == 1 ||
         min_char == as.integer(min_char)),
    "`use_stemming` must be a logical" = is.logical(use_stemming)
  )

  # function to ensure consistency
  .format <- \(x) x |> clean_text_fn() |> unique()

  if(!is.null(stop_words)) {
    stop_words <- .format(stop_words)
  }

  if(!is.null(stem_word_exceptions)) {
    stem_word_exceptions <- .format(stem_word_exceptions)
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
      output = !!dplyr::sym(col_word),
      input = !!dplyr::sym(col_question),
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

  if (use_stemming) {
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
