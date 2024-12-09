#' Stem un-nested words before LDA analysis
#'
#' Uses the [SnowballC::wordStem()] function to apply Dr Porter's stemming
#' algorithm.
#'
#' @param x character vector of words to be stemmed.
#' @param exceptions optional character vector of strings not to stem. Note
#'   these strings must match exactly.
#'
#' @return character vector with their porter stemmed version.
#' @export
#'
#' @examplesIf interactive()
#'
#' words <- c("win", "winning", "winner", NA)
#'
#' stem_words(words)
#'
stem_words <- function(x, exceptions = NULL) {
  assert_character(x)
  assert_character(exceptions, null.ok = TRUE)

  word_stems <- SnowballC::wordStem(x)

  if (!is.null(exceptions)) {
    x <- dplyr::if_else(x %in% exceptions, x, word_stems)
  } else {
    x <- word_stems
  }

  return(x)
}
