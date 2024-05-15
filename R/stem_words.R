
#' Stem unnested words before LDA analysis
#'
#' @param x character vector of words to be stemmed.
#' @param exceptions optional character vector of strings not to stem. Note
#'   these strings must match exactly.
#'
#' @return character vector with their porter stemmed version.
#' @export
#'
#' @examplesIf interactive()
#' df <- data.frame(id = c(1, 2, 3, 4), word = c("win", "winning", "winner", NA))
#' stem_words(df, word)
#'
stem_words <- function(x, exceptions = NULL) {

  stopifnot(
    "`x` must be a character vector" = (is.character(x) || is.vector(x)),
    "`exceptions` must be NULL or a character vector" =
      (is.null(exceptions) || is.character(exceptions) || is.vector(exceptions))
  )

  word_stems <- SnowballC::wordStem(x)

  if (!is.null(exceptions)) {
    x <- dplyr::if_else(x %in% exceptions, x, word_stems)
  } else {
    x <- word_stems
  }

  return(x)
}
