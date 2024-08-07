% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unnest_words.R
\name{unnest_words}
\alias{unnest_words}
\title{Unnest text responses to a question into words for analysis by LDA}
\usage{
unnest_words(
  data,
  col_free_text,
  clean_text_fn,
  glossary_words,
  stop_words,
  stem_word_exceptions,
  col_word = "word",
  min_char = 2
)
}
\arguments{
\item{data}{data frame containing response text and ids to a single question.}

\item{col_free_text}{character string of column containing the
question text to un-nest.}

\item{clean_text_fn}{function used to clean question text.}

\item{glossary_words}{character vector of multi-word terms to be replaced
with single word equivalents (set to NULL to ignore).}

\item{stop_words}{character vector of common words to be removed from
un-nested values (set to NULL to ignore).}

\item{stem_word_exceptions}{character vector of terms not to stem (set to
NULL to ignore). Note these strings must match exactly.}

\item{col_word}{character string for un-nested column (default = "word")}

\item{min_char}{minimum number of characters needed to be included in
analysis (default = 2).}
}
\value{
data frame of un-nested words
}
\description{
Note that the passed \code{clean_text_fn} should deal with stripping out
all punctuation, numbers, and changing case as required.
}
