% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/word_summary.R
\name{summarise_words}
\alias{summarise_words}
\title{Summarise all words in response text by frequency}
\usage{
summarise_words(data, clean_text_fn, glossary_words, stop_words, min_char = 2)
}
\arguments{
\item{data}{data frame containing response text for all questions.}

\item{clean_text_fn}{function used to clean question text.}

\item{glossary_words}{character vector of multi-word terms to be replaced
with single word equivalents (set to NULL to ignore).}

\item{stop_words}{character vector of common words to be removed from
un-nested values (set to NULL to ignore).}

\item{min_char}{minimum number of characters needed to be included in
analysis (default = 2).}
}
\value{
named list of data frame where names are questions and values are
word counts
}
\description{
Summarise all words in response text by frequency
}
