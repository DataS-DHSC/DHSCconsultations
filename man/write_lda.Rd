% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lda_model.R
\name{write_lda}
\alias{write_lda}
\title{Write plots and LDA object to folder}
\usage{
write_lda(lda_list, folder, prefix = basename(folder))
}
\arguments{
\item{lda_list}{named list of LDA objects generated by the
\code{\link[=fit_lda]{fit_lda()}} function.}

\item{folder}{path to the folder in which to save (will create if not
present).}

\item{prefix}{prefix to use when naming files (defaults to
\code{basename(folder)})}
}
\value{
invisibly returns passed LDA list object
}
\description{
Outputs all plots and objects into the specified \code{folder} using the
passed \code{prefix} when naming files.
}
