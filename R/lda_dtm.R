
#' Calculate LDA topic model
#'
#' @param data document-term matrix
#' @param k number of topics to detect
#' @param seed seed for Gibbs LDA
#' @param burn_in parameter for Gibbs LDA
#' @param iter parameter for Gibbs LDA
#' @param keep parameter for Gibbs LDA
#'
#' @return list with elements: lda_out (full LDA topicmodel), beta (beta scores),
#' gamma (gamma scores), and logLik (Log likelihood of topicmodel object)
#' @export
#'
lda_dtm <- function(data, k, seed, burn_in = 1000, iter = 1000, keep = 50){
  out <- list()

  lda <- topicmodels::LDA(
    data,
    k = k,
    method = "Gibbs",
    model = NULL,
    control = list(
      best = TRUE,
      burnin = burn_in,
      iter = iter,
      keep = keep,
      seed = seed
    ),
    seedwords = NULL
  )

  beta <- tidytext::tidy(lda, matrix = "beta")
  gamma <- tidytext::tidy(lda, matrix = "gamma")

  logLiks <- dplyr::tibble(
    iteration = seq_along(lda@logLiks) * keep,
    logLik = lda@logLiks
  )

  return(
    list(
      lda = lda,
      beta = beta,
      gamma = gamma,
      loglikelihood = lda@loglikelihood,
      logLiks = logLiks
    )
  )
}
