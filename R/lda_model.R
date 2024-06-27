
#' Calculate LDA topic model
#'
#' @param dtm document-term matrix
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
run_lda_dtm <- function(dtm, k, seed, burn_in = 1000, iter = 1000, keep = 50){
  out <- list()

  lda <- topicmodels::LDA(
    dtm,
    k = k,
    method = "Gibbs",
    model = NULL,
    control = list(
      best = TRUE,
      burnin = 0,
      iter = iter + burn_in,
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

  mean_logLik <- logLiks |>
    dplyr::filter(iteration >= burn_in) |>
    dplyr::pull(logLik) |>
    mean(na.rm = TRUE)

  return(
    list(
      lda = lda,
      beta = beta,
      gamma = gamma,
      mean_logLik = mean_logLik,
      final_logLik = lda@loglikelihood,
      logLiks = logLiks
    )
  )
}


#' Title
#'
#' @param data
#' @param col_doc_id
#' @param question_topic_numbers
#' @param seed
#' @param clean_text_fn
#' @param glossary_words
#' @param stop_words
#' @param stem_word_exceptions
#'
#' @return
#' @export
#'
#' @examples
run_lda <- function(
    data,
    col_doc_id,
    question_topic_numbers,
    seed,
    clean_text_fn,
    glossary_words,
    stop_words,
    stem_word_exceptions = NULL
) {
  fn <- function(col) {
    return(
      data |>
        dplyr::select(all_of(c(col_doc_id, col))) |>
        unnest_words(
          col,
          clean_text_fn = clean_text_fn,
          glossary_words = glossary_words,
          stop_words = stop_words,
          stem_word_exceptions = stem_word_exceptions,
          use_stemming = TRUE
        ) |>
        unnest_to_dtm(col_doc_id, col_word = "word") |>
        run_lda_dtm(
          question_topic_numbers[[col]],
          seed
        )
    )
  }

  questions <- names(question_topic_numbers)
  lda_out <- questions |>
    rlang::set_names(questions) |>
    purrr::map(fn, .progress = "questions processed")

  return(lda_out)
}


#' Title
#'
#' @param ngram_list
#' @param plot_rows
#'
#' @return
#' @export
#'
view_lda_convergence <- function(lda_list) {

  questions <- names(lda_list)

  plots_c <- questions |>
    purrr::map(\(x) plot_lda_convergence(list(lda_list[[x]]), x))

  plots_c |>
    purrr::map(print)

  return(invisible(lda_list))
}


#' Title
#'
#' @param ngram_list
#' @param folder
#' @param plot_rows
#' @param prefix
#'
#' @return
#' @export
#'
write_lda <- function(
    lda_list, folder, prefix = basename(folder)
) {
  # create output folder if not already present
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  questions <- names(lda_list)

  c_plots <- questions |>
    purrr::map(\(x) plot_lda_convergence(list(lda_list[[x]]), x))

  c_plots |>
    seq_along() |>
    purrr::map(
      \(i) write_plot_png(
        c_plots[[i]],
        file.path(
          folder,
          sprintf("%s_convergence_question_%02d", prefix, i)
        )
      )
    )

  saveRDS(
    lda_list,
    file.path(
      folder,
      sprintf("%s_object.rds", prefix)
    )
  )

  return(invisible(lda_list))
}
