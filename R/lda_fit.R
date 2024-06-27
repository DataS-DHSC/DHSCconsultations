
#' Run LDA across a series of responses to get curves
#'
#' @export
#'
#'@importFrom rlang :=
#'
fit_lda <- function(
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
    dtm <- data |>
      dplyr::select(all_of(c(col_doc_id, col))) |>
      unnest_words(
        col,
        clean_text_fn = clean_text_fn,
        glossary_words = glossary_words,
        stop_words = stop_words,
        stem_word_exceptions = stem_word_exceptions,
        use_stemming = TRUE
      ) |>
      unnest_to_dtm(col_doc_id, col_word = "word")

    topic_number_range <- question_topic_numbers[[col]]

    return(
      topic_number_range |>
        rlang::set_names(as.character(topic_number_range)) |>
        purrr::map(\(k) run_lda_dtm(dtm, k, seed), .progress = "topic run progress")
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
view_lda_fit_convergence <- function(lda_list) {

  questions <- names(lda_list)

  plots_c <- questions |>
    purrr::map(\(x) plot_lda_convergence(lda_list[[x]], x))

  plots_c |>
    purrr::map(print)

  return(invisible(lda_list))
}


#' Title
#'
#' @param ngram_list
#' @param plot_rows
#'
#' @return
#' @export
#'
view_lda_fit_loglikelihood <- function(lda_list) {

  questions <- names(lda_list)

  plots_l <- questions |>
    purrr::keep(\(x) length(lda_list[[x]]) > 1) |>
    purrr::map(\(x) plot_lda_k_logLik(lda_list[[x]], x))

  plots_l |>
    purrr::map(print)

  plots_l |>
    patchwork::wrap_plots() |>
    print()

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
write_lda_fit <- function(
    lda_list, folder, prefix = basename(folder)
) {
  # create output folder if not already present
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  questions <- names(lda_list)

  c_plots <- questions |>
    purrr::map(\(x) plot_lda_convergence(lda_list[[x]], x))

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

  l_plots <- questions |>
    purrr::keep(\(x) length(lda_list[[x]]) > 1) |>
    purrr::map(\(x) plot_lda_k_logLik(lda_list[[x]], x))

  l_plots |>
    patchwork::wrap_plots() |>
    write_plot_png(
      file.path(
        folder,
        sprintf("%s_loglikelihood_all", prefix)
      )
    )

  l_plots |>
    seq_along() |>
    purrr::map(
      \(i) write_plot_png(
        l_plots[[i]],
        file.path(
          folder,
          sprintf("%s_loglikelihood_question_%02d", prefix, i)
        )
      )
    )

  saveRDS(
    lda_list,
    file.path(
      folder,
      sprintf("%s_fit_object.rds", prefix)
    )
  )

  return(invisible(lda_list))
}


