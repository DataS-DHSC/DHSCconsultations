
#' Run LDA across a series of responses to get curves
#'
#' @export
#'
#'@importFrom rlang :=
#'
lda_fitter <- function(
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
      select(all_of(c(col_doc_id, col))) |>
      unnest_words(
        col,
        clean_text_fn = clean_text_fn,
        glossary_words = glossary_words,
        stop_words = stop_words,
        stem_word_exceptions = stem_word_exceptions,
        use_stemming = TRUE
      ) %>%
      unnest_to_dtm(col_doc_id, col_word = "word")

    topic_number_range <- question_topic_numbers[[col]]

    return(
      topic_number_range |>
        rlang::set_names(as.character(topic_number_range)) |>
        purrr::map(\(k) lda_model(dtm, k, seed), .progress = "topic run progress")
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
view_lda_fitter <- function(lda_list) {

  questions <- names(lda_list)

  questions |>
    purrr::map(\(x) plot_convergence(lda_list[[x]], x)) |>
    patchwork::wrap_plots() |>
    print()

  questions |>
    purrr::keep(\(x) length(lda_list[[x]]) > 1) |>
    purrr::map(\(x) plot_logLik(lda_list[[x]], x)) |>
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
write_lda_fitter <- function(
    lda_list, folder, prefix = basename(folder)
) {
  # create output folder if not already present
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  questions <- names(lda_list)

  c_plots <- questions |>
    purrr::map(\(x) plot_convergence(lda_list[[x]], x))

  c_plots |>
    patchwork::wrap_plots() |>
    ggplot2::ggsave(
      file.path(
        folder,
        sprintf("%s_convergence_all.png", prefix)
      ),
      plot = _
    )

  c_plots |>
    seq_along() |>
    purrr::map(
      \(i) ggplot2::ggsave(
        file.path(
          folder,
          sprintf("%s_convergence_question_%02d.png", prefix, i)
        ),
        plot = c_plots[[i]]
      )
    )

  l_plots <- questions |>
    purrr::keep(\(x) length(lda_list[[x]]) > 1) |>
    purrr::map(\(x) plot_logLik(lda_list[[x]], x))

  l_plots |>
    patchwork::wrap_plots() |>
    ggplot2::ggsave(
      file.path(
        folder,
        sprintf("%s_loglikelihood_all.png", prefix)
      ),
      plot = _
    )

  l_plots |>
    seq_along() |>
    purrr::map(
      \(i) ggplot2::ggsave(
        file.path(
          folder,
          sprintf("%s_loglikelihood_question_%02d.png", prefix, i)
        ),
        plot = l_plots[[i]]
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


#' @keywords internal
plot_convergence <- function(k_list, question) {

  convergence <- k_list |>
    purrr::map(
      \(x) x$logLiks |> dplyr::mutate(k = as.character(x$lda@k))
    ) |>
    dplyr::bind_rows()


  plt <-
    ggplot2::ggplot(convergence) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = iteration, y = logLik,
        colour = reorder(k, as.numeric(k))
      ),
      linewidth = 2
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::scale_color_viridis_d(name = "k") +
    ggplot2::labs(
      title = question,
      subtitle = "Convergence of log-likelihoods at different topic numbers (k)"
    )

  return(plt)
}


#' @keywords internal
integer_breaks <- function(n = 5, ...) {
  fn <- function(x) {
    breaks <- ceiling(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fn)
}


#' @keywords internal
plot_logLik <- function(k_list, question) {

  logliks <- k_list |>
    purrr::map(
      \(x) dplyr::tibble(k = x$lda@k, loglikelihood = x$loglikelihood)
    ) |>
    dplyr::bind_rows()


  plt <-
    ggplot2::ggplot(logliks) +
    ggplot2::geom_line(
      ggplot2::aes(x = k, y = loglikelihood),
      colour = "#00ad93", #dhsc_primary()
      linewidth = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = k, y = loglikelihood),
      colour = "#00ad93" #dhsc_primary()
    ) +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = question,
      subtitle = "Log-likelihoods at different topic numbers (k)"
    )

  return(plt)
}

