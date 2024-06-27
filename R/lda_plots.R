
#' @keywords internal
plot_lda_convergence <- function(k_list, question) {

  convergence <- k_list |>
    purrr::map(
      \(x) x$logLiks |> dplyr::mutate(
        k = as.character(x$lda@k),
        loglikelihood = x$mean_logLik
      )
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
    ggplot2::geom_hline(
      ggplot2::aes(
        yintercept = loglikelihood
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    ) +
    ggplot2::scale_color_viridis_d(name = "k") +
    ggplot2::labs(
      title = question,
      subtitle = "Convergence of log-likelihoods at different topic numbers (k)"
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(k),
      scales = "free"
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
plot_lda_k_logLik <- function(k_list, question) {

  logliks <- k_list |>
    purrr::map(
      \(x) dplyr::tibble(k = x$lda@k, loglikelihood = x$mean_logLik)
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
    ggplot2::scale_x_continuous(breaks = logliks$k) +
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
