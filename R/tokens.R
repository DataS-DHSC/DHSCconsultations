
#' @keywords internal
get_token_counts <- function(data, cols_free_text, fn) {

  token_counts <- cols_free_text |>
    rlang::set_names(cols_free_text) |>
    purrr::map(
      \(x) data |>
        dplyr::select(dplyr::all_of(c(x))) |>
        fn(x) |>
        dplyr::count(dplyr::across(dplyr::everything()), sort = TRUE)
    )

  return(token_counts)
}


#' @keywords internal
view_token_counts <- function(token_counts, token_type, plot_rows) {

  display <- function(x) {
    token_counts[[x]] |>
      tibble::view(title = sprintf("%s: %s count", x, token_type))

    token_counts[[x]] |>
      plot_token_counts(x, plot_rows) |>
      print()
  }

  token_counts |>
    names() |>
    purrr::map(display)

  invisible(token_counts)
}


#' @keywords internal
write_token_counts <- function(
    token_counts, folder, token_type, plot_rows, prefix
) {

  # create output folder if not already present
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  # write xlsx file of all data
  token_counts |>
    writexl::write_xlsx(
      file.path(
        folder,
        sprintf("%s_%s_count.xlsx", prefix, token_type)
      )
    )

  questions <- names(token_counts)

  questions |>
    seq_along() |>
    purrr::map(
      \(i) plot_token_counts(token_counts[[i]], questions[[i]], plot_rows) |>
        ggplot2::ggsave(
          file.path(
            folder,
            sprintf("%s_%s_count_question_%02d.png", prefix, token_type, i)
          ),
          plot = _
        )
    )
}

#' @keywords internal
plot_token_counts <- function(token_counts, question, plot_rows) {
  token_counts <- token_counts |>
    dplyr::mutate(rank = dplyr::row_number(dplyr::desc(n))) |>
    dplyr::filter(rank <= plot_rows)

  x_val <- setdiff(names(token_counts), c("n", "rank"))

  plt <-
    ggplot2::ggplot(token_counts) +
    ggplot2::geom_col(
      ggplot2::aes(x = reorder(.data[[x_val]], -rank), y = n),
      fill = "#00ad93" #dhsc_primary()
    ) +
    ggplot2::geom_text(
      aes(x = .data[[x_val]], y = n, label = n),
      hjust = 1.5,
      position = ggplot2::position_stack(vjust = 0.9),
      colour = "white"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = question,
      subtitle = sprintf(
        "Count of top %d %ss",
        plot_rows,
        x_val
      )
    )

  return(plt)
}
