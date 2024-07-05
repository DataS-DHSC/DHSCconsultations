#' Produce summary table of characters replaced by text cleaning function
#'
#' @param data data frame containing response text for all questions.
#' @param clean_text_fn function used to clean question text.
#'
#' @return data frame of character replacements
#' @export
#'
summarise_text_cleaner <- function(
    data,
    clean_text_fn) {
  assert_data_frame(data)
  assert_function(clean_text_fn)

  all_text <- data |>
    tidyr::unite(
      "_text",
      dplyr::everything(),
      sep = " ",
      na.rm = TRUE
    ) |>
    dplyr::pull(.data[["_text"]]) |>
    paste0(collapse = " ")

  # Note that the regex below should be the inverse of that used in clean_text
  # for keeping characters
  df_out <-
    dplyr::tibble(
      text = stringr::str_extract_all(
        all_text,
        "[^[0-9a-zA-Z]'\\s]|\\b[A-Z]+\\b"
      )[[1]]
    ) |>
    dplyr::count(.data[["text"]]) |>
    dplyr::mutate(
      code = stringi::stri_escape_unicode(.data[["text"]]),
      replacement = clean_text_fn(.data[["text"]]),
      replacement = stringr::str_replace_all(
        .data[["replacement"]], "\\s", "<space>"
      ),
      replaced = dplyr::if_else(
        stringr::str_to_lower(.data[["text"]]) ==
          stringr::str_to_lower(.data[["replacement"]]),
        FALSE,
        TRUE
      ),
      is_alnum = stringr::str_detect(.data[["text"]], "[:alnum:]"),
      is_punct = stringr::str_detect(.data[["text"]], "[:punct:]")
    ) |>
    dplyr::arrange(dplyr::desc(.data[["n"]]))

  return(df_out)
}
