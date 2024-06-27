

#' Title
#'
#' @param data
#' @param text_cleaner
#' @param col_id
#' @param cols_free_text
#'
#' @return
#' @export
#'
summarise_text_cleaner <- function(data, text_cleaner,
                                   col_id = NULL, cols_free_text = NULL) {

  cols_free_text <- get_cols_free_text(data, col_id, cols_free_text)

  all_text <- data |>
    tidyr::unite(
      "_text",
      dplyr::all_of(cols_free_text),
      sep = " ",
      na.rm = TRUE
    ) |>
    dplyr::pull(`_text`) |>
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
    dplyr::count(text) |>
    dplyr::mutate(
      code = stringi::stri_escape_unicode(text),
      replacement = text_cleaner(text),
      replacement = stringr::str_replace_all(replacement, "\\s", "<space>"),
      replaced = dplyr::if_else(
        stringr::str_to_lower(text) == stringr::str_to_lower(replacement),
        FALSE,
        TRUE
      ),
      is_alnum = stringr::str_detect(text, "[:alnum:]"),
      is_punct = stringr::str_detect(text, "[:punct:]")
    ) |>
    dplyr::arrange(desc(n))

  return(df_out)
}
