#' Replace multi-word glossary terms with single word equivalents
#'
#' This function allows multi-word terms to be preserved during LDA.
#' For example, National Health Service becomes "nationalhealthservice" rather
#' than "national", "health", and "service"
#'
#' @param x character vector of text to be transformed.
#' @param glossary character vector of multi-word character strings to be
#'   replaced.
#'
#' @return character vector with glossary words replaced with their single word
#'   equivalents.
#' @export
#'
replace_glossary <- function(x, glossary) {
  assert_character(x)
  assert_character(glossary)

  glossary_pattern <- glossary |>
    stringr::str_replace_all("\\s", "") |>
    stats::setNames(paste0("\\b", glossary, "\\b"))

  x <- stringr::str_replace_all(x, glossary_pattern)

  return(x)
}


#' Return glossary terms from www.thinklocalactpersonal.org.uk
#'
#' @return list of multi-word glossary terms
#' @export
#'
get_tlap_glossary <- function() {
  tlap_url <- paste0(
    "https://www.thinklocalactpersonal.org.uk/Browse/Informationandadvice",
    "/CareandSupportJargonBuster/"
  )

  glossary <- rvest::read_html(tlap_url) |>
    rvest::html_elements("a.term") |>
    rvest::html_text() |>
    stringr::str_squish() |>
    stringr::str_replace_all("\\s*\\(.*", "") |>
    stringr::str_replace_all("\\s*\\[.*", "")

  glossary <- glossary[stringr::str_detect(glossary, " ")]

  return(glossary)
}
