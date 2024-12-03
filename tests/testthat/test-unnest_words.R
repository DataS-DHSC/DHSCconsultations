test_that("unnest_words works", {
  # simple cleaning function for testing purposes
  clean_words <- function(x) {
    x |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("[^[:alnum:]]", " ") |>
      stringr::str_squish()
  }

  # simplified options for testing purposes - no glossary or stem exceptions
  resp <- unnest_words(
    dummy_response,
    "Please share your views on policy 1",
    clean_words, "", tidytext::stop_words$word, ""
  )

  # check that the output is a data frame
  checkmate::expect_data_frame(resp)

  # check that the last column is "word"
  expect_equal(colnames(resp)[ncol(resp)], "word")

  # check that "word" column contains single words
  checkmate::expect_character(resp$word)
  expect_equal(
    # every entry has two, and only two, word boundaries \b
    all(stringr::str_count(resp$word, "\\b") == 2), TRUE
  )
})
