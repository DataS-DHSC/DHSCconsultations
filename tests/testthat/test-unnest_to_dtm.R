test_that("unnest_to_dtm works", {
  # simple cleaning function for testing purposes
  clean_words <- function(x) {
    x |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("[^[:alnum:]]", " ") |>
      stringr::str_squish()
  }

  # dummy response data
  d1 <- unnest_words(
    dummy_response,
    "Please share your views on policy 1",
    clean_words, "", tidytext::stop_words$word, ""
  ) %>%
    unnest_to_dtm("response_id", "word")

  # is the right class
  expect_s3_class(d1, "DocumentTermMatrix")

  # is the right dimensions
  expect_equal(dim(d1), c(100, 3707))
})
