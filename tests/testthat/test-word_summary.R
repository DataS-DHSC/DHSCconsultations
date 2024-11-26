test_that("summarise_words works", {
  responses <- DHSCconsultations::dummy_response

  responses_free_text <- responses %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "Please share your views on policy 1",
          "Please share your views on policy 2"
        )
      )
    )

  result <- summarise_words(responses_free_text,
    identity, # no cleaning
    NULL, # no glossary
    stop_words = tidytext::stop_words$word,
    min_char = 2
  )

  # check the result is a list of two dataframes
  # first: two things with the right names
  expect_equal(
    names(result),
    c(
      "Please share your views on policy 1",
      "Please share your views on policy 2"
    )
  )
  # second: is list
  checkmate::expect_list(result)
  # third: is data frames
  checkmate::expect_data_frame(result[[1]])
  checkmate::expect_data_frame(result[[2]])
})
