test_that("test fit_lda", {
  data = dplyr::slice_head(dummy_response, n=5)
  topic_number = list("Please share your views on policy 1" = 3)

  glossary_words = c("lady catherine de bourgh",
                     "mr frank churchill",
                     "mr john knightley" )

  # run the function and snapshot it
  expect_snapshot_value({
    fit_data = fit_lda(data,"response_id",
            topic_number,
            20241202,
            clean_text = identity,
            glossary_words,
            stop_words = tidytext::stop_words$word,
            stem_word_exceptions = NULL
    )
    #strip the lda_Gibbs object

    fit_data$`Please share your views on policy 1`[[1]][2:5]}, style = "json2",
    tolerance = 1e-5)
})
