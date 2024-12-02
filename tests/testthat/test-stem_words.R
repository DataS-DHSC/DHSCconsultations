test_that("stemming works", {
  x <- c("win", "winning", "winner", NA)

  # test with no exceptions

  expect_equal(stem_words(x), c("win", "win", "winner", NA))

  # test with single exception
  expect_equal(
    stem_words(x, exceptions = "winning"),
    c("win", "winning", "winner", NA)
  )
})
