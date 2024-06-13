

devtools::load_all()

?stem_words

df <- data.frame(id = c(1, 2, 3, 4), word = c("win", "winning", "winner", NA))
stem_words(df, word)
