

get_cols_free_text <- function(data, col_id, cols_free_text) {
  if (!is.null(cols_free_text)) {
    out <- cols_free_text
  } else if (!is.null(col_id)) {
    out <- setdiff(names(data), col_id)
  } else {
    out <- names(data)
  }

  return(out)
}
