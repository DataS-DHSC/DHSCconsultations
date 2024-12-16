#' @keywords internal
write_plot_png <- function(plt, file_path) {
  return(
    .write_plot(
      plt,
      grDevices::png,
      sprintf("%s.png", file_path)
    )
  )
}

#' @keywords internal
write_plot_svg <- function(plt, file_path) {
  return(
    .write_plot(
      plt,
      grDevices::svg,
      sprintf("%s.svg", file_path)
    )
  )
}

#' @keywords internal
.write_plot <- function(plt, fn, file_path) {
  fn(filename = file_path)
  print(plt)
  grDevices::dev.off()

  return(plt)
}
