#' Calculate frequency table for questions responses with multiple options selected
#'
#' @param data tibble, data.frame of response level data
#' @param response_column Column identifying individual responses
#' @param demo_columns Optional. Set of columns to breakdown multichoice column by
#' @param multichoice_column Column identifying multichoice question
#' @param round_dec Number of decimal places to round percentages, default = 1
#' @param expand TRUE/FALSE. Whether to expand end table to fill for all combinations
#' @param multichoice_options Optional. If expand = TRUE, vector of options multichoice responses can take
#'
#' @return A frequency table with percentages of multichoice question by demographics
#' @export
#'
#' @examples
#' multichoice_freq_table(data = consultations::dummy_response,
#'                        response_column = response_id,
#'                        demo_columns = `What is your age?`,
#'                        multichoice_column = `Which themes would you like to share your responses about?`,
#'                        demo_columns = "What is your age?",
#'                        multichoice_column = "Which themes would you like to share your responses about?",
#'                        multichoice_options = c("TopicA", "Topic B", "TopicC", "Topic D", "Topic E", "Topic F"),
#'                        round_dec = 1,
#'                        expand = FALSE)
#'
multichoice_freq_table <- function(data,
                                   response_column,
                                   demo_columns,
                                   multichoice_column,
                                   round_dec = 1,
                                   expand = FALSE,
                                   multichoice_options
                                   ){
mtcars
  data[data == ""] <- NA_character_
  data[data == "NA"] <- NA_character_

  filtered_data <- data |>
    dplyr::filter(!is.na({{ multichoice_column }}))

  unique_respondents <- filtered_data |>
    dplyr::select({{response_column}}) |>
    dplyr::summarise(Total = dplyr::n()) |>
    dplyr::pull(Total)


  seperated_data <- data |>
    tidyr::separate_rows({{ multichoice_column }},
                         sep=",(?!\\s)")

  if(!missing(demo_columns)){
    freq_table <- seperated_data |>
      dplyr::filter(nchar({{ multichoice_column }}) > 0) |>
      dplyr::group_by({{ demo_columns }}, {{ multichoice_column }}) |>
      dplyr::summarise(Freq = dplyr::n()) |>
      dplyr::mutate(Percent = round(100* Freq / unique_respondents,
                                    round_dec))
  } else {
    freq_table <- seperated_data |>
      dplyr::filter(nchar({{ multichoice_column }}) > 0) |>
      dplyr::group_by({{ multichoice_column }}) |>
      dplyr::summarise(Freq = dplyr::n()) %>%
      dplyr::mutate(Percent = round(100* Freq / unique_respondents,
                                    round_dec))
  }

  # Optional expansion to include all options

  if(expand == TRUE) {

    found_options <- freq_table |>
      dplyr::pull({{ multichoice_column }}) |>
      dplyr::unique()
    missing_options <- dplyr::setdiff(multichoice_options, found_options)
    options_table <- data.frame(missing_options)
    colnames(options_table) <- rlang::as_string(rlang::ensym(multichoice_column))

    all_options_table <- dplyr::bind_rows(freq_table, options_table)


    if(!missing(demo_columns)){
      freq_table <-  all_options_table  |>
        as.data.frame() |>
        dplyr::complete({{ demo_columns }}, {{ multichoice_column }},
                        fill = list(Freq = NA,
                                    Percent = NA)) |>
        dplyr::arrange({{ demo_columns }}, {{ multichoice_column }})
    } else {
      freq_table <-  all_options_table  |>
        as.data.frame() |>
        dplyr::complete({{ multichoice_column }},
                        fill = list(Freq = NA,
                                    Percent = NA)) |>
        dplyr::arrange({{ multichoice_column }})
    }
  }

  TotalRow <- data.frame(col = "Total",
                         Freq = unique_respondents)

  colnames(TotalRow) <- c(rlang::as_string(rlang::ensym(multichoice_column)),
                          "Freq")

  output <- dplyr::bind_rows(freq_table, TotalRow)

  return(output)

}
