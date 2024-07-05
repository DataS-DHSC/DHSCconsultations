#'  Suppress a singlechoice by demographic frequency table
#'
#' @param freq_table data.frame, tibble of a singlechoice frequency
#' @param demo_column
#' @param singlechoice_column
#' @param freq_column Column identifying frequency values
#' @param percent_column Column identifying percentage values
#' @param min_n Value to suppress less than (min = 5 will suppress 4 or less)
#'
#' @return A suppressed frequency table for singlechoice question by a demographic
#' @export
#'
#' @examples
#' freq_table <- singlechoice_demos_freq_table(consultations::dummy_response,
#'                                            `What is your ethnicity?`,
#'                                            c("Asian/Asian British",
#'                                              "Other ethnic group",
#'                                              "Mixed/Multiple ethnicities",
#'                                              "Black/African/Caribbean/Black British",
#'                                              "White",
#'                                              "unchosen_response_ethnicity"),
#'                                            `What is your age?`,
#'                                            c("60-69 years old",
#'                                              "25-29 years old",
#'                                              "70-79 years old",
#'                                              "Above 80 years old",
#'                                              "16-17 years old",
#'                                              "18-19 years old",
#'                                              "30-39 years old",
#'                                              "20-24 years old",
#'                                              "40-49 years old",
#'                                              "50-59 years old",
#'                                             "unchosen_response_age")) |>
#'  dplyr::arrange(`What is your ethnicity?`,
#'                 `What is your age?`)
#'
#'
#' singlechoice_demos_suppress(freq_table,
#'                             `What is your ethnicity?`,
#'                             `What is your age?`,
#'                             Freq,
#'                             Percent,
#'                             5)
#'
singlechoice_demos_suppress <- function(freq_table,
                                        demo_column,
                                        singlechoice_column,
                                        freq_column,
                                        percent_column,
                                        min_n = 5) {

  # What should be suppressed
  # Responses with a freq less than min_n
  # When groups sum to 100% - at least two should be suppressed if one is required
  # The smallest value will continue to be suppressed until at least a total of min_n is suppressed
  # Any frequency of 100%

  ### Needs dual suppressing, only does on one column. However, order of suppression matters too.
  ### Doing demo then singlechoice will have different suppresion to singlchoice then demo

  masked_freq_table <- freq_table |>
    dplyr::group_by({{ demo_column }}) |>
    dplyr::arrange({{ freq_column }}) |>
    dplyr::mutate(ranking = rank({{ freq_column }}, ties.method = "min")) |>
    dplyr::mutate(masked_freq = dplyr::case_when(cumsum({{ freq_column }}) < 1 ~ -99,
                                                 TRUE ~ {{ freq_column }})) |>
    dplyr::mutate(masked_count = sum(masked_freq == -99)) |>
    dplyr::mutate(masked_freq = dplyr::case_when(masked_count == 1 & ranking == 2 ~ -99,
                                                 TRUE ~ masked_freq))  |>
    dplyr::ungroup() |>
    dplyr::mutate(masked_percent = dplyr::case_when(masked_freq == -99 ~ -99,
                                                    TRUE ~ {{ percent_column }})) |>
    dplyr::select(- {{ freq_column }},
                  - {{ percent_column }},
                  - masked_count,
                  - ranking
    )

  return(masked_freq_table)
}




