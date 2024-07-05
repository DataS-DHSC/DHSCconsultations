#'  Suppress a singlechoice frequency table 
#'
#' @param freq_table data.frame, tibble of a multichoice frequency
#' @param freq_column Column identifying frequency values
#' @param percent_column Column identifying percentage values
#' @param min_n Value to suppress less than (min = 5 will suppress 4 or less)
#'
#' @return A suppressed frequency table for singlechoice question
#' @export
#'
#' @examples
#' singlechoice_freq_table(consultations::dummy_response,
#'             `What is your age?`,
#'             c("60-69 years old",
#'               "25-29 years old",
#'               "70-79 years old",
#'               "Above 80 years old",
#'               "16-17 years old",
#'               "18-19 years old",
#'               "30-39 years old",
#'               "20-24 years old",
#'               "40-49 years old",
#'               "50-59 years old",
#'               "unchosen_response_age")
#' ) |>
#'   dplyr::arrange(`What is your age?`)
#'
#' singlechoice_suppress(out,
#'                       Freq,
#'                       Percent,
#'                       5)
#'
singlechoice_suppress <- function(freq_table,
                                  freq_column,
                                  percent_column,
                                  min_n = 5) {

  # What should be suppressed
  # Responses with a freq less than min_n
  # When groups sum to 100% - at least two should be suppressed if one is required
  # The smallest value will continue to be suppressed until at least a total of min_n is suppressed
  # Any frequency of 100%

  masked_freq_table <- freq_table |>
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
