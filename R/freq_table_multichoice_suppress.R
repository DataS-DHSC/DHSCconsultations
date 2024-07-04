#' Suppress a multichoice frequency table
#'
#' @param freq_table data.frame, tibble of a multichoice frequency
#' @param freq_column Column identifying frequency values
#' @param percent_col Column identifying percentage values
#' @param min_n Value to suppress less than (min = 5 will suppress 4 or less)
#'
#' @return A suppressed frequency table for multichoice question
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
#' freq_table <- multichoice_freq_table(data = consultations::dummy_response,
#'                        response_column = response_id,
#'                        demo_columns = `What is your age?`,
#'                        multichoice_column = `Which themes would you like to share your responses about?`,
#'                        demo_columns = "What is your age?",
#'                        multichoice_column = "Which themes would you like to share your responses about?",
#'                        multichoice_options = c("TopicA", "Topic B", "TopicC", "Topic D", "Topic E", "Topic F"),
#'                        round_dec = 1,
#'                        expand = FALSE)
#'
#' suppressed <- multichoice_suppress(freq_table = freq_table,
#'                                    freq_column = Freq,
#'                                    percent_col = Percent,
#'                                    min_n = 5)
#'
multichoice_suppress <- function(freq_table,
                                 freq_column,
                                 percent_col,
                                 min_n){

  # Suppression rules
  ## Suppress any freq below min_n
  ## If percent column exists-
  ### Suppress matching percent row
  ### Check if percent is 100% then suppress matching Freq row

  suppressed_table <- freq_table |>
    dplyr::mutate({{ freq_column }} := dplyr::case_when({{ freq_column }} < min_n ~ -99,
                                          TRUE ~ {{ freq_column }}))

  if(!missing(percent_col)){
    suppressed_table <- suppressed_table |>
      dplyr::mutate({{ percent_col }} := dplyr::case_when({{ freq_column }} == -99 ~ -99,
                                                     TRUE ~ {{ percent_col }}))

    suppressed_table <- suppressed_table |>
      dplyr::mutate({{ percent_col }} := dplyr::case_when({{ percent_col }} == 1 ~ -99,
                                                     TRUE ~ {{ percent_col }})) |>
      dplyr::mutate({{ freq_column }} := dplyr::case_when({{ percent_col }} == -99 ~ -99,
                                            TRUE ~ {{ freq_column }}))

  }

  return(suppressed_table)

}
