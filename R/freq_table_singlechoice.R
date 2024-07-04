#' Calculate frequency table for questions responses with a single option selected
#'
#' @param data tibble, dataframe of data
#' @param singlechoice_column Column identifying single choice question responses
#' @param expand TRUE/FALSE. Whether to expand end table to fill for all combinations
#' @param singlechoice_choices Optional. If expand = TRUE, vector of options singlechoice responses can take
#'
#'
#' @return A frequency table with percentages of singlechoice question
#' @export
#'
#' @importFrom rlang :=
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
singlechoice_freq_table <- function(data,
                                    singlechoice_column,
                                    round_dec = 1,
                                    expand = FALSE,
                                    singlechoice_choices

) {

  data[data == ""] <- NA_character_
  data[data == "NA"] <- NA_character_

  filtered_data <- data |>
    dplyr::filter(!is.na({{ singlechoice_column }}))

  # get the base frequency table - this won't include missing values
  df <- data |>
    dplyr::group_by({{ singlechoice_column }} ) |>
    dplyr::summarise(Freq = dplyr::n())

  if(expand == TRUE) {

  #create a new df with expand.grid to get all combinations of var1 and var2
  newdf <- tidyr::expand_grid({{ singlechoice_column }} := singlechoice_choices)

  #join original and newdf to have the frequency counts from original df
  df <- dplyr::left_join(newdf,
                            df
  )

  #replace all NA with 0 for rows which were not in original df
  df <- df |>
    dplyr::mutate(Freq = tidyr::replace_na(Freq, 0))

  }

  percent_table <- df |>
    dplyr::ungroup() |>
    dplyr::mutate(round( Percent = 100*  Freq / sum(Freq),
                         round_dec)) |>
    dplyr::select({{ singlechoice_column }},
                  Freq,
                  Percent)


  return(percent_table)
}
