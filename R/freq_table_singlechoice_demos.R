#' Calculate frequency table for questions responses with a single option selected by demographics
#'
#' @param data tibble, dataframe of data
#' @param singlechoice_column Column identifying single choice question responses
#' @param singlechoice_choices Optional. If expand = TRUE, vector of options singlechoice responses can take
#' @param demo_column Column identifying a demographic columns to break the question down by
#' @param demo_choices Optional. If expand = TRUE, vector of options demographic responses can take
#' @param expand TRUE/FALSE. Whether to expand end table to fill for all combinations
#'
#' @return
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
#'freq_table <- singlechoice_demos_freq_table(consultations::dummy_response,
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
#'                                              "unchosen_response_age")) |>
#'  dplyr::arrange(`What is your ethnicity?`,
#'                 `What is your age?`)
#'
singlechoice_demos_freq_table  <- function(data,
                                           singlechoice_column,
                                           singlechoice_choices,
                                           demo_column,
                                           demo_choices,
                                           round_dec = 1,
                                           expand = FALSE

) {

  data[data == ""] <- NA_character_
  data[data == "NA"] <- NA_character_

  filtered_data <- data |>
    dplyr::filter(!is.na({{ demo_column }})) |>
    dplyr::filter(!is.na({{ singlechoice_column }}))

  # get the base frequency table - this won't include missing values
  df <- data |>
    dplyr::group_by({{ demo_column }}, {{ singlechoice_column }} ) |>
    dplyr::summarise(Freq = dplyr::n())

  if(expand == TRUE) {

  #create a new df with expand.grid to get all combinations of var1 and var2
    df <- tidyr::expand_grid({{ demo_column }} := demo_choices,
                       {{ singlechoice_column }} := singlechoice_choices)

  #join original and newdf to have the frequency counts from original df
    df <- left_join(newdf,
                    df
  )

  #replace all NA with 0 for rows which were not in original df

    df <- df |>
    mutate(Freq = tidyr::replace_na(Freq, 0))

  }

  percent_table <- df |>
    dplyr::ungroup() |>
    dplyr::group_by({{ singlechoice_column }}) |>
    dplyr::mutate(Percent = 100 * Freq / sum(Freq)) |>
    dplyr::select({{ singlechoice_column }},
           {{ demo_column }},
           Freq,
           Percent)


  return(percent_table)
}

freq_table <- singlechoice_demos_freq_table(consultations::dummy_response,
                         `What is your ethnicity?`,
                         c("Asian/Asian British",
                           "Other ethnic group",
                           "Mixed/Multiple ethnicities",
                           "Black/African/Caribbean/Black British",
                           "White",
                           "unchosen_response_ethnicity"),
                            `What is your age?`,
                            c("60-69 years old",
                              "25-29 years old",
                              "70-79 years old",
                              "Above 80 years old",
                              "16-17 years old",
                              "18-19 years old",
                              "30-39 years old",
                              "20-24 years old",
                              "40-49 years old",
                              "50-59 years old",
                              "unchosen_response_age")
) |>
  dplyr::arrange(`What is your ethnicity?`,
          `What is your age?`)
