coprescribing_matrix_extract <- function(con,
                                         schema,
                                         table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::filter(PATIENT_IDENTIFIED == "Y") |>
    dplyr::mutate(OPIOIDS =  case_when (CATEGORY == "OPIOIDS"  ~ 1,
                                        TRUE ~ 0)) |>
    dplyr::mutate(BENZODIAZEPINES =  case_when (CATEGORY == "BENZODIAZEPINES"  ~ 1,
                                                TRUE ~ 0)) |>
    dplyr::mutate(GABAPENTINOIDS =  case_when (CATEGORY == "GABAPENTINOIDS"  ~ 1,
                                               TRUE ~ 0)) |>
    
    dplyr::mutate(ZDRUGS =  case_when (CATEGORY == "Z-DRUGS"  ~ 1,
                                       TRUE ~ 0)) |>
    dplyr::mutate(ANTIDEPRESSANTS =  case_when (CATEGORY == "ANTIDEPRESSANTS"  ~ 1,
                                                TRUE ~ 0)) |>
    
    dplyr::group_by(IDENTIFIED_PATIENT_ID,
                    YEAR_MONTH) |>
    dplyr::summarise(
      CAT_COUNT = n_distinct(CATEGORY),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      OPIOIDS = sum(OPIOIDS, na.rm = T),
      BENZODIAZEPINES = sum(BENZODIAZEPINES, na.rm = T),
      GABAPENTINOIDS = sum(GABAPENTINOIDS, na.rm = T),
      ZDRUGS = sum(ZDRUGS, na.rm = T),
      ANTIDEPRESSANTS = sum(ANTIDEPRESSANTS, na.rm = T),
      .groups = "drop"
    )
  
  fact_coprescribing_m <- fact |>
    dplyr::filter(CAT_COUNT == 2) |>
    dplyr::mutate(
      COMBINATION =  case_when(
        OPIOIDS > 0 & BENZODIAZEPINES > 0 ~ "Opioids and Benzodiazepines",
        OPIOIDS > 0 &
          GABAPENTINOIDS > 0 ~ "Opioids and Gabapentinoids",
        OPIOIDS > 0 & ZDRUGS > 0 ~ "Opioids and Z-Drugs",
        OPIOIDS > 0 &
          ANTIDEPRESSANTS > 0 ~ "Opioids and Antidepressants",
        BENZODIAZEPINES > 0 &
          GABAPENTINOIDS > 0 ~ "Benzodiazepines and Gabapentinoids",
        BENZODIAZEPINES > 0 &
          ZDRUGS > 0 ~ "Benzodiazepines and Z-Drugs",
        BENZODIAZEPINES > 0 &
          ANTIDEPRESSANTS > 0 ~ "Benzodiazepines and Antidepressants",
        ZDRUGS > 0 &
          GABAPENTINOIDS > 0 ~ "Z-Drugs and Gabapentinoids",
        ZDRUGS > 0 &
          ANTIDEPRESSANTS > 0 ~ "Z-Drugs and Antidepressants",
        GABAPENTINOIDS > 0 &
          ANTIDEPRESSANTS > 0 ~ "Gabapentinoids and Antidepressants"
      )
    ) |>
    dplyr::group_by(`Drug Combination` = COMBINATION,
                    `Year Month` = YEAR_MONTH) |>
    dplyr::summarise(
      `Total Identified Patients` = count(IDENTIFIED_PATIENT_ID),
      .groups = "drop"
    ) |>
    dplyr::arrange(`Year Month`,
                   `Drug Combination`) |>
    select(`Year Month`, `Drug Combination`, `Total Identified Patients`) |>
    collect()
  
  return(fact_coprescribing_m)
}