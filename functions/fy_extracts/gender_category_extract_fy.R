gender_category_extract_fy <- function(con,
                                    schema,
                                    table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                            TRUE ~ 0)) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::mutate(
      PAT_GENDER = case_when(
        PAT_GENDER == "Female" ~ "Female",
        PAT_GENDER == "Male" ~ "Male",
        TRUE ~ "Unknown"
      )
    ) |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      CATEGORY,
      PAT_GENDER,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  fact_gender <- fact |>
    dplyr::group_by(
      `Financial Year` = FINANCIAL_YEAR,
      `Drug Category` = stringr::str_to_title(CATEGORY),
      `Patient Gender` = PAT_GENDER,
      `Identified Patient Flag` = PATIENT_IDENTIFIED
    ) |>
    dplyr::summarise(
      `Total Identified Patients` = sum(PATIENT_COUNT, na.rm = T),
      `Total Items` = sum(ITEM_COUNT, na.rm = T),
      `Total Net Ingredient Cost (GBP)` = sum(ITEM_PAY_DR_NIC, na.rm = T) /
        100,
      .groups = "drop"
    ) |>
    dplyr::arrange(`Financial Year`,
                   `Drug Category`,
                   `Patient Gender`,
                   desc(`Identified Patient Flag`)) |>
    
    collect()
  
  
  return(fact_gender)
}
