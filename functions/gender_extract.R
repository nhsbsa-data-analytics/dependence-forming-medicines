gender_extract <- function(con,
                           schema,
                           table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                            TRUE ~ 0)) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      PDS_GENDER,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  fact_gender <- fact |>
    dplyr::mutate(PDS_GENDER = case_when(
      PDS_GENDER == 1 ~ "Male",
      PDS_GENDER == 2 ~ "Female",
      TRUE ~ "Unknown"
    )) |>
    dplyr::group_by(
      `Financial Year` = FINANCIAL_YEAR,
      `Patient Gender` = PDS_GENDER,
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
                   `Patient Gender`,
                   desc(`Identified Patient Flag`)) |>
    collect()
  return(fact_gender)
  
}