age_extract_quarterly <- function(con,
                        schema,
                        table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                            TRUE ~ 0)) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::inner_join(dplyr::tbl(con,
                                 from = dbplyr::in_schema("DIM", "AGE_DIM")),
                      by = c("CALC_AGE" = "AGE")) |>
    dplyr::group_by(
      FINANCIAL_QUARTER,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      DALL_5YR_BAND,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  fact_age <- fact |>
    dplyr::mutate(AGE_BAND = dplyr::case_when(is.na(DALL_5YR_BAND) ~ "Unknown",
                                              TRUE ~ DALL_5YR_BAND)) |>
    dplyr::group_by(
      `Financial Quarter` = FINANCIAL_QUARTER,
      `Age Band` = AGE_BAND,
      `Identified Patient Flag` = PATIENT_IDENTIFIED
    ) |>
    dplyr::summarise(
      `Total Identified Patients` = sum(PATIENT_COUNT, na.rm = T),
      `Total Items` = sum(ITEM_COUNT, na.rm = T),
      `Total Net Ingredient Cost (GBP)` = sum(ITEM_PAY_DR_NIC, na.rm = T) /
        100,
      .groups = "drop"
    ) |>
    dplyr::arrange(`Financial Quarter`,
                   `Age Band`,
                   desc(`Identified Patient Flag`)) |>
    collect()
  
  return(fact_age)
}
