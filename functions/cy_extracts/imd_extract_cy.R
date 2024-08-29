imd_extract_cy <- function(con,
                        schema,
                        table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::mutate(
      PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                TRUE ~ 0),
      IMD_QUINTILE = case_when(
        IMD_DECILE <= 2 ~ "1 - Most Deprived",
        IMD_DECILE <= 4 ~ "2",
        IMD_DECILE <= 6 ~ "3",
        IMD_DECILE <= 8 ~ "4",
        IMD_DECILE <= 10 ~ "5 - Least Deprived",
        TRUE ~ "Unknown"
      )
    ) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::group_by(
      CALENDAR_YEAR,
      IMD_QUINTILE,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  fact_imd <- fact |>
    dplyr::group_by(`Calendar Year` = CALENDAR_YEAR,
                    `IMD Quintile` = IMD_QUINTILE) |>
    dplyr::summarise(
      `Total Identified Patients` = sum(PATIENT_COUNT, na.rm = T),
      `Total Items` = sum(ITEM_COUNT, na.rm = T),
      `Total Net Ingredient Cost (GBP)` = sum(ITEM_PAY_DR_NIC, na.rm = T) /
        100,
      .groups = "drop"
    ) |>
    dplyr::arrange(`Calendar Year`,
                   `IMD Quintile`) |>
    collect()
  
  
  return(fact_imd)
}