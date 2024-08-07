
DFM_child_adult_extract_quarterly <- function(con,
                                       schema,
                                       table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::mutate(PATIENT_COUNT = if_else(PATIENT_IDENTIFIED == "Y", 1, 0)) |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      SECTION_DESCR,
      BNF_SECTION,
      CALC_AGE,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
    ) 
  
  child_adult_stp <- fact |>
    mutate(
      AGE_BAND = case_when(
        CALC_AGE < 0 ~ "Unknown",
        CALC_AGE <= 17 ~ "17 and under",
        TRUE ~ "18 and over"
      )
    ) |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      SECTION_DESCR,
      BNF_SECTION,
      AGE_BAND
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T)
    ) |>
    ungroup() |>
    dplyr::arrange(
      FINANCIAL_YEAR,
      BNF_SECTION,
      AGE_BAND
    ) |>
    collect ()
  
  return(child_adult_stp)
}