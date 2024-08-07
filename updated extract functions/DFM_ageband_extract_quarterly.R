ageband_extract_quarterly <- function(con,
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
  
  fact_age <- fact |>
    dplyr::inner_join(
      dplyr::tbl(con,
                 from = dbplyr::in_schema("DIM", "AGE_DIM")),
      by = c(
        "CALC_AGE" = "AGE"
      )
    ) |>
    dplyr::mutate(
      AGE_BAND = dplyr::case_when(
        is.na(DALL_5YR_BAND) ~ "Unknown",
        TRUE ~ DALL_5YR_BAND
      )
    ) |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      SECTION_DESCR,
      BNF_SECTION,
      AGE_BAND,
      PATIENT_IDENTIFIED
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T)
    ) |>
    dplyr::arrange(
      FINANCIAL_YEAR,
      BNF_SECTION,
      AGE_BAND,
      desc(PATIENT_IDENTIFIED)
    ) |>
    collect()
  
  return(fact_age)
}


national_data2 <-
  ageband_extract_quarterly(con = con,
                            schema = "OST",
                            table = "DFM_FACT_CATEGORY_202407")
