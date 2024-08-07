DFM_imd_extract_quarterly <- function(con,
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
      IMD_DECILE,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
    ) 
  
  fact_imd <- fact |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      SECTION_DESCR,
      BNF_SECTION,
      IMD_DECILE
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)/100,
      PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T)
    ) |>
    dplyr::arrange(
      FINANCIAL_YEAR,
      BNF_SECTION,
      IMD_DECILE
    ) |>
    collect()
  
  return(fact_imd)
  
}