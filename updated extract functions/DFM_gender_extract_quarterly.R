DFM_gender_extract_quarterly <- function(con,
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
      PAT_GENDER,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
    ) 
  
  fact_gender <- fact |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      SECTION_DESCR,
      BNF_SECTION,
      PAT_GENDER,
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
      PAT_GENDER,
      desc(PATIENT_IDENTIFIED)
    ) |>
    collect()
  
  return(fact_gender)
}