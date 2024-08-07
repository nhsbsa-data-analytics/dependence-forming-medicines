
chem_sub_extract_quarterly <- function(con,
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
      PARAGRAPH_DESCR,
      BNF_PARAGRAPH,
      CHEMICAL_SUBSTANCE_BNF_DESCR,
      BNF_CHEMICAL_SUBSTANCE,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
    ) 
  
  fact_chem_sub <- fact |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      SECTION_DESCR,
      BNF_SECTION,
      PARAGRAPH_DESCR,
      BNF_PARAGRAPH,
      CHEMICAL_SUBSTANCE_BNF_DESCR,
      BNF_CHEMICAL_SUBSTANCE,
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
      BNF_PARAGRAPH,
      BNF_CHEMICAL_SUBSTANCE,
      desc(PATIENT_IDENTIFIED)
    ) |>
    collect
  
  return(fact_chem_sub)
}