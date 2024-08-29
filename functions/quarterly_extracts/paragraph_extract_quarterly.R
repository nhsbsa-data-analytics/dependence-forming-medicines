paragraph_extract_quarterly <- function(con,
                                 schema,
                                 table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::mutate(PATIENT_COUNT = if_else(PATIENT_IDENTIFIED == "Y", 1, 0)) |>
    
    
    dplyr::group_by(
      FINANCIAL_QUARTER,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      SECTION_DESCR,
      BNF_SECTION,
      PARAGRAPH_DESCR,
      BNF_PARAGRAPH,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  fact_paragraph <- fact |>
    dplyr::group_by(
      `Financial Quarter` = FINANCIAL_QUARTER,
      `BNF Section Name` = SECTION_DESCR,
      `BNF Section Code` = BNF_SECTION,
      `BNF Paragraph Name` = PARAGRAPH_DESCR,
      `BNF Paragraph Code` = BNF_PARAGRAPH,
      `Identified Patient Flag` = PATIENT_IDENTIFIED
    ) |>
    dplyr::summarise(
      `Total Identified Patients`  = sum(PATIENT_COUNT, na.rm = T),
      `Total Items` = sum(ITEM_COUNT, na.rm = T),
      `Total Net Ingredient Cost (GBP)` = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
      .groups = "drop"
    ) |>
    dplyr::arrange(
      `Financial Quarter`,
      `BNF Section Code`,
      `BNF Paragraph Code`,
      desc(`Identified Patient Flag`)
    ) |>
    collect()
  
  return(fact_paragraph)
  
}
