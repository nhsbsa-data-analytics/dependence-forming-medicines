chem_sub_extract_cy <- function(con,
                                       schema,
                                       table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::mutate(PATIENT_COUNT = if_else(PATIENT_IDENTIFIED == "Y", 1, 0)) |>
    dplyr::group_by(
      CALENDAR_YEAR,
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
      `Calendar Year` = CALENDAR_YEAR,
      `BNF Section Name` = SECTION_DESCR,
      `BNF Section Code` = BNF_SECTION,
      `BNF Paragraph Name` = PARAGRAPH_DESCR,
      `BNF Paragraph Code` = BNF_PARAGRAPH,
      `BNF Chemical Substance Name` = CHEMICAL_SUBSTANCE_BNF_DESCR,
      `BNF Chemical Substance Code` = BNF_CHEMICAL_SUBSTANCE,
      `Identified Patient Flag` = PATIENT_IDENTIFIED
    ) |>
    dplyr::summarise(
      `Total Identified Patients` = sum(PATIENT_COUNT, na.rm = T),
      `Total Items` = sum(ITEM_COUNT, na.rm = T),
      `Total Net Ingredient Cost (GBP)` = sum(ITEM_PAY_DR_NIC, na.rm = T) /
        100,
      .groups = "drop"
    ) |>
    dplyr::arrange(
      `Calendar Year`,
      `BNF Section Code`,
      `BNF Paragraph Code`,
      `BNF Chemical Substance Code`,
      desc(`Identified Patient Flag`)
    ) |>
    collect()
  
  return(fact_chem_sub)
}
