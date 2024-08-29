category_extract_monthly <- function(con,
                             schema,
                             table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                            TRUE ~ 0)) |>
    dplyr::group_by(
      YEAR_MONTH,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      SECTION_DESCR,
      BNF_SECTION,
      CATEGORY,
      PATIENT_COUNT
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  fact_category <- fact |>
    dplyr::group_by(
      `Year Month` = YEAR_MONTH,
      `BNF Section Name` = SECTION_DESCR,
      `BNF Section Code` = BNF_SECTION,
      `Drug Category` = stringr::str_to_title(CATEGORY),
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
      `Year Month`,
      `BNF Section Code`,
      `Drug Category`,
      desc(`Identified Patient Flag`)
    ) |>
    collect()
  
  #remove antidepressants from final table using not in function
  fact_category <- fact_category |>
    dplyr::filter(`Drug Category` != "Antidepressants")
  
  return(fact_category)
}