icb_category_extract <- function(con,
                                 schema = "GRPLA",
                                 table = "DFM_FACT_CATEGORY_202308") {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::filter(CATEGORY != "ANTIDEPRESSANTS") |>
    dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                            TRUE ~ 0)) |>
    dplyr::group_by(
      FINANCIAL_YEAR,
      IDENTIFIED_PATIENT_ID,
      PATIENT_IDENTIFIED,
      PATIENT_COUNT,
      CATEGORY,
      ICB_NAME,
      ICB_CODE
    ) |>
    dplyr::summarise(
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  fact_icb_cat <- fact |>
    dplyr::group_by(
      `Financial Year` = FINANCIAL_YEAR,
      `Integrated Care Board Name` = ICB_NAME,
      `Integrated Care Board Code` = ICB_CODE,
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
      `Financial Year`,
      `Integrated Care Board Name`,
      `Drug Category`,
      desc(`Identified Patient Flag`)
    ) |>
    collect()
  
  return(fact_icb_cat)
  
}