coprescribing_extract <-  function(con,
                                   schema = "GRPLA",
                                   table = "DFM_FACT_CATEGORY_202308") {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table)) |>
    dplyr::filter(PATIENT_IDENTIFIED == "Y") |>
    dplyr::group_by(IDENTIFIED_PATIENT_ID, YEAR_MONTH) |>
    dplyr::summarise(cat_count = n_distinct(CATEGORY), .groups = "drop")
  
  fact_coprescribing <- fact |>
    dplyr::group_by(`Year Month` = YEAR_MONTH,
                    `Number of Categories` = cat_count) |>
    dplyr::summarise(
      `Total Identified Patients` = count(IDENTIFIED_PATIENT_ID),
      .groups = "drop"
    ) |>
    dplyr::arrange(`Year Month`,
                   `Number of Categories`) |>
    collect()
  return(fact_coprescribing)
}