DFM_capture_rate_extract_quarterly <- function(con,
                                      schema,
                                      table) {
    
    fact <- dplyr::tbl(src = con,
                       dbplyr::in_schema(schema, table))  |>
      dplyr::group_by(
        `Financial Quarter` = FINANCIAL_QUARTER,
        `Drug Category` = CATEGORY,
        PATIENT_IDENTIFIED
      ) |>
      dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                       .groups = "drop") |>
      dplyr::arrange(`Financial Quarter`) |>
      collect() |>
      tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                         values_from = ITEM_COUNT) |>
      mutate(
        `Identified Patient Rate` = Y / (Y + N) * 100
      ) |>
      dplyr::select(-Y,-N) |>
      dplyr::arrange(`Financial Quarter`, `Drug Category`)
    return(fact)
}
  
