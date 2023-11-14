capture_rate_extract_dt <- function(con,
                                    schema,
                                    table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table))|>
    dplyr::group_by(
      FINANCIAL_YEAR,
      `Drug Category` = CATEGORY,
      PATIENT_IDENTIFIED
    ) |>
    dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                     .groups = "drop") |>
    dplyr::arrange(FINANCIAL_YEAR) |>
    collect() |>
    tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                       values_from = ITEM_COUNT) |>
    mutate(`Identified Patient Rate` = Y / (Y + N) * 100) |>
    dplyr::select(-Y,-N) |>
    tidyr::pivot_wider(names_from = FINANCIAL_YEAR,
                       values_from = `Identified Patient Rate`) |>
    dplyr::arrange(`Drug Category`)
  
  return(fact)
}