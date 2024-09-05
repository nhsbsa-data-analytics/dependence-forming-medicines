capture_rate_extract_fy <- function(con,
                                 schema,
                                 table) {
  fact <- dplyr::tbl(src = con,
                     dbplyr::in_schema(schema, table))  |>
    dplyr::group_by(
      `Financial Year` = FINANCIAL_YEAR,
      `Drug Category` = CATEGORY,
      PATIENT_IDENTIFIED
    ) |>
    dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                     .groups = "drop") |>
    dplyr::arrange(`Financial Year`) |>
    collect() |>
    tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                       values_from = ITEM_COUNT) |>
    mutate(
      `Identified Patient Rate` = Y / (Y + N) * 100
    ) |>
    dplyr::select(-Y,-N) |>
    dplyr::arrange(`Financial Year`, `Drug Category`)
  return(fact)
}