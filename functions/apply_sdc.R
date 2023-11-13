apply_sdc <-
  function(data,
           level = 5,
           rounding = TRUE,
           round_val = 5,
           mask = "") {
    rnd <- round_val
    
    if (is.character(mask)) {
      type <- function(x)
        as.character(x)
    } else {
      type <- function(x)
        x
    }
    
    data |> dplyr::mutate(dplyr::across(
      where(is.numeric),
      .fns = ~ dplyr::case_when(
        .x >= level &
          rounding == T ~ as.numeric(type(rnd * round(.x / rnd))),
        .x < level & .x > 0 & rounding == T ~ as.numeric(mask),
        .x < level & .x > 0 & rounding == F ~ as.numeric(mask),
        TRUE ~ as.numeric(type(.x))
      ),
      .names = "{.col}"
    ))
  }