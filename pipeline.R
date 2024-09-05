### Pipeline to run dfm annual publication
# clear environment
rm(list = ls())

## NOTE: Need to automate this?
max_year_month <- 202403

# source functions
# get all .R files in the functions sub-folder and sub-folders of this
function_files <-
  list.files(
    path = "functions",
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )

# loop over function_files to source them all
for (file in function_files) {
  source(file)
}

# 1. Setup --------------------------------------------
# load GITHUB_KEY if available in environment or enter if not

if (Sys.getenv("GITHUB_PAT") == "") {
  usethis::edit_r_environ()
  stop(
    "You need to set your GITHUB_PAT = YOUR PAT KEY in the .Renviron file which pops up. Please restart your R Studio after this and re-run the pipeline."
  )
}

# load GITHUB_KEY if available in environment or enter if not

if (Sys.getenv("DB_DWCP_USERNAME") == "") {
  usethis::edit_r_environ()
  stop(
    "You need to set your DB_DWCP_USERNAME = YOUR DWCP USERNAME and  DB_DWCP_PASSWORD = YOUR DWCP PASSWORD in the .Renviron file which pops up. Please restart your R Studio after this and re-run the pipeline."
  )
}

#install and library devtools
install.packages("devtools")
library(devtools)

#install nhsbsaUtils package first as need check_and_install_packages()
devtools::install_github(
  "nhsbsa-data-analytics/nhsbsaUtils",
  auth_token = Sys.getenv("GITHUB_PAT"),
  dependencies = TRUE
)

library(nhsbsaUtils)

#install and library packages
req_pkgs <-
  c(
    "dplyr",
    "stringr",
    "data.table",
    "yaml",
    "openxlsx",
    "rmarkdown",
    "logr",
    "highcharter",
    "lubridate",
    "dbplyr",
    "tidyr",
    "janitor",
    "magrittr",
    "tcltk",
    "DT",
    "htmltools",
    "geojsonsf",
    "readxl",
    "kableExtra",
    "svDialogs",
    "nhsbsa-data-analytics/nhsbsaR",
    "nhsbsa-data-analytics/nhsbsaExternalData",
    "nhsbsa-data-analytics/accessibleTables",
    "nhsbsa-data-analytics/nhsbsaDataExtract",
    "nhsbsa-data-analytics/nhsbsaVis"
  )

#library/install packages as required
nhsbsaUtils::check_and_install_packages(req_pkgs)

# load config
config <- yaml::yaml.load_file("config.yml")

# load options
nhsbsaUtils::publication_options()

# 2. connect to DWH  ---------
#build connection to warehouse
con <- nhsbsaR::con_nhsbsa(dsn = "FBS_8192k",
                           driver = "Oracle in OraClient19Home1",
                           "DWCP")

# 3. Extract FY data required ------------------------------------------------
age_category_data_fy <-
  age_category_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

age_data_fy <-
  age_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

age_gender_cat_data_fy <-
  age_gender_cat_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

age_gender_data_fy <-
  age_gender_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

ageband_data_fy <-
  ageband_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

category_data_fy <-
  category_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

gender_category_data_fy <-
  gender_category_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

gender_data_fy <-
  gender_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

icb_category_data_fy <-
  icb_category_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

icb_data_fy <-
  icb_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

imd_category_data_fy <-
  imd_category_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

imd_data_fy <-
  imd_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

national_data_fy <-
  national_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

national_pop <- ons_national_pop(year = c(2015:2023),
                                 area = "ENPOP")

population_category_data_fy <- category_data_fy |>
  dplyr::ungroup() |>
  dplyr::select(
    `Financial Year`,
    `Identified Patient Flag`,
    `Drug Category`,
    `Total Identified Patients`
  ) |>
  dplyr::mutate(`Mid-year Population Year` = as.numeric((substr(
    c(`Financial Year`), 1, 4
  )))) |>
  dplyr::filter(`Identified Patient Flag` == "Y") |>
  dplyr::left_join(select(national_pop, YEAR, ENPOP),
                   by = c("Mid-year Population Year" = "YEAR")) |>
  dplyr::mutate(`Patients per 1,000 Population` = ((`Total Identified Patients` /
                                                      ENPOP) * 1000)) |>
  
  dplyr::select(
    `Financial Year`,
    `Mid-year Population Year`,
    `Drug Category`,
    `Total Identified Patients`,
    `Mid-year Population Estimate` = ENPOP,
    `Patients per 1,000 Population`
  ) |>
  filter(`Financial Year` <= config$fy)

population_data_fy <- national_data_fy |>
  dplyr::select(`Financial Year`,
                `Identified Patient Flag`,
                `Total Identified Patients`) |>
  dplyr::mutate(`Mid-year Population Year` = as.numeric((substr(
    c(`Financial Year`), 1, 4
  )))) |>
  dplyr::filter(`Identified Patient Flag` == "Y") |>
  dplyr::left_join(select(national_pop, YEAR, ENPOP),
                   by = c("Mid-year Population Year" = "YEAR")) |>
  dplyr::mutate(`Patients per 1,000 Population` = ((`Total Identified Patients` /
                                                      ENPOP) * 1000)) |>
  dplyr::select(
    `Financial Year`,
    `Mid-year Population Year`,
    `Total Identified Patients`,
    `Mid-year Population Estimate` = ENPOP,
    `Patients per 1,000 Population`
  ) |>
  filter(`Financial Year` <= config$fy)

patient_identification_fy <-
  capture_rate_extract_fy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  filter(`Financial Year` <= config$fy)


patient_identification_dt <-
  capture_rate_extract_dt(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  )  |>
  select(1, last_col(5):last_col(1))

paragraph_data_fy <- paragraph_extract_fy(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  filter(`Financial Year` <= config$fy)

chem_sub_data_fy <- chem_sub_extract_fy(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  apply_sdc(suppress_column = "Total Items") |>
  filter(`Financial Year` <= config$fy)

# 4. Extract CY data required ------------------------------------------------
age_category_data_cy <-
  age_category_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

age_data_cy <-
  age_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

age_gender_cat_data_cy <-
  age_gender_cat_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

age_gender_data_cy <-
  age_gender_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

ageband_data_cy <-
  ageband_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

category_data_cy <-
  category_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

gender_category_data_cy <-
  gender_category_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

gender_data_cy <-
  gender_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

icb_category_data_cy <-
  icb_category_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

icb_data_cy <-
  icb_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

imd_category_data_cy <-
  imd_category_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

imd_data_cy <-
  imd_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

national_data_cy <-
  national_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

population_category_data_cy <- category_data_cy |>
  dplyr::ungroup() |>
  dplyr::select(
    `Calendar Year`,
    `Identified Patient Flag`,
    `Drug Category`,
    `Total Identified Patients`
  ) |>
  dplyr::mutate(`Mid-year Population Year` = as.numeric((substr(
    c(`Calendar Year`), 1, 4
  )))) |>
  dplyr::filter(`Identified Patient Flag` == "Y") |>
  dplyr::left_join(select(national_pop, YEAR, ENPOP),
                   by = c("Mid-year Population Year" = "YEAR")) |>
  dplyr::mutate(`Patients per 1,000 Population` = ((`Total Identified Patients` /
                                                      ENPOP) * 1000)) |>
  
  dplyr::select(
    `Calendar Year`,
    `Mid-year Population Year`,
    `Drug Category`,
    `Total Identified Patients`,
    `Mid-year Population Estimate` = ENPOP,
    `Patients per 1,000 Population`
  ) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

population_data_cy <- national_data_cy |>
  dplyr::select(`Calendar Year`,
                `Identified Patient Flag`,
                `Total Identified Patients`) |>
  dplyr::mutate(`Mid-year Population Year` = as.numeric((substr(
    c(`Calendar Year`), 1, 4
  )))) |>
  dplyr::filter(`Identified Patient Flag` == "Y") |>
  dplyr::left_join(select(national_pop, YEAR, ENPOP),
                   by = c("Mid-year Population Year" = "YEAR")) |>
  dplyr::mutate(`Patients per 1,000 Population` = ((`Total Identified Patients` /
                                                      ENPOP) * 1000)) |>
  dplyr::select(
    `Calendar Year`,
    `Mid-year Population Year`,
    `Total Identified Patients`,
    `Mid-year Population Estimate` = ENPOP,
    `Patients per 1,000 Population`
  ) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

patient_identification_cy <-
  capture_rate_extract_cy(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

paragraph_data_cy <- paragraph_extract_cy(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

chem_sub_data_cy <- chem_sub_extract_cy(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Calendar Year")) |>
  apply_sdc(suppress_column = "Total Items",
            exclude_columns = c("Calendar Year")) |>
  filter(`Calendar Year` <= config$cy,
         `Calendar Year` >= "2016")

# 5. Extract quarterly data required ------------------------------------------------
age_category_data_quarterly <-
  age_category_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

age_data_quarterly <-
  age_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

age_gender_cat_data_quarterly <-
  age_gender_cat_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

age_gender_data_quarterly <-
  age_gender_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

ageband_data_quarterly <-
  ageband_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

category_data_quarterly <-
  category_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

gender_category_data_quarterly <-
  gender_category_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

gender_data_quarterly <-
  gender_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

icb_category_data_quarterly <-
  icb_category_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

icb_data_quarterly <-
  icb_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

imd_category_data_quarterly <-
  imd_category_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

imd_data_quarterly <-
  imd_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

national_data_quarterly <-
  national_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients")

patient_identification_quarterly <-
  capture_rate_extract_quarterly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  )

paragraph_data_quarterly <- paragraph_extract_quarterly(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients")

chem_sub_data_quarterly <- chem_sub_extract_quarterly(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients") |>
  apply_sdc(suppress_column = "Total Items")

# 6. Extract monthly data required ------------------------------------------------
category_data_monthly <-
  category_extract_monthly(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  ) |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Year Month")) |>
  apply_sdc(suppress_column = "Total Items",
            exclude_columns = c("Year Month"))

chem_sub_data_monthly <- chem_sub_extract_monthly(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Year Month")) |>
  apply_sdc(suppress_column = "Total Items",
            exclude_columns = c("Year Month"))

paragraph_data_monthly <- paragraph_extract_monthly(
  con = con,
  schema = config$sql_schema,
  table = config$sql_table_name
)  |>
  apply_sdc(suppress_column = "Total Identified Patients",
            exclude_columns = c("Year Month")) |>
  apply_sdc(suppress_column = "Total Items",
            exclude_columns = c("Year Month"))

# 7. Extract co-prescribing data required ------------------------------------------------
coprescribing_data <-
  coprescribing_extract(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  )

coprescribing_matrix_data <-
  coprescribing_matrix_extract(
    con = con,
    schema = config$sql_schema,
    table = config$sql_table_name
  )

# 8. Build fy Excel tables ------------------------------------------------------
source(file.path("excel_functions", "costs_items_fy.R"))

# 9. Build cy Excel tables ------------------------------------------------------
source(file.path("excel_functions", "costs_items_cy.R"))

# 10. Build quarterly Excel tables ------------------------------------------------------
source(file.path("excel_functions", "costs_items_quarterly.R"))

# 11. Build co-prescribing Excel tables ------------------------------------------------------
source(file.path("excel_functions", "co_prescribing.R"))

# 12. Create charts/tables and data ----------------------------------------


## Table 1 -----------------------------------------------------------------
table_1_data <- patient_identification_fy |>
  filter(`Drug Category` != "ANTIDEPRESSANTS") |>
  mutate(`Drug Category` = str_to_title(`Drug Category`)) |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())


table_1 <- patient_identification_dt |>
  filter(`Drug Category` != "ANTIDEPRESSANTS") |>
  mutate(`Drug Category` = str_to_title(`Drug Category`)) |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  mutate(across(where(is.numeric), round, 2)) |>
  mutate(across(where(is.numeric), format, nsmall = 2)) |>
  mutate(across(contains("20"), ~ paste0(.x, "%")))

## Figure 1 / Table 2 -----------------------------------------------------------------
figure_1_raw <- national_data_fy |>
  group_by(`Financial Year`) |>
  summarise(
    `Prescription items` = sum(`Total Items`),
    `Identified patients` = sum(`Total Identified Patients`)
  ) |>
  pivot_longer(
    cols = c(`Prescription items`, `Identified patients`),
    names_to = "Measure",
    values_to = "Value"
  )

table_2 <- figure_1_raw |>
  arrange(Measure) |>
  mutate(Value = format(Value, big.mark = ",")) |>
  pivot_wider(names_from = c("Measure"),
              values_from = c("Value"))

figure_1_data <- figure_1_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

figure_1 <- group_chart_hc(
  data = figure_1_data,
  x = FINANCIAL_YEAR,
  y = VALUE,
  group = MEASURE,
  type = "line",
  xLab = "Financial year",
  yLab = "Number of identified patients / prescription items",
  title = ""
) |>
  hc_subtitle(text = "M = Millions",
              align = "left") |>
  highcharter::hc_plotOptions(series = list(enableMouseTracking = FALSE))

# adjust figure 1 datalabels to match scale (M)
figure_1$x$hc_opts$series[[1]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return ynum.toLocaleString('en-GB', options) + 'M';
}
"
)

figure_1$x$hc_opts$series[[2]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return ynum.toLocaleString('en-GB', options) + 'M';
}
"
)

## Figure 2 / Table 3 -----------------------------------------------------------------
figure_2_raw <- national_data_fy |>
  group_by(`Financial Year`) |>
  summarise(`Total Net Ingredient Cost (GBP)` = sum(`Total Net Ingredient Cost (GBP)`)) |>
  pivot_longer(
    cols = c(`Total Net Ingredient Cost (GBP)`),
    names_to = "measure",
    values_to = "value"
  )

table_3 <- figure_2_raw |>
  mutate(value = format(value, big.mark = ",")) |>
  pivot_wider(names_from = c("measure"),
              values_from = c("value"))

figure_2_data <- figure_2_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

figure_2 <- group_chart_hc(
  data = figure_2_data,
  x = FINANCIAL_YEAR,
  y = VALUE,
  group = MEASURE,
  type = "line",
  xLab = "Financial year",
  yLab = "Total Net Ingredient Cost (GBP)",
  title = "",
  currency = TRUE
) |>
  hc_subtitle(text = "M = Millions",
              align = "left") |>
  highcharter::hc_plotOptions(series = list(enableMouseTracking = FALSE)) |>
  hc_legend(enabled = FALSE)

figure_2$x$hc_opts$series[[1]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return '£' + ynum.toLocaleString('en-GB', options) + 'M';
}
"
)

## Figure 3 / Table 4 -----------------------------------------------------------------
figure_3_raw <- category_data_fy |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  group_by(`Financial Year`, `Drug Category`) |>
  summarise(`Total Items` = sum(`Total Items`)) |>
  pivot_longer(
    cols = c(`Total Items`),
    names_to = "measure",
    values_to = "value"
  ) 

table_4 <- figure_3_raw |>
  mutate(value = format(value, big.mark = ",")) |>
  pivot_wider(names_from = c("measure"),
              values_from = c("value"))
  
figure_3_data <- figure_3_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything()) |>
  mutate(ROUNDED_VALUE = signif(VALUE, 3))

figure_3 <- group_chart_hc(
  data = figure_3_data,
  x = FINANCIAL_YEAR,
  y = ROUNDED_VALUE,
  group = DRUG_CATEGORY,
  type = "line",
  xLab = "Financial year",
  yLab = "Number of prescribed items",
  title = "",
  dlOn = F
) |>
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) |>
  hc_subtitle(text = "M = Millions",
              align = "left")

## Figure 4 / Table 5 -----------------------------------------------------------------
figure_4_raw <- population_category_data_fy |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  group_by(`Financial Year`, `Drug Category`) |>
  summarise(`Patients per 1,000 Population` = `Patients per 1,000 Population`) |>
  pivot_longer(
    cols = c(`Patients per 1,000 Population`),
    names_to = "measure",
    values_to = "value"
  ) 

table_5 <- figure_4_raw |>
  mutate(value = format(round(value, 1), big.mark = ",")) |>
  pivot_wider(names_from = c("measure"),
              values_from = c("value"))
  
figure_4_data <- figure_4_raw |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything()) |>
  na.omit() |>
  mutate(ROUNDED_VALUE = signif(VALUE, 3))

figure_4 <- group_chart_hc(
  data = figure_4_data,
  x = FINANCIAL_YEAR,
  y = ROUNDED_VALUE,
  group = DRUG_CATEGORY,
  type = "line",
  xLab = "Financial year",
  yLab = "Patients per 1,000 Population",
  title = "",
  dlOn = F
) |>
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) |>
  hc_legend(enabled = TRUE)

## Figure 5 / Table 6 -----------------------------------------------------------------
figure_5_raw <- category_data_fy |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  group_by(`Financial Year`, `Drug Category`) |>
  summarise(`Total Net Ingredient Cost (GBP)` = sum(`Total Net Ingredient Cost (GBP)`)) |>
  pivot_longer(
    cols = c(`Total Net Ingredient Cost (GBP)`),
    names_to = "measure",
    values_to = "value"
  ) 

table_6 <- figure_5_raw |>
  mutate(value = format(round(value, 1), big.mark = ",")) |>
  pivot_wider(names_from = c("measure"),
              values_from = c("value"))

figure_5_data <- figure_5_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything()) |>
  na.omit() |>
  mutate(ROUNDED_VALUE = signif(VALUE, 3))

figure_5 <- group_chart_hc(
  data = figure_5_data,
  x = FINANCIAL_YEAR,
  y = ROUNDED_VALUE,
  group = DRUG_CATEGORY,
  type = "line",
  xLab = "Financial year",
  yLab = "Cost (GBP)",
  title = "",
  dlOn = F
) |>
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) |>
  hc_legend(enabled = TRUE)|>
  hc_subtitle(text = "M = Millions",
              align = "left")

## Figure 6 / Table 7 -----------------------------------------------------------------
figure_6_raw <- national_data_fy |>
  filter(`Identified Patient Flag` == "Y") |>
  mutate(`Items Per Patient` = `Total Items`  / `Total Identified Patients`) |>
  select(`Financial Year`,
         `Total Items`,
         `Total Identified Patients`,
         `Items Per Patient`)

table_7 <- figure_6_raw |>
  mutate(`Total Items` = format(`Total Items`, big.mark = ","),
         `Total Identified Patients` = format(`Total Identified Patients`, big.mark = ","),
         `Items Per Patient` = format(round(`Items Per Patient`,1), big.mark = ","))

figure_6_data <- figure_6_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

figure_6 <- basic_chart_hc(
  data = figure_6_data,
  x = FINANCIAL_YEAR,
  y = ITEMS_PER_PATIENT,
  type = "line",
  xLab = "Financial year",
  yLab = "Prescription items per patient",
  title = ""
)|>
  hc_yAxis(min = 0)|>
  highcharter::hc_plotOptions(
    series = list(
      enableMouseTracking = FALSE
    )
  )

## Figure 7 / Table 8 -----------------------------------------------------------------
figure_7_raw <- gender_data_fy |>
  filter(`Patient Gender` != "Unknown") |>
  group_by(`Financial Year`, `Patient Gender`) |>
  summarise(`Total Identified Patients` =
              sum(`Total Identified Patients`)) |>
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  )

table_8 <- figure_7_raw |>
  mutate(value = format(round(value, 1), big.mark = ",")) |>
  select(-measure) |>
  rename(
    "Total Identified Patients" = 3
  )

figure_7_data <- figure_7_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything()) |>
  mutate(ROUNDED_VALUE = signif(VALUE, 3))

figure_7 <-  group_chart_hc(
  data = figure_7_data,
  x = FINANCIAL_YEAR,
  y = ROUNDED_VALUE,
  group = PATIENT_GENDER,
  type = "line",
  xLab = "Financial year",
  yLab = "Number of identified patients",
  title = "",
  dlOn = T
) |>
  hc_subtitle(text = "M = Millions",
              align = "left") |>
  highcharter::hc_plotOptions(series = list(enableMouseTracking = FALSE))

# adjust figure 1 datalabels to match scale (M)
figure_7$x$hc_opts$series[[1]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return ynum.toLocaleString('en-GB', options) + 'M';
}
"
)

figure_7$x$hc_opts$series[[2]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return ynum.toLocaleString('en-GB', options) + 'M';
}
"
)
## Figure 8 / Table 9 -----------------------------------------------------------------
figure_8_raw <- age_gender_data_fy |>
  select(`Financial Year`,
         `Age Band`,
         `Patient Gender`,
         `Total Identified Patients`) |>
  filter(`Financial Year` == max(`Financial Year`))

table_9 <- figure_8_raw |>
  mutate(`Total Identified Patients` = format(`Total Identified Patients`, big.mark = ","))

figure_8_data <- figure_8_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

figure_8 <- age_gender_chart(figure_8_data,
                              labels = FALSE)

## Figure 9 / Table 10 -----------------------------------------------------------------
figure_9_raw <- imd_data_fy |>
  select(-`Total Items`,-`Total Net Ingredient Cost (GBP)`) |>
  filter(`Financial Year` == max(`Financial Year`),
         `IMD Quintile` != "Unknown") |>
  arrange(`IMD Quintile`) |>
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  )

table_10 <- figure_9_raw |>
  mutate(value = format(round(value, 1), big.mark = ",")) |>
  pivot_wider(names_from = c("measure"),
              values_from = c("value"))

figure_9_data <- figure_9_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

figure_9 <- basic_chart_hc(
  data = figure_9_data,
  x = IMD_QUINTILE,
  y = VALUE,
  type = "column",
  xLab = "IMD quintile",
  yLab = "Number of identified patients",
  title = ""
)|>
  hc_subtitle(text = "K = Thousands",
              align = "left") |>
  highcharter::hc_plotOptions(series = list(enableMouseTracking = FALSE)) 

figure_9$x$hc_opts$series[[1]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return ynum.toLocaleString('en-GB', options) + 'K';
}
"
)

## Figure 10 / Table 11 -----------------------------------------------------------------
figure_10_raw <- coprescribing_data |>
  filter(`Year Month` == max_year_month,
         `Number of Categories` > 1) |>
  arrange(`Number of Categories`) |>
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  ) 

table_11 <- figure_10_raw |>
  mutate(value = format(round(value, 1), big.mark = ",")) |>
  pivot_wider(names_from = c("measure"),
              values_from = c("value"))
  
figure_10_data <- figure_10_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())

figure_10 <-
  basic_chart_hc(
    data = figure_10_data,
    x = NUMBER_OF_CATEGORIES,
    y = VALUE,
    type = "column",
    xLab = "Number of Categories",
    yLab = "Number of identified patients",
    title = ""
  )|>
  hc_subtitle(text = "K = Thousands",
              align = "left") |>
  highcharter::hc_plotOptions(series = list(enableMouseTracking = FALSE))

# adjust figure 1 datalabels to match scale (M)
figure_10$x$hc_opts$series[[1]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return ynum.toLocaleString('en-GB', options) + 'K';
}
"
)

## Figure 11 / Table 12 -----------------------------------------------------------------
figure_11_raw <- coprescribing_matrix_data |>
  filter(`Year Month` == max_year_month) |>
  arrange(desc(`Total Identified Patients`)) |>
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  ) 

table_12 <- figure_11_raw |>
  mutate(value = format(round(value, 1), big.mark = ",")) |>
  pivot_wider(names_from = c("measure"),
              values_from = c("value"))

figure_11_data <- figure_11_raw |>
  rename_with(~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything()) |>
  mutate(DRUG_COMBINATION = str_replace(DRUG_COMBINATION, "and ", "and <br>"))

figure_11 <- basic_chart_hc(
  data = figure_11_data,
  x = DRUG_COMBINATION,
  y = VALUE,
  type = "column",
  xLab = "Drug Combination",
  yLab = "Number of identified patients",
  title = ""
) |>
  hc_xAxis(labels = list(rotation = 45))|>
  hc_subtitle(text = "K = Thousands",
              align = "left") |>
  highcharter::hc_plotOptions(series = list(enableMouseTracking = FALSE))

# adjust figure 1 datalabels to match scale (M)
figure_11$x$hc_opts$series[[1]]$dataLabels$formatter <- JS(
  "function formatCurrency() {
    var ynum = this.point.y/1000;
    var options = { maximumSignificantDigits: 3, minimumSignificantDigits: 3 };
    return ynum.toLocaleString('en-GB', options) + 'K';
}
"
)

# 13. create markdowns -------

# narrative

rmarkdown::render("dfm_annual_narrative.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/dfm_summary_narrative_2023_24_v001.html")

rmarkdown::render("dfm_annual_narrative.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/dfm_summary_narrative_2023_24_v001.docx")

# user engagement
#
# rmarkdown::render("dfm_user_engagement_2223.Rmd",
#                   output_format = "html_document",
#                   output_file = "outputs/dfm_user_engagement_2023_24_v001.html")

# background

rmarkdown::render("dfm-background.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/dfm_background_info_methodology_v001.html")

rmarkdown::render("dfm-background.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/dfm_background_info_methodology_v001.docx")

# 14. disconnect from DWH  ---------
DBI::dbDisconnect(con)
