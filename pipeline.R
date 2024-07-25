### Pipeline to run PCA annual publication
# clear environment
rm(list = ls())

# source functions
# get all .R files in the functions sub-folder
function_files <- list.files(path = "functions", pattern = "\\.R$")

# loop over function_files to source them all
for (file in function_files) {
  source(file.path("functions", file))
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

schema <- config$sql_schema

# 3. Extract data required ------------------------------------------------

age_category_data <-
  age_category_extract(con = con,
                       schema = schema,
                       table = config$sql_table_name) |>
  apply_sdc(rounding = F)

age_data <-
  age_extract(con = con,
              schema = schema,
              table = config$sql_table_name) |>
  apply_sdc(rounding = F)

age_gender_cat_data <-
  age_gender_cat_extract(con = con,
                         schema = schema,
                         table = config$sql_table_name) |>
  apply_sdc(rounding = F)

age_gender_data <-
  age_gender_extract(con = con,
                     schema = schema,
                     table = config$sql_table_name) |>
  apply_sdc(rounding = F)

ageband_data <-
  ageband_extract(con = con,
                  schema = schema,
                  table = config$sql_table_name) |>
  apply_sdc(rounding = F)

category_data <-
  category_extract(con = con,
                   schema = schema,
                   table = config$sql_table_name) |>
  apply_sdc(rounding = F)

coprescribing_data <-
  coprescribing_extract(con = con,
                        schema = schema,
                        table = config$sql_table_name)

coprescribing_matrix_data <-
  coprescribing_matrix_extract(con = con,
                               schema = schema,
                               table = config$sql_table_name)

gender_category_data <-
  gender_category_extract(con = con,
                          schema = schema,
                          table = config$sql_table_name) |>
  apply_sdc(rounding = F)

gender_data <-
  gender_extract(con = con,
                 schema = schema,
                 table = config$sql_table_name) |>
  apply_sdc(rounding = F)

icb_category_data <-
  icb_category_extract(con = con,
                       schema = schema,
                       table = config$sql_table_name) |>
  apply_sdc(rounding = F)

icb_data <-
  icb_extract(con = con,
              schema = schema,
              table = config$sql_table_name) |>
  apply_sdc(rounding = F)

imd_category_data <-
  imd_category_extract(con = con,
                       schema = schema,
                       table = config$sql_table_name) |>
  apply_sdc(rounding = F)

imd_data <-
  imd_extract(con = con,
              schema = schema,
              table = config$sql_table_name) |>
  apply_sdc(rounding = F)

national_data <-
  national_extract(con = con,
                   schema = schema,
                   table = config$sql_table_name) |>
  apply_sdc(rounding = F)

national_pop <- ons_national_pop(year = c(2015:2023),
                                 area = "ENPOP")

population_category_data <- category_data |>
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
  )

population_data <- national_data |>
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
  )

patient_identification_dt <-
  capture_rate_extract_dt(con = con,
                          schema = schema,
                          table = config$sql_table_name) |>
  select(1, last_col(4):last_col())

patient_identification <-
  capture_rate_extract(con = con,
                       schema = schema,
                       table = config$sql_table_name)


# 4. Build costs and items Excel tables ------------------------------------------------------
sheetNames <- c(
  "Patient_Identification",
  "Table_1",
  "Table_2",
  "Table_3",
  "Table_4",
  "Table_5",
  "Table_6"
)

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "Drug Category",
  "Financial Year",
  "Identified Patient",
  "Integrated Care Board Code",
  "Integrated Care Board Name",
  "Mid-Year England Population Estimate",
  "Mid-Year Population Year",
  "Patients per 1,000 Population",
  "Total Items",
  "Total Net Ingredient Cost (GBP)",
  "Total Patients"
)

meta_descs <-
  c(
    "The category of class of drug that can cause dependence.",
    "The financial year to which the data belongs.",
    "This shows where an item has been attributed to an NHS number that has been verified by the Personal Demographics Service (PDS).",
    "The unique code used to refer to an Integrated Care Board (ICB).",
    "The name given to the Integrated Care Board (ICB) that a prescribing organisation belongs to. This is based upon NHSBSA administrative records, not geographical boundaries and more closely reflect the operational organisation of practices than other geographical data sources.",
    "The population estimate for the corresponding Mid-Year Population Year.",
    "The year in which population estimates were taken, required due to the presentation of this data in financial year format.",
    "(Total Identified Patients / Mid-Year England Population Estimate) * 1000.",
    "The number of prescription items dispensed. 'Items' is the number of times a product appears on a prescription form. Prescription forms include both paper prescriptions and electronic messages.",
    "Total Net Ingredient Cost is the amount that would be paid using the basic price of the prescribed drug or appliance and the quantity prescribed. Sometimes called the 'Net Ingredient Cost' (NIC). The basic price is given either in the Drug Tariff or is determined from prices published by manufacturers, wholesalers or suppliers. Basic price is set out in Parts 8 and 9 of the Drug Tariff. For any drugs or appliances not in Part 8, the price is usually taken from the manufacturer, wholesaler or supplier of the product. This is given in GBP (£).",
    "Where patients are identified via the flag, the number of patients that the data corresponds to. This will always be 0 where 'Identified Patient' = N."
    
  )

accessibleTables::create_metadata(wb,
                                  meta_fields,
                                  meta_descs)

#### Patient identification
# write data to sheet
write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Dependency Forming Medicines - 2015/16 to ",
    config$fy,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "The below proportions reflect the percentage of prescription items where a PDS verified NHS number was recorded."
  ),
  patient_identification,
  42
)
#left align columns A to C
format_data(wb,
            "Patient_Identification",
            c("A", "B"),
            "left",
            "")
#right align columns and round to 2 DP - D (if using long data not pivoting wider) (!!NEED TO UPDATE AS DATA EXPANDS!!)
format_data(wb,
            "Patient_Identification",
            c("C"),
            "right",
            "0.00")

#### national data
# write data to sheet
write_sheet(
  wb,
  "Table_1",
  paste0(
    "Table 1: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Total dispensed items and costs per financial year"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "3. Total costs and items may not be reconciled back to Prescribing Cost Analysis (PCA) publication figures as these figures are based around a 'prescribing view' of the data. This is where we use the drug or device that was prescribed to a patient, rather than the drug that was reimbursed to the dispenser to classify a prescription item. PCA uses a dispensing view where the inverse is true."
  ),
  national_data,
  14
)

#left align columns A to C
format_data(wb,
            "Table_1",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Table_1",
            c("C", "D"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Table_1",
            c("E"),
            "right",
            "#,##0.00")

#### category data
# write data to sheet
write_sheet(
  wb,
  "Table_2",
  paste0(
    "Table 2: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly totals split by Drug Category and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  category_data |> select(-`BNF Section Name`, -`BNF Section Code`),
  14
)

#left align columns A to C
format_data(wb,
            "Table_2",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Table_2",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Table_2",
            c("F"),
            "right",
            "#,##0.00")

#### ICB data
# write data to sheet
write_sheet(
  wb,
  "Table_3",
  paste0(
    "Table 3: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly totals split by Integrated Care Board and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "3. Integrated Care Boards (ICBs) succeeded sustainability and transformation plans (STPs) and replaced the functions of clinical commissioning groups (CCGs) in July 2022 with ICB sub locations replacing CCGs during the transition period of 2022/23."
  ),
  icb_data,
  14
)

#left align columns A to C
format_data(wb,
            "Table_3",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Table_3",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Table_3",
            c("G"),
            "right",
            "#,##0.00")

#### ICB category data
# write data to sheet
write_sheet(
  wb,
  "Table_4",
  paste0(
    "Table 4: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly totals split by Integrated Care Board, Drug Category and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "3. Integrated Care Boards (ICBs) succeeded sustainability and transformation plans (STPs) and replaced the functions of clinical commissioning groups (CCGs) in July 2022 with ICB sub locations replacing CCGs during the transition period of 2022/23."
  ),
  icb_category_data,
  14
)

#left align columns A to C
format_data(wb,
            "Table_4",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Table_4",
            c("F", "G"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Table_4",
            c("H"),
            "right",
            "#,##0.00")

#### National population data
# write data to sheet
write_sheet(
  wb,
  "Table_5",
  paste0(
    "Table 5: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Population totals split by financial year"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. ONS population estimates taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates.",
    "3. ONS population estimates for 2022/2023 were not available prior to publication."
  ),
  population_data,
  14
)

#left align columns A to C
format_data(wb,
            "Table_5",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Table_5",
            c("C", "D"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Table_5",
            c("E"),
            "right",
            "#,##0.00")

#### National population category data
# write data to sheet
write_sheet(
  wb,
  "Table_6",
  paste0(
    "Table 6: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Population totals split by financial year and Drug Category"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. ONS population estimates taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates.",
    "3. ONS population estimates for 2022/2023 were not available prior to publication."
  ),
  population_category_data,
  14
)

#left align columns A to C
format_data(wb,
            "Table_6",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Table_6",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Table_6",
            c("F"),
            "right",
            "#,##0.00")

# build cover sheet
accessibleTables::makeCoverSheet(
  paste0("Dependency Forming Medicines - England 2015/16 - ", config$fy),
  "Costs and Items",
  "Publication date: 7 September 2023",
  wb,
  sheetNames,
  c(
    "Metadata",
    "Patient Identification Rates",
    "Table 1: Total items",
    "Table 2:  Items by category",
    "Table 3:  Items by Integrated Care Board (ICB)",
    "Table 4:  Items by ICB and category",
    "Table 5:  National Population",
    "Table 6:  Population by category"
  ),
  c("Metadata", sheetNames)
)

#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       "outputs/dfm_2022_2023_costs_and_items_v001.xlsx",
                       overwrite = TRUE)


# 5. Build demographics Excel file -------------------------------------------
sheetNames_dem <- c(
  "Patient_Identification",
  "Table_1",
  "Table_2",
  "Table_3",
  "Table_4",
  "Table_5",
  "Table_6",
  "Table_7",
  "Table_8",
  "Table_9",
  "Table_10",
  "Table_11"
)

wb_dem <- accessibleTables::create_wb(sheetNames_dem)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields_dem <- c(
  "Age Band",
  "Drug Category",
  "Financial Year",
  "Identified Patient",
  "IMD Quintile",
  "Patient Gender",
  "Total Items",
  "Total Net Ingredient Cost (GBP)",
  "Total Patients",
  "Year Month"
)

meta_descs_dem <-
  c(
    "The age band of the patient as of the 30th September of the corresponding financial year the drug was prescribed.",
    "The category of class of drug that can cause dependence.",
    "The financial year to which the data belongs.",
    "This shows where an item has been attributed to an NHS number that has been verified by the Personal Demographics Service (PDS).",
    "The IMD Quintile of the patient, based on the location of their practice, where '1' is the 20% of areas with the highest deprivation score in the Index of Multiple Deprivation (IMD) from the English Indices of Deprivation 2019, and '5' is the 20% of areas with the lowest IMD deprivation score. Unknown values are where we have been unable to match the patient postcode to a postcode in the National Statistics Postcode Lookup (NSPL).",
    "The gender of the patient as noted at the time the prescription was processed. This includes where the patient has been identified but the gender has not been recorded.",
    "The number of prescription items dispensed. 'Items' is the number of times a product appears on a prescription form. Prescription forms include both paper prescriptions and electronic messages.",
    "Total Net Ingredient Cost is the amount that would be paid using the basic price of the prescribed drug or appliance and the quantity prescribed. Sometimes called the 'Net Ingredient Cost' (NIC). The basic price is given either in the Drug Tariff or is determined from prices published by manufacturers, wholesalers or suppliers. Basic price is set out in Parts 8 and 9 of the Drug Tariff. For any drugs or appliances not in Part 8, the price is usually taken from the manufacturer, wholesaler or supplier of the product. This is given in GBP (Â£).",
    "Where patients are identified via the flag, the number of patients that the data corresponds to. This will always be 0 where 'Identified Patient' = N.",
    "The year and month to which the data belongs, denoted in YYYYMM format."
  )

accessibleTables::create_metadata(wb_dem,
                                  meta_fields_dem,
                                  meta_descs_dem)

#### Patient identification
# write data to sheet
write_sheet(
  wb_dem,
  "Patient_Identification",
  paste0(
    "Dependency Forming Medicines - 2015/16 to ",
    config$fy,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "The below proportions reflect the percentage of prescription items where a PDS verified NHS number was recorded."
  ),
  patient_identification,
  17
)
#left align columns A to C
format_data(wb_dem,
            "Patient_Identification",
            c("A", "B"),
            "left",
            "")
#right align columns and round to 2 DP - D (if using long data not pivoting wider) (!!NEED TO UPDATE AS DATA EXPANDS!!)
format_data(wb_dem,
            "Patient_Identification",
            c("C"),
            "right",
            "0.00")

#### national data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_1",
  paste0(
    "Table 1: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Total dispensed items and costs per financial year"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "3. Total costs and items may not be reconciled back to Prescribing Cost Analysis (PCA) publication figures as these figures are based around a 'prescribing view' of the data. This is where we use the drug or device that was prescribed to a patient, rather than the drug that was reimbursed to the dispenser to classify a prescription item. PCA uses a dispensing view where the inverse is true."
  ),
  national_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_1",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_1",
            c("C", "D"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_1",
            c("E"),
            "right",
            "#,##0.00")

#### gender data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_2",
  paste0(
    "Table 2: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - National prescribing by gender per financial year"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. It is possible for a patient to be marked as 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together.",
    "3. The NHSBSA does not use the latest national data standard relating to patient gender, and use historic nomenclature in some cases. Please see the detailed Background Information and Methodology notice released with this publication for further information."
  ),
  gender_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_2",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_2",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_2",
            c("F"),
            "right",
            "#,##0.00")

#### gender category  data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_3",
  paste0(
    "Table 3: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly patients, items and costs by Drug Category split by patient gender"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. It is possible for a patient to be marked as 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together.",
    "3. The NHSBSA does not use the latest national data standard relating to patient gender, and use historic nomenclature in some cases. Please see the detailed Background Information and Methodology notice released with this publication for further information.",
    "4. Statistical disclosure control is applied where prescribing relates to fewer than 5 patients. In these instances, figures are shown as blanks."
  ),
  gender_category_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_3",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_3",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_3",
            c("G"),
            "right",
            "#,##0.00")

#### age data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_4",
  paste0(
    "Table 4: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - National prescribing by age per financial year"
  ),
  c("1. Field definitions can be found on the 'Metadata' tab."),
  age_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_4",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_4",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_4",
            c("F"),
            "right",
            "#,##0.00")

#### age category  data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_5",
  paste0(
    "Table 5: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly patients, items and costs by Drug Category split by patient gender"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "3. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
  ),
  age_category_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_5",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_5",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_5",
            c("G"),
            "right",
            "#,##0.00")

#### age gender  data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_6",
  paste0(
    "Table 6: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - National prescribing by age and gender per financial year"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. These totals only include patients where both age and gender are known."
  ),
  age_gender_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_6",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_6",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_6",
            c("G"),
            "right",
            "#,##0.00")

#### age gender category data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_7",
  paste0(
    "Table 7: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly patients, items and costs by Drug Category split by patient age and gender"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. These totals only include patients where both age and gender are known.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "4. Statistical disclosure control has been applied to cells containing 5 or fewer patients or items. These cells will appear blank."
    
  ),
  age_gender_cat_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_7",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_7",
            c("F", "G"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_7",
            c("H"),
            "right",
            "#,##0.00")

#### imd data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_8",
  paste0(
    "Table 8: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly patients, items and costs by IMD Quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. IMD Quintiles used are taken from the English Indices of Deprivation 2019 National Statistics publication.",
    "3. Where a patients lower-layer super output areas (LSOAs) is not available or has not been able to to be matched to National Statistics Postcode Lookup (May 2022) the records are reported as 'unknown' IMD Quintile.",
    "4. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  imd_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_8",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_8",
            c("C", "D"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_8",
            c("E"),
            "right",
            "#,##0.00")

#### imd category data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_9",
  paste0(
    "Table 9: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly patients, items and costs by Drug Category split by IMD Quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. IMD Quintiles used are taken from the English Indices of Deprivation 2019 National Statistics publication.",
    "3. Where a patients lower-layer super output areas (LSOAs) is not available or has not been able to to be matched to National Statistics Postcode Lookup (May 2022) the records are reported as 'unknown' IMD Quintile.",
    "4. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  imd_category_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_9",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_9",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb_dem,
            "Table_9",
            c("F"),
            "right",
            "#,##0.00")

#### copresc data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_10",
  paste0(
    "Table 10: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Monthly patients by number of categories of drugs recieved"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  coprescribing_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_10",
            c("A"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_10",
            c("B", "C"),
            "right",
            "#,##0")

#### copresc matrix data
# write data to sheet
write_sheet(
  wb_dem,
  "Table_11",
  paste0(
    "Table 11: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Monthly patients by combination of drugs recieved for those recieving 2 drug categories"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  coprescribing_matrix_data,
  14
)

#left align columns A to C
format_data(wb_dem,
            "Table_11",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb_dem,
            "Table_11",
            c("C"),
            "right",
            "#,##0")

# build cover sheet
accessibleTables::makeCoverSheet(
  paste0("Dependency Forming Medicines - England 2015/16 - ", config$fy),
  "Patient Demographics",
  "Publication date: 7 September 2023",
  wb_dem,
  sheetNames_dem,
  c(
    "Metadata",
    "Patient Identification Rates",
    "Table 1: Total Items",
    "Table 2: Gender",
    "Table 3: Gender by category",
    "Table 4: Age band",
    "Table 5: Age band by category",
    "Table 6: Age band and gender",
    "Table 7: Age band and gender by category",
    "Table 8: Indices of Deprivation",
    "Table 9: Indices of Deprivation by category",
    "Table 10: Coprescribing",
    "Table 11: Coprescribing Combinations"
  ),
  c("Metadata", sheetNames_dem)
)


#save file into outputs folder
openxlsx::saveWorkbook(wb_dem,
                       "outputs/dfm_2022_2023_patient_demographics_v001.xlsx",
                       overwrite = TRUE)


# 6. Create charts/tables and data ----------------------------------------

table_1_data <- patient_identification |>
  filter(`Drug Category` != "ANTIDEPRESSANTS") |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())


table_1 <- patient_identification_dt |>
  filter(`Drug Category` != "ANTIDEPRESSANTS") |>
  mutate(`Drug Category` = str_to_title(`Drug Category`)) |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  mutate(across(where(is.numeric), round, 2)) |>
  mutate(across(where(is.numeric), format, nsmall = 2)) |>
  mutate(across(contains("20"), ~ paste0(.x, "%"))) |>
  DT::datatable(rownames = FALSE,
                options = list(dom = "t",
                               columnDefs = list(
                                 list(orderable = FALSE,
                                      targets = "_all"),
                                 list(className = "dt-left", targets = 0:0),
                                 list(className = "dt-right", targets = 1:5)
                               )))

figure_1_data <- national_data |>
  group_by(`Financial Year`) |>
  summarise(
    `Prescription items` = sum(`Total Items`),
    `Identified patients` = sum(`Total Identified Patients`)
  ) |>
  pivot_longer(
    cols = c(`Prescription items`, `Identified patients`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())


figure_1 <- group_chart_hc(
  data = figure_1_data,
  x = FINANCIAL_YEAR,
  y = VALUE,
  group = MEASURE,
  type = "line",
  xLab = "Financial year",
  yLab = "Number of prescription items/identified patients",
  title = ""
  
)

figure_2_data <- national_data |>
  group_by(`Financial Year`) |>
  summarise(`Total Net Ingredient Cost (GBP)` = sum(`Total Net Ingredient Cost (GBP)`)) |>
  pivot_longer(
    cols = c(`Total Net Ingredient Cost (GBP)`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())


figure_2 <- group_chart_hc(
  data = figure_2_data,
  x = FINANCIAL_YEAR,
  y = VALUE,
  group = MEASURE,
  type = "line",
  xLab = "Financial year",
  yLab = "Cost (GBP)",
  title = "",
  currency = TRUE
)

figure_3_data <- category_data |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  group_by(`Financial Year`, `Drug Category`) |>
  summarise(`Total Items` = sum(`Total Items`)) |>
  pivot_longer(
    cols = c(`Total Items`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
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
  hc_legend(enabled = TRUE)


figure_4_data <- population_category_data |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  group_by(`Financial Year`, `Drug Category`) |>
  summarise(`Patients per 1,000 Population` = `Patients per 1,000 Population`) |>
  pivot_longer(
    cols = c(`Patients per 1,000 Population`),
    names_to = "measure",
    values_to = "value"
  ) |>
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

figure_5_data <- category_data |>
  mutate(`Drug Category` = case_when(`Drug Category` == "Z-Drugs" ~ "Z-drugs",
                                     TRUE ~ `Drug Category`)) |>
  group_by(`Financial Year`, `Drug Category`) |>
  summarise(`Total Net Ingredient Cost (GBP)` = sum(`Total Net Ingredient Cost (GBP)`)) |>
  pivot_longer(
    cols = c(`Total Net Ingredient Cost (GBP)`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
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
  hc_legend(enabled = TRUE)

figure_6_data <- national_data |>
  filter(`Identified Patient Flag` == "Y") |>
  mutate(`Items Per Patient` = `Total Items`  / `Total Identified Patients`) |>
  select(`Financial Year`,
         `Total Items`,
         `Total Identified Patients`,
         `Items Per Patient`) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
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
)

figure_7_data <- gender_data |>
  filter(`Patient Gender` != "Unknown") |>
  group_by(`Financial Year`, `Patient Gender`) |>
  summarise(`Total Identified Patients` =
              sum(`Total Identified Patients`)) |>
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything()) |>
  mutate(ROUNDED_VALUE = signif(VALUE, 3))

figure_7 <-  group_chart_hc(
  data = figure_7_data,
  x = FINANCIAL_YEAR,
  y = ROUNDED_VALUE,
  group = PATIENT_GENDER,
  type = "column",
  xLab = "Financial year",
  yLab = "Number of identified patients",
  title = "",
  dlOn = F
) |>
  hc_tooltip(enabled = T,
             shared = T,
             sort = T)

figure_8_data <- age_gender_data |>
  select(`Financial Year`,
         `Age Band`,
         `Patient Gender`,
         `Total Identified Patients`) |>
  filter(`Financial Year` == max(`Financial Year`)) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
    "[^[:alnum:] ]", "", .
  ))), everything())


figure_8 <-  age_gender_chart(figure_8_data,
                              labels = FALSE)

figure_9_data <- imd_data |>
  select(-`Total Items`, -`Total Net Ingredient Cost (GBP)`) |>
  filter(`Financial Year` == max(`Financial Year`),
         `IMD Quintile` != "Unknown") |>
  arrange(`IMD Quintile`) |>
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
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
)

figure_10_data <- coprescribing_data |>
  filter(`Year Month` == max(`Year Month`),
         `Number of Categories` > 1) |>
  arrange(`Number of Categories`) |>
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
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
  )

figure_11_data <- coprescribing_matrix_data |>
  filter(`Year Month` == max(`Year Month`)) |>
  arrange(desc(`Total Identified Patients`)) |>
  
  pivot_longer(
    cols = c(`Total Identified Patients`),
    names_to = "measure",
    values_to = "value"
  ) |>
  rename_with( ~ gsub(" ", "_", toupper(gsub(
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
  hc_tooltip(enabled = T,
             shared = T,
             sort = T) |>
  hc_xAxis(labels = list(rotation = 45))


# 7. create markdowns -------

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

rmarkdown::render(
  "dfm-background-september-2023.Rmd",
  output_format = "html_document",
  output_file = "outputs/dfm_background_info_methodology_v001.html"
)

rmarkdown::render(
  "dfm-background-september-2023.Rmd",
  output_format = "word_document",
  output_file = "outputs/dfm_background_info_methodology_v001.docx"
)

# 8. disconnect from DWH  ---------
DBI::dbDisconnect(con)



