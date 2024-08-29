sheetNames <- c("Category_Numbers",
                "Drug_Combinations")

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c("Drug Combination",
                 "Number of Categories",
                 "Total Identified Patients",
                 "Year Month")

meta_descs <-
  c(
    "The combination of drug categories that can cause dependence dispensed to a patieint in the time scale.",
    "The number of unique drug categories that can cause dependence dispensed to a patieint in the time scale.",
    "Where patients are identified via the flag, the number of patients that the data corresponds to. This will always be 0 where 'Identified Patient' = N.",
    "The year and month to which the data belongs, denoted in YYYYMM format."
  )

accessibleTables::create_metadata(wb,
                                  meta_fields,
                                  meta_descs)

#### copresc data
# write data to sheet
write_sheet(
  wb,
  "Category_Numbers",
  paste0(
    "Dependency Forming Medicines - England April 2015 to ",
    config$month,
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
format_data(wb,
            "Category_Numbers",
            c("A"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Category_Numbers",
            c("B", "C"),
            "right",
            "#,##0")

#### copresc matrix data
# write data to sheet
write_sheet(
  wb,
  "Drug_Combinations",
  paste0(
    "Table 11: Dependency Forming Medicines - England April 2015 to ",
    config$month,
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
format_data(wb,
            "Drug_Combinations",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Drug_Combinations",
            c("C"),
            "right",
            "#,##0")

# Cover sheet --------------------------------------------------
# build cover sheet
accessibleTables::makeCoverSheet(
  paste0("Dependency-Forming Medicines - England April 2015 - ", config$month),
  "Co-prescribing Statistics",
  paste0("Publication date: ", config$publication_date),
  wb,
  sheetNames,
  c(
    "Metadata",
    "Monthly patients by number of categories of drugs recieved",
    "Monthly patients by combination of drugs recieved for those recieving 2 drug categories"
  ),
  c("Metadata", sheetNames)
)

#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       "outputs/dfm_june_2024_co_prescribing_v001.xlsx",
                       overwrite = TRUE)
