sheetNames <- c(
  "Patient_Identification",
  "National",
  "National_Population",
  "Category",
  "Category_Population",
  "ICB",
  "ICB_Category"
)

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "Age Band",
  "Drug Category",
  "Financial Year",
  "Identified Patient",
  "IMD Quintile",
  "Integrated Care Board Code",
  "Integrated Care Board Name",
  "Mid-Year England Population Estimate",
  "Mid-Year Population Year",
  "The gender of the patient as noted at the time the prescription was processed. This includes where the patient has been identified but the gender has not been recorded.",
  "Patients per 1,000 Population",
  "Total Items",
  "Total Net Ingredient Cost (GBP)",
  "Total Patients"
)

meta_descs <-
  c(
    "The age band of the patient as of the 30th September of the corresponding financial year the drug was prescribed.",
    "The category of class of drug that can cause dependence.",
    "The financial year to which the data belongs.",
    "This shows where an item has been attributed to an NHS number that has been verified by the Personal Demographics Service (PDS).",
    "The IMD Quintile of the patient, based on the location of their practice, where '1' is the 20% of areas with the highest deprivation score in the Index of Multiple Deprivation (IMD) from the English Indices of Deprivation 2019, and '5' is the 20% of areas with the lowest IMD deprivation score. Unknown values are where we have been unable to match the patient postcode to a postcode in the National Statistics Postcode Lookup (NSPL).",
    "The unique code used to refer to an Integrated Care Board (ICB).",
    "The name given to the Integrated Care Board (ICB) that a prescribing organisation belongs to. This is based upon NHSBSA administrative records, not geographical boundaries and more closely reflect the operational organisation of practices than other geographical data sources.",
    "The population estimate for the corresponding Mid-Year Population Year.",
    "The year in which population estimates were taken, required due to the presentation of this data in financial year format.",
    "",
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
  patient_identification_fy,
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
  "National",
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
  national_data_fy,
  14
)

#left align columns A to C
format_data(wb,
            "National",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "National",
            c("C", "D"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "National",
            c("E"),
            "right",
            "#,##0.00")

#### category data
# write data to sheet
write_sheet(
  wb,
  "Category",
  paste0(
    "Table 2: Dependency Forming Medicines - England 2015/16 to ",
    config$fy,
    " - Yearly totals split by Drug Category and identified patients"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  category_data_fy |> select(-`BNF Section Name`, -`BNF Section Code`),
  14
)

#left align columns A to C
format_data(wb,
            "Category",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Category",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Category",
            c("F"),
            "right",
            "#,##0.00")

#### ICB data
# write data to sheet
write_sheet(
  wb,
  "ICB",
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
  icb_data_fy,
  14
)

#left align columns A to C
format_data(wb,
            "ICB",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "ICB",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "ICB",
            c("G"),
            "right",
            "#,##0.00")

#### ICB category data
# write data to sheet
write_sheet(
  wb,
  "ICB_Category",
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
  icb_category_data_fy,
  14
)

#left align columns A to C
format_data(wb,
            "ICB_Category",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "ICB_Category",
            c("F", "G"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "ICB_Category",
            c("H"),
            "right",
            "#,##0.00")

#### National population data
# write data to sheet
write_sheet(
  wb,
  "National_Population",
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
  population_data_fy,
  14
)

#left align columns A to C
format_data(wb,
            "National_Population",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "National_Population",
            c("C", "D"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "National_Population",
            c("E"),
            "right",
            "#,##0.00")

#### National population category data
# write data to sheet
write_sheet(
  wb,
  "Category_Population",
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
  population_category_data_fy,
  14
)

#left align columns A to C
format_data(wb,
            "Category_Population",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Category_Population",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Category_Population",
            c("F"),
            "right",
            "#,##0.00")

# build cover sheet
accessibleTables::makeCoverSheet(
  paste0("Dependency Forming Medicines - England 2015/16 - ", config$fy),
  "Costs and Items",
  paste0("Publication date: ", config$publication_date),
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
