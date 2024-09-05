sheetNames <- c(
  "Patient_Identification",
  "National",
  "National_Population",
  "Category",
  "Category_Population",
  "Paragraph",
  "Chemical_Substance",
  "ICB",
  "ICB_Category",
  "Gender",
  "Gender_Category",
  "Age_Band",
  "Age_Band_Category",
  "Age_Band_Gender",
  "Age_Band_Gender_Category",
  "IMD",
  "IMD_Category"
)

wb <- accessibleTables::create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(
  "Age Band",
  "Drug Category",
  "Calendar Year",
  "Identified Patient",
  "IMD Quintile",
  "Integrated Care Board Code",
  "Integrated Care Board Name",
  "Mid-Year England Population Estimate",
  "Mid-Year Population Year",
  "Patient Gender",
  "Patients per 1,000 Population",
  "Total Items",
  "Total Net Ingredient Cost (GBP)",
  "Total Patients"
)

meta_descs <-
  c(
    "The age band of the patient as of the 30th September of the corresponding calendar year the drug was prescribed.",
    "The category of class of drug that can cause dependence.",
    "The calendar year to which the data belongs.",
    "This shows where an item has been attributed to an NHS number that has been verified by the Personal Demographics Service (PDS).",
    "The IMD Quintile of the patient, based on the location of their practice, where '1' is the 20% of areas with the highest deprivation score in the Index of Multiple Deprivation (IMD) from the English Indices of Deprivation 2019, and '5' is the 20% of areas with the lowest IMD deprivation score. Unknown values are where we have been unable to match the patient postcode to a postcode in the National Statistics Postcode Lookup (NSPL).",
    "The unique code used to refer to an Integrated Care Board (ICB).",
    "The name given to the Integrated Care Board (ICB) that a prescribing organisation belongs to. This is based upon NHSBSA administrative records, not geographical boundaries and more closely reflect the operational organisation of practices than other geographical data sources.",
    "The population estimate for the corresponding Mid-Year Population Year.",
    "The year in which population estimates were taken, required due to the presentation of this data in calendar year format.",
    "The gender of the patient as noted at the time the prescription was processed. This includes where the patient has been identified but the gender has not been recorded.",
    "(Total Identified Patients / Mid-Year England Population Estimate) * 1000.",
    "The number of prescription items dispensed. 'Items' is the number of times a product appears on a prescription form. Prescription forms include both paper prescriptions and electronic messages.",
    "Total Net Ingredient Cost is the amount that would be paid using the basic price of the prescribed drug or appliance and the quantity prescribed. Sometimes called the 'Net Ingredient Cost' (NIC). The basic price is given either in the Drug Tariff or is determined from prices published by manufacturers, wholesalers or suppliers. Basic price is set out in Parts 8 and 9 of the Drug Tariff. For any drugs or appliances not in Part 8, the price is usually taken from the manufacturer, wholesaler or supplier of the product. This is given in GBP (£).",
    "Where patients are identified via the flag, the number of patients that the data corresponds to. This will always be 0 where 'Identified Patient' = N."
    
  )

accessibleTables::create_metadata(wb,
                                  meta_fields,
                                  meta_descs)

# Patient_Identification --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Dependency-Forming Medicines - 2016 to ",
    config$cy,
    " - Proportion of items for which an NHS number was recorded (%) by drug category and calendar year"
  ),
  c(
    "1. The below proportions reflect the percentage of prescription items where a PDS verified NHS number was recorded."
  ),
  patient_identification_cy,
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

# National --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "National",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "4. Total costs and items may not be reconciled back to Prescribing Cost Analysis (PCA) publication figures as these figures are based around a 'prescribing view' of the data. This is where we use the drug or device that was prescribed to a patient, rather than the drug that was reimbursed to the dispenser to classify a prescription item. PCA uses a dispensing view where the inverse is true."
  ),
  national_data_cy,
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

# National population --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "National_Population",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Population totals by calendar year"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. ONS population estimates taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates."
  ),
  population_data_cy,
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

# Category --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Category",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by drug category"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  category_data_cy |> select(-`BNF Section Name`, -`BNF Section Code`),
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

# Category population --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Category_Population",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Population totals by calendar year and drug category"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. ONS population estimates taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates."
  ),
  population_category_data_cy,
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

# Paragraph --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Paragraph",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by BNF paragraph"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  paragraph_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Paragraph",
            c("A", "B", "C", "D", "E", "F"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Paragraph",
            c("G", "H"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Paragraph",
            c("I"),
            "right",
            "#,##0.00")

# Chemical substance --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Chemical_Substance",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by BNF chemical substance"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  chem_sub_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Chemical_Substance",
            c("A", "B", "C", "D", "E", "F", "G", "H"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Chemical_Substance",
            c("I", "J"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Chemical_Substance",
            c("K"),
            "right",
            "#,##0.00")

# ICB --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "ICB",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by Integrated Care Board"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "4. Integrated Care Boards (ICBs) succeeded sustainability and transformation plans (STPs) and replaced the functions of clinical commissioning groups (CCGs) in July 2022 with ICB sub locations replacing CCGs during the transition period of 2022/23."
  ),
  icb_data_cy,
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

# ICB category --------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "ICB_Category",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by Integrated Care Board and drug category"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients.",
    "4. Integrated Care Boards (ICBs) succeeded sustainability and transformation plans (STPs) and replaced the functions of clinical commissioning groups (CCGs) in July 2022 with ICB sub locations replacing CCGs during the transition period of 2022/23."
  ),
  icb_category_data_cy,
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


# Gender ------------------------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Gender",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by gender"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. It is possible for a patient to be marked as 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together.",
    "4. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  gender_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Gender",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Gender",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Gender",
            c("F"),
            "right",
            "#,##0.00")

# Gender category ---------------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Gender_Category",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by drug category and gender"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. It is possible for a patient to be marked as 'unknown' or 'indeterminate'. Due to the low number of patients that these two groups contain the NHSBSA has decided to group these classifications together.",
    "4. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  gender_category_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Gender_Category",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Gender_Category",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Gender_Category",
            c("G"),
            "right",
            "#,##0.00")

# Age band ----------------------------------------------------------------
# write data to sheet
write_sheet(
  wb,
  "Age_Band",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  age_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Age_Band",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Age_Band",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Age_Band",
            c("F"),
            "right",
            "#,##0.00")

# Age band category -------------------------------------------------------
#### age category  data
# write data to sheet
write_sheet(
  wb,
  "Age_Band_Category",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by drug category and age band"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  age_category_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Age_Band_Category",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Age_Band_Category",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Age_Band_Category",
            c("G"),
            "right",
            "#,##0.00")

# Age band gender ---------------------------------------------------------
#### age gender  data
# write data to sheet
write_sheet(
  wb,
  "Age_Band_Gender",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by age band and gender"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. These totals only include patients where both age and gender are known.",
    "4. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  age_gender_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Age_Band_Gender",
            c("A", "B", "C", "D"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Age_Band_Gender",
            c("E", "F"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Age_Band_Gender",
            c("G"),
            "right",
            "#,##0.00")


# Age band gender category ------------------------------------------------
#### age gender category data
# write data to sheet
write_sheet(
  wb,
  "Age_Band_Gender_Category",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by age, gender and drug category"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. These totals only include patients where both age and gender are known.",
    "4. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  age_gender_cat_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "Age_Band_Gender_Category",
            c("A", "B", "C", "D", "E"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "Age_Band_Gender_Category",
            c("F", "G"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "Age_Band_Gender_Category",
            c("H"),
            "right",
            "#,##0.00")

# IMD ---------------------------------------------------------------------
#### imd data
# write data to sheet
write_sheet(
  wb,
  "IMD",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by IMD quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. IMD Quintiles used are taken from the English Indices of Deprivation 2019 National Statistics publication.",
    "4. Where a patient's postcode has not been able to to be matched to NSPL, and the postcode of the prescribing practice is also not available or the patient has not been identified, the records are reported as 'unknown' IMD quintile.",
    "5. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  imd_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "IMD",
            c("A", "B"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "IMD",
            c("C", "D"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "IMD",
            c("E"),
            "right",
            "#,##0.00")

# IMD category ------------------------------------------------------------
#### imd category data
# write data to sheet
write_sheet(
  wb,
  "IMD_Category",
  paste0(
    "Dependency-Forming Medicines - England 2016 to ",
    config$cy,
    " - Calendar year totals by drug category and IMD quintile"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. Statistical disclosure control has been applied to cells containing fewer than 5 patients or items. These cells will appear blank.",
    "3. IMD Quintiles used are taken from the English Indices of Deprivation 2019 National Statistics publication.",
    "4. Where a patient's postcode has not been able to to be matched to NSPL, and the postcode of the prescribing practice is also not available or the patient has not been identified, the records are reported as 'unknown' IMD quintile.",
    "5. The patient counts shown in these statistics should only be analysed at the level at which they are presented. Adding together any patient counts is likely to result in an overestimate of the number of patients."
  ),
  imd_category_data_cy,
  14
)

#left align columns A to C
format_data(wb,
            "IMD_Category",
            c("A", "B", "C"),
            "left",
            "")

#right align columns D and E and round to whole numbers with thousand separator
format_data(wb,
            "IMD_Category",
            c("D", "E"),
            "right",
            "#,##0")

#right align column F and round to 2dp with thousand separator
format_data(wb,
            "IMD_Category",
            c("F"),
            "right",
            "#,##0.00")

# Cover sheet --------------------------------------------------
# build cover sheet
accessibleTables::makeCoverSheet(
  paste0("Dependency-Forming Medicines - England 2016 - ", config$cy),
  "Calendar Year Summary Statistics",
  paste0("Publication date: ", config$publication_date),
  wb,
  sheetNames,
  c(
    "Metadata",
    "Patient Identification Rates",
    "Calendar year totals",
    "Population totals by calendar year",
    "Calendar year totals by drug category",
    "Population totals by calendar year and drug category",
    "Calendar year totals by BNF paragraph",
    "Calendar year totals by BNF chemical substance",
    "Calendar year totals by Integrated Care Board",
    "Calendar year totals by Integrated Care Board and drug category",
    "Calendar year totals by gender",
    "Calendar year totals by drug category and gender",
    "Calendar year totals by age band",
    "Calendar year totals by drug category and age band",
    "Calendar year totals by age band and gender",
    "Calendar year totals by age, gender and drug category",
    "Calendar year totals by IMD quintile",
    "Calendar year totals by drug category and IMD quintile"
  ),
  c("Metadata", sheetNames)
)

#save file into outputs folder
openxlsx::saveWorkbook(wb,
                       "outputs/dfm_2023_costs_and_items_v001.xlsx",
                       overwrite = TRUE)
