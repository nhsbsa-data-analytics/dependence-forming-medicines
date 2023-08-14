# Fact table creation -----------------------------------------------------
#category
create_fact_category <- function(con,
                                 from = 201504L,
                                 to = 202203L) {
  # Build tdim ---------------------------------------------------------
  tdim <- dplyr::tbl(con,
                     from = dbplyr::in_schema("DIM", "YEAR_MONTH_DIM")) |>
    select(FINANCIAL_YEAR, YEAR_MONTH) |>
    filter(YEAR_MONTH >= from,
           YEAR_MONTH <= to)
  
  
  
  
  
  # Build porg ---------------------------------------------------------
  max_org <- dplyr::tbl(con,
                        from = dbplyr::in_schema("DIM", "HS_DY_LEVEL_5_FLAT_DIM")) |>
    dplyr::filter(HS_CTRY_OU == 1,
                  DATA_ADDED_BY_DENTAL == 'N',
                  LVL_5_OUPDT > 0) |>
    dplyr::group_by(LVL_5_OUPDT,
                    LVL_5_OU) |>
    dplyr::filter(YEAR_MONTH == max(YEAR_MONTH, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(M_YEAR_MONTH = YEAR_MONTH,
                  LVL_5_OUPDT,
                  LVL_5_OU)
  
  
  ## get latest postcode of closed practices set to NULL. cleanse postcode and set to null for unidentifieds (lvl_5_ou < 0)
  
  pcd <- dplyr::tbl(con,
                    from = dbplyr::in_schema("DIM", "HS_DY_LEVEL_5_FLAT_DIM")) |>
    dplyr::select(
      YEAR_MONTH,
      HS_CTRY_OU,
      DATA_ADDED_BY_DENTAL,
      LVL_5_OUPDT,
      LVL_5_OU,
      LVL_5_LTST_ALT_CDE,
      LVL_5_HIST_POSTCODE
    ) |>
    dplyr::filter(HS_CTRY_OU == 1,
                  DATA_ADDED_BY_DENTAL == 'N',
                  LVL_5_OUPDT > 0) |>
    #dplyr::mutate(LVL_5_HIST_POSTCODE = upper(REGEXP_REPLACE(LVL_5_HIST_POSTCODE, ' ', ''))) |>
    #dplyr::mutate(LVL_5_HIST_POSTCODE = case_when(is.na(nchar(LVL_5_HIST_POSTCODE)) ~ "0000000000",
    #                                              TRUE ~ LVL_5_HIST_POSTCODE))|>
    #dbplyr::window_order(YEAR_MONTH) |>
    dplyr::group_by(LVL_5_OUPDT, LVL_5_OU, LVL_5_LTST_ALT_CDE) |>
    dplyr::mutate(LVL_5_CUR_POSTCODE = case_when(
      LVL_5_OU < 0 ~ NA,
      TRUE ~ sql(
        "LAST_VALUE(UPPER(REPLACE(LVL_5_HIST_POSTCODE, ' ', '')))  IGNORE NULLS  OVER(PARTITION BY LVL_5_OUPDT, LVL_5_OU, LVL_5_LTST_ALT_CDE ORDER BY  YEAR_MONTH)"
      )
    )) |>
    dplyr::ungroup() |>
    dplyr::select(YEAR_MONTH,
                  LVL_5_OUPDT,
                  LVL_5_OU,
                  LVL_5_CUR_POSTCODE,
                  LVL_5_HIST_POSTCODE) 
  
  cur_pcd <- pcd |>
    inner_join(
      max_org,
      by = c(
        "LVL_5_OUPDT" = "LVL_5_OUPDT",
        "YEAR_MONTH" = "M_YEAR_MONTH",
        "LVL_5_OU" = "LVL_5_OU"
      )
    ) |>
    select(
      LVL_5_OUPDT,
      LVL_5_OU,
      LVL_5_CUR_POSTCODE
    ) 
  
  # ons lookup --------------------------------------------------------------
  ons <- porg <- dplyr::tbl(con,
                            from = dbplyr::in_schema("GRALI", "ONS_NSPL_MAY_23")) |>
    select(
      PCD,
      LSOA11,
      IMD_DECILE,
      IMD_RANK
    ) |>
    mutate(
      PCD = sql("UPPER(REPLACE(PCD, ' ', '')) ")
    )
  
  porg <- dplyr::tbl(con,
                     from = dbplyr::in_schema("DIM", "CUR_EP_LEVEL_5_FLAT_DIM")) |>
    mutate(
      ICB_NAME = case_when(
        CUR_AREA_LTST_CLSD == "Y" ~ "UNKNOWN ICB",
        CUR_AREA_TEAM_LTST_NM %in% c(
          'ENGLISH/WELSH DUMMY DENTAL',
          'UNIDENTIFIED DEPUTISING SERVICES',
          'UNIDENTIFIED DOCTORS'
        ) ~ "UNKNOWN ICB",
        TRUE ~ CUR_FRMTTD_AREA_TEAM_LTST_NM
      ),
      ICB_CODE = case_when(
        CUR_AREA_LTST_CLSD == "Y" ~ "-",
        CUR_AREA_TEAM_LTST_NM %in% c(
          'ENGLISH/WELSH DUMMY DENTAL',
          'UNIDENTIFIED DEPUTISING SERVICES',
          'UNIDENTIFIED DOCTORS'
        ) ~ "-",
        TRUE ~ CUR_AREA_TEAM_LTST_ALT_CDE
      ),
      REGION_NAME = case_when(
        CUR_REGION_LTST_CLSD == "Y" ~ "UNKNOWN REGION",
        CUR_REGION_LTST_NM %in% c(
          'ENGLISH/WELSH DUMMY DENTAL',
          'UNIDENTIFIED DEPUTISING SERVICES',
          'UNIDENTIFIED DOCTORS'
        ) ~ "UNKNOWN REGION",
        TRUE ~ CUR_FRMTTD_REGION_LTST_NM
      ),
      REGION_CODE = case_when(
        CUR_REGION_LTST_CLSD == "Y" ~ "-",
        CUR_REGION_LTST_NM %in% c(
          'ENGLISH/WELSH DUMMY DENTAL',
          'UNIDENTIFIED DEPUTISING SERVICES',
          'UNIDENTIFIED DOCTORS'
        ) ~ "-",
        TRUE ~ CUR_REGION_LTST_ALT_CDE
      )
    ) |>
    select(
      LVL_5_OUPDT,
      LVL_5_OU,
      ICB_NAME,
      ICB_CODE,
      REGION_NAME,
      REGION_CODE
    ) |>
    inner_join(
      cur_pcd,
      by = c(
        "LVL_5_OUPDT" = "LVL_5_OUPDT",
        "LVL_5_OU" = "LVL_5_OU"
      )
    ) |> 
    left_join(
      ons,
      by = c(
        "LVL_5_CUR_POSTCODE"  = "PCD"
      )
    )
  
  # Build drug ---------------------------------------------------------
  drug <- dplyr::tbl(con,
                     from = dbplyr::in_schema("DIM", "CDR_EP_DRUG_BNF_DIM")) |>
    filter(
      BNF_CHEMICAL_SUBSTANCE %in% c(
        "0408010G0",
        "0408010AE",
        "0401010T0",
        "0401010L0",
        "0401010N0",
        "0401020E0",
        "0401020K0",
        "0401020P0",
        "0401020T0",
        "0401010R0",
        "0401010P0",
        "0401020A0",
        "040702020",
        "040702040",
        "040702050",
        "0407010F0",
        "0407010N0",
        "0407020A0",
        "0407020AB",
        "0407020AD",
        "0407020AF",
        "0407020AG",
        "0407020AH",
        "0407020B0",
        "0407020C0",
        "0407020D0",
        "0407020G0",
        "0407020H0",
        "0407020K0",
        "0407020L0",
        "0407020M0",
        "0407020P0",
        "0407020Q0",
        "0407020T0",
        "0407020U0",
        "0407020V0",
        "0407020Z0",
        "0401010Z0",
        "0401010Y0",
        "0401010W0",
        "0403020Q0",
        "0403020M0",
        "0403020K0",
        "0403040X0",
        "0403010L0",
        "0403010S0",
        "0403040S0",
        "0403010Y0",
        "0403040Y0",
        "0403010V0",
        "0403010X0",
        "0403030L0",
        "0403030E0",
        "0403030D0",
        "0403040W0",
        "0403010B0",
        "0403030Q0",
        "0403020H0",
        "0403010J0",
        "0403010F0",
        "0403040R0",
        "0403010T0",
        "0403010C0",
        "0403040T0",
        "0403030P0",
        "0403010N0",
        "0403040AB",
        "0403040F0",
        "0403030Z0",
        "0403040U0",
        "0403010R0",
        "0403030X0",
        "0403040Z0"
        
      )
    ) |>
    mutate(
      CATEGORY = case_when(
        BNF_CHEMICAL_SUBSTANCE %in% c("0408010G0",
                                      "0408010AE")  ~ "GABAPENTINOIDS",
        BNF_CHEMICAL_SUBSTANCE %in% c(
          "0401010T0",
          "0401010L0",
          "0401010N0",
          "0401020E0",
          "0401020K0",
          "0401020P0",
          "0401020T0",
          "0401010R0",
          "0401010P0",
          "0401020A0"
        ) ~ "BENZODIAZEPINES",
        BNF_CHEMICAL_SUBSTANCE %in% c(
          "040702020",
          "040702040",
          "040702050",
          "0407010F0",
          "0407010N0",
          "0407020A0",
          "0407020AB",
          "0407020AD",
          "0407020AF",
          "0407020AG",
          "0407020AH",
          "0407020B0",
          "0407020C0",
          "0407020D0",
          "0407020G0",
          "0407020H0",
          "0407020K0",
          "0407020L0",
          "0407020M0",
          "0407020P0",
          "0407020Q0",
          "0407020T0",
          "0407020U0",
          "0407020V0",
          "0407020Z0"
        ) ~ "OPIOIDS",
        BNF_CHEMICAL_SUBSTANCE %in% c("0401010Z0",
                                      "0401010Y0",
                                      "0401010W0") ~ "Z-DRUGS",
        BNF_CHEMICAL_SUBSTANCE %in% c(
          "0403020Q0",
          "0403020M0",
          "0403020K0",
          "0403040X0",
          "0403010L0",
          "0403010S0",
          "0403040S0",
          "0403010Y0",
          "0403040Y0",
          "0403010V0",
          "0403010X0",
          "0403030L0",
          "0403030E0",
          "0403030D0",
          "0403040W0",
          "0403010B0",
          "0403030Q0",
          "0403020H0",
          "0403010J0",
          "0403010F0",
          "0403040R0",
          "0403010T0",
          "0403010C0",
          "0403040T0",
          "0403030P0",
          "0403010N0",
          "0403040AB",
          "0403040F0",
          "0403030Z0",
          "0403040U0",
          "0403010R0",
          "0403030X0",
          "0403040Z0"
        ) ~ "ANTIDEPRESSANTS"
      )
    ) |>
    select(
      YEAR_MONTH,
      RECORD_ID,
      CATEGORY,
      GENENRIC_BNF_NAME = GEN_PRESENTATION_BNF_DESCR,
      GENERIC_BNF_CODE = GENERIC_BNF_CODE,
      BNF_NAME = PRESENTATION_BNF_DESCR,
      BNF_CODE = PRESENTATION_BNF,
      CHEM_SUB_NAME = CHEMICAL_SUBSTANCE_BNF_DESCR,
      CHEM_SUB_CODE = BNF_CHEMICAL_SUBSTANCE,
      PARAGRAPH_NAME = PARAGRAPH_DESCR,
      PARAGRAPH_CODE = BNF_PARAGRAPH,
      SECTION_NAME = SECTION_DESCR,
      SECTION_CODE = BNF_SECTION,
      UNIT_OF_MEASURE = VMPP_UOM
    )
  
  # Build age ---------------------------------------------------------
  age <- dplyr::tbl(con,
                    from = dbplyr::in_schema("DIM", "AGE_DIM")) |>
    select(AGE,
           DALL_5YR_BAND)
  
  # Build imd ---------------------------------------------------------
  imd <- dplyr::tbl(con,
                    from = dbplyr::in_schema("GRALI", "ONS_NSPL_MAY_23")) |>
    select(LSOA11,
           IMD_DECILE,
           IMD_RANK) |>
    #using distinct to remove duplicate rows
    distinct()
  
  # Build fact ---------------------------------------------------------
  fact <- dplyr::tbl(con,
                     from = dbplyr::in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT_AV")) |>
    #regular exclusions
    filter(
      PAY_DA_END == "N",
      # excludes disallowed items
      PAY_ND_END == "N",
      # excludes not dispensed items
      PAY_RB_END == "N",
      # excludes referred back items
      CD_REQ == "N",
      # excludes controlled drug requisitions
      OOHC_IND == 0L,
      # excludes out of hours dispensing
      PRIVATE_IND == 0L,
      # excludes private dispensers
      IGNORE_FLAG == "N",
      # excludes LDP dummy forms
      PRESC_TYPE_PRNT %NOT IN% c(8L, 54L)
    ) |>
    select(
      YEAR_MONTH,
      PRESC_TYPE_PRNT,
      PRESC_ID_PRNT,
      CALC_PREC_DRUG_RECORD_ID,
      PDS_DOB,
      PATIENT_LSOA_CODE,
      PATIENT_ID,
      PDS_GENDER,
      ITEM_CALC_PAY_QTY,
      ITEM_COUNT,
      ITEM_PAY_DR_NIC,
      EPS_PART_DATE
    ) |>
    group_by(
      YEAR_MONTH,
      PRESC_TYPE_PRNT,
      PRESC_ID_PRNT,
      CALC_PREC_DRUG_RECORD_ID,
      PDS_DOB,
      PATIENT_LSOA_CODE,
      PATIENT_ID,
      PDS_GENDER,
      EPS_PART_DATE
    ) |>
    summarise(
      TOTAL_QTY = sum(ITEM_CALC_PAY_QTY, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  query <- fact |>
    inner_join(tdim,
               by = c("YEAR_MONTH" = "YEAR_MONTH")) |>
    inner_join(porg,
               by = c("PRESC_TYPE_PRNT" = "LVL_5_OUPDT",
                      "PRESC_ID_PRNT" = "LVL_5_OU")) |>
    inner_join(drug,
               by = c(
                 "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID",
                 "YEAR_MONTH" = "YEAR_MONTH"
               )) |>
    #	calculate age of patient using PDS_DOB at 30th Sept of given year
    mutate(
      CALC_AGE = sql(
        "nvl(trunc((to_number(substr(financial_year,1,4)||'0930') - to_number(to_char(pds_dob,'YYYYMMDD')))/10000),-1)"
      )
    ) |>
    
    # create identified flag using pds
    mutate(PATIENT_IDENTIFIED = sql("case when	pds_dob	is	null	and	pds_gender	=	0	then	'N'	else	'Y'	end")) |>
    
    inner_join(age,
               by = c("CALC_AGE" = "AGE")) |>
    #pull forward last observation of PATIENT_LSOA_CODE to account for null data
    mutate(
      PATIENT_LSOA_CODE = sql(
        "last_value(PATIENT_LSOA_CODE ignore nulls) over (partition by PATIENT_ID order by YEAR_MONTH, EPS_PART_DATE, PATIENT_LSOA_CODE NULLS last rows between unbounded preceding and current row)"
      )
    ) |>
    
    left_join(imd,
              by = c("PATIENT_LSOA_CODE" = "LSOA11")) |>
    select(
      FINANCIAL_YEAR,
      REGION_NAME,
      REGION_CODE,
      ICB_NAME,
      ICB_CODE,
      CATEGORY,
      SECTION_NAME,
      SECTION_CODE,
      PARAGRAPH_NAME,
      PARAGRAPH_CODE,
      CHEM_SUB_NAME,
      CHEM_SUB_CODE,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      IMD_DECILE,
      IMD_RANK,
      PDS_GENDER,
      DALL_5YR_BAND,
      TOTAL_QTY,
      ITEM_COUNT,
      ITEM_PAY_DR_NIC
    ) |>
    mutate(
      PDS_GENDER = case_when(PDS_GENDER == 1 ~ "M",
                             PDS_GENDER == 2 ~ "F",
                             TRUE ~ "U"),
      DALL_5YR_BAND = case_when(is.na(DALL_5YR_BAND) ~ "Unknown",
                                TRUE ~ DALL_5YR_BAND)
    ) |>
    group_by(
      FINANCIAL_YEAR,
      REGION_NAME,
      REGION_CODE,
      ICB_NAME,
      ICB_CODE,
      CATEGORY,
      SECTION_NAME,
      SECTION_CODE,
      PARAGRAPH_NAME,
      PARAGRAPH_CODE,
      CHEM_SUB_NAME,
      CHEM_SUB_CODE,
      PATIENT_ID,
      PATIENT_IDENTIFIED,
      IMD_DECILE,
      IMD_RANK,
      PDS_GENDER,
      DALL_5YR_BAND
    ) |>
    summarise(
      TOTAL_QTY = sum(TOTAL_QTY, na.rm = T),
      ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
      .groups = "drop"
    )
  
  # drop time dimension if exists
  exists <- con |>
    DBI::dbExistsTable(name = "DFM_FACT_CAT_DIM")
  # Drop any existing table beforehand
  if (exists) {
    con |>
      DBI::dbRemoveTable(name = "DFM_FACT_CAT_DIM")
  }
  
  #build table
  query |>
    compute("DFM_FACT_CAT_DIM",
            analyze = FALSE,
            temporary = FALSE)
  
}

# Info boxes -------------------------------------------------------------
infoBox_border <- function(header = "Header here",
                           text = "More text here",
                           backgroundColour = "#ccdff1",
                           borderColour = "#005EB8",
                           width = "31%",
                           fontColour = "black") {
  #set handling for when header is blank
  display <- "block"
  
  if (header == "") {
    display <- "none"
  }
  
  paste(
    "<div class='infobox_border' style = 'border: 1px solid ",
    borderColour,
    "!important;
  border-left: 5px solid ",
  borderColour,
  "!important;
  background-color: ",
  backgroundColour,
  "!important;
  padding: 10px;
  width: ",
  width,
  "!important;
  display: inline-block;
  vertical-align: top;
  flex: 1;
  height: 100%;'>
  <h4 style = 'color: ",
  fontColour,
  ";
  font-weight: bold;
  font-size: 18px;
  margin-top: 0px;
  margin-bottom: 10px;
  display: ",
  display,
  ";'>",
  header,
  "</h4>
  <p style = 'color: ",
  fontColour,
  ";
  font-size: 16px;
  margin-top: 0px;
  margin-bottom: 0px;'>",
  text,
  "</p>
</div>"
  )
}

infoBox_no_border <- function(header = "Header here",
                              text = "More text here",
                              backgroundColour = "#005EB8",
                              width = "31%",
                              fontColour = "white") {
  #set handling for when header is blank
  display <- "block"
  
  if (header == "") {
    display <- "none"
  }
  
  paste(
    "<div class='infobox_no_border',
    style = 'background-color: ",
    backgroundColour,
    "!important;padding: 10px;
    width: ",
    width,
    ";
    display: inline-block;
    vertical-align: top;
    flex: 1;
    height: 100%;'>
  <h4 style = 'color: ",
  fontColour,
  ";
  font-weight: bold;
  font-size: 18px;
  margin-top: 0px;
  margin-bottom: 10px;
  display: ",
  display,
  ";'>",
  header,
  "</h4>
  <p style = 'color: ",
  fontColour,
  ";
  font-size: 16px;
  margin-top: 0px;
  margin-bottom: 0px;'>",
  text,
  "</p>
</div>"
  )
}

#-----------------------------------------

#MiSC
round_any <-
  function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

# ------------------

### CSV Download button
get_download_button <-
  function(data = data,
           title = "Download chart data",
           filename = "data") {
    dt <- datatable(
      data,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        searching = FALSE,
        paging = TRUE,
        bInfo = FALSE,
        pageLength = 1,
        dom = '<"datatable-wrapper"B>',
        buttons = list(
          list(
            extend = 'csv',
            text = title,
            filename = filename,
            className = "nhs-button-style"
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().node()).css('visibility', 'collapse');",
          "}"
        )
      )
    )
    
    return(dt)
  }
#---------------
