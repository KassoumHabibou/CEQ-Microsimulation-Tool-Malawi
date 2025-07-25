
################################################################################
######### CEQ initial data wrangling file for MALAWI ###########################
################################################################################

# Data wrangling

######################## Importing library and external files ##################
### List of required packages
required_packages <- c("tidyverse", "dplyr","haven","here","DescTools","labelled",
                       "survey","wINEQ","Hmisc","labelled")

### Check if packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

### Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

### Load all packages
lapply(setdiff(required_packages,"plyr"), library, character.only = TRUE)

# Remove all objects
rm(list = ls())

# Set vector size to maximum value
mem.maxVSize(vsize = Inf)
################################################################################
######################## Importing the datasets ################################
# Read the stata file containing the output data into

## Set file-paths
data_folder <- "/input/Data"
output_folder <- "/output"

# Cleaning shapefiles (for map) 
mlw_bound <- read_sf(paste0(here(),"/input/Shapefile/gadm41_MWI_shp/gadm41_MWI_1.shp"))

correspondance_table <- readxl::read_excel("input/Shapefile/Correspondance table.xlsx", 
                                           col_types = c("text", "text", "text", "numeric", "text", 
                                                         "text", "text", "text", "text"))

hh_data_path  <- paste0(data_folder,"/Household Surveys/IHS5 2019-20/Household")
int_data_path <- paste0(data_folder, "/Intermediate")
pdata_path <- paste0(output_folder, "/Output")
agri_data_path <- paste0(data_folder, "/Household Surveys/IHS5 2019-20/Agriculture")
fish_data_path <- paste0(data_folder, "/Household Surveys/IHS5 2019-20/Fisheries")
#dofile_path <- paste0(data_folder, "Script/income_aggregates")
script_path <- paste0(data_folder, "/Script")


######################## Importing data to merge ###############################
## Importing for merge
agric_subsidy <-  read_dta(paste0(here(),int_data_path, "/final/agric_subsidy.dta"))
corp_tax <-  read_dta(paste0(here(),int_data_path, "/final/corp_tax.dta"))
education <-  read_dta(paste0(here(),int_data_path, "/final/education.dta"))
elec_subsidy <-  read_dta(paste0(here(),int_data_path, "/final/Elec_subsidy.dta"))
agric_subsidy <-  read_dta(paste0(here(),int_data_path, "/final/agric_subsidy.dta"))
fuel_subsidy <-  read_dta(paste0(here(),int_data_path, "/final/Fuel_subsidy.dta"))
health <-  read_dta(paste0(here(),int_data_path, "/final/health.dta"))
indirect_tax <-  read_dta(paste0(here(),int_data_path, "/final/indirect_tax.dta"))
paye_tax <-  read_dta(paste0(here(),int_data_path, "/final/PAYE_tax.dta"))
users_fee <-  read_dta(paste0(here(),int_data_path, "/final/users_fee.dta"))
yc <-  read_dta(paste0(here(),int_data_path, "/final/yc.dta"))
yd <-  read_dta(paste0(here(),int_data_path, "/final/yd.dta"))
yf <-  read_dta(paste0(here(),int_data_path, "/final/yf.dta"))
yg <-  read_dta(paste0(here(),int_data_path, "/final/yg.dta"))
yn <-  read_dta(paste0(here(),int_data_path, "/final/yn.dta"))
yp <-  read_dta(paste0(here(),int_data_path, "/final/yp.dta"))

## To get residence and region level informations
HH_MOD_A <- read_dta(paste0(here(),int_data_path, "/final/HH_MOD_A_FILT.dta"))
HH_MOD_A <- HH_MOD_A %>% select(case_id, HHID, ea_id, district, reside)

HH_MOD_A$reside <- labelled::unlabelled(HH_MOD_A$reside) %>% as.character()
HH_MOD_A <- HH_MOD_A %>% mutate(reside = recode(reside,
  RURAL = "Rural",
  URBAN = 'Urban'
)) %>% 
  rename(hhid= HHID)

# 
# before_data <- HH_MOD_A %>% 
#   left_join(yc, by=c("hhid"), multiple = "all") %>% 
#   left_join(yd,by=c("hhid","pid")) %>% 
#   left_join(yf,by=c("hhid","pid")) %>% 
#   left_join(yg, by=c("hhid","pid")) %>% 
#   left_join(yn,by=c("hhid","pid")) %>% 
#   left_join(yp,by=c("hhid","pid")) %>% 
#   left_join(agric_subsidy,by=c("hhid")) %>% 
#   left_join(corp_tax,by=c("hhid","pid")) %>% 
#   left_join(education,by=c("hhid","pid")) %>% 
#   left_join(elec_subsidy,by=c("hhid","pid")) %>% 
#   left_join(fuel_subsidy,by=c("hhid")) %>% 
#   left_join(health,by=c("hhid","pid")) %>% 
#   left_join(indirect_tax,by=c("hhid")) %>% 
#   left_join(paye_tax,by=c("hhid","pid")) %>% 
#   left_join(users_fee,by=c("hhid","pid"))  

baseline_data <- HH_MOD_A %>% 
  left_join(yc, multiple = "all") %>% 
  left_join(yd) %>% 
  left_join(yf) %>% 
  left_join(yg) %>% 
  left_join(yn) %>% 
  left_join(yp) %>% 
  left_join(agric_subsidy) %>% 
  left_join(corp_tax) %>% 
  left_join(education) %>% 
  left_join(elec_subsidy) %>% 
  left_join(fuel_subsidy) %>% 
  left_join(health) %>% 
  left_join(indirect_tax) %>% 
  left_join(paye_tax) %>% 
  left_join(users_fee)  

# write_rds(before_data, paste0(here(),output_folder, "/Shiny Data/before.rds"))
# c(  "yd_pc", "ym_hh", "yp_hh", "yn_hh", "yg_hh", "yt_hh", "yd_hh", "yc_hh",
# "yf_hh", "coupons_hh", "i_coupons", "tfc_ben", "i_health", "illness_cat",
# "gov_expense_individual", "donor_expense_individual", "hh_expense_individual",
# "i_Health_hh", "gov_expense_hh", "donor_expense_hh", "hh_expense_hh",
# "illness_cat_hh", "gov_expense_HIV", "gov_expense_NCD", "gov_expense_Repr",
# "gov_expense_Mal", "gov_expense_TB", "gov_expense_NutritionalD", "gov_expense_Other",
# "donor_expense_HIV", "donor_expense_NCD", "donor_expense_Repr", "donor_expense_Mal",
# "donor_expense_TB", "donor_expense_NutritionalD", "donor_expense_Other",
# "hh_expense_HIV", "hh_expense_NCD", "hh_expense_Repr", "hh_expense_Mal",
# "hh_expense_TB", "hh_expense_NutritionalD", "hh_expense_Other", "edu_unit_cost_total",
# "i_edu_unit_cost_total", "i_Education_hh", "gov_edu_preschool", "gov_edu_primary",
# "gov_edu_secondary", "gov_edu_postsecondary", "gov_edu_tertiary", "gov_edu_masterphd",
# "at_school", "type_school", "Education_fee", "Health_fee", "Users_fee", "illness",
# "Users_fee_hh", "Education_fee_hh", "Health_fee_hh", "i_Usersfee", "i_Usersfee_hh",
# "i_Education_fee", "i_Education_fee_hh", "i_Health_fee", "i_Health_fee_hh",
# "dtx_payt_hh", "i_dtx_payt_hh", "p_tax", "i_ptax", "tran_1", "tran_2", "tran_32",
# "tran_31", "tran_4", "tran_8", "tran_91", "tran_13", "tran_11", "tran_12",
# "dct_hh", "dtr_nct_hh")
# write_rds(baseline_data, paste0(here(),output_folder, "/Shiny Data/baseline_data.rds"))


#baseline_data <- readRDS("~/Dropbox/PhD thesis/Internship World Bank/CEQ - Dashboard/CEQ - assessement tool/output/Shiny Data/baseline_data.rds")
source(paste0(here(),"/script/intermediate/1.PID_PAYE_FS.R"))

######################## Data wrangling ###############################

baseline_data <- curr_df %>% select(hhid, pid, i22_return_a) %>% right_join(baseline_data, by=c("hhid"="hhid", "pid"="pid"))


# 

baseline_data <- baseline_data[!is.na(baseline_data$hhsize),]
baseline_data <- baseline_data %>% mutate(across(everything(), ~ replace_na(.x, 0)))

######################## Computing some agregates concepts ##############################

# Creating household education variables
baseline_data <- baseline_data %>%
  group_by(hhid) %>%
  mutate(
    educ_preschool_hh = sum(gov_edu_preschool, na.rm = TRUE),
    educ_primary_hh = sum(gov_edu_primary, na.rm = TRUE),
    educ_secondary_hh = sum(gov_edu_secondary, na.rm = TRUE),
    educ_postsecondary_hh = sum(gov_edu_postsecondary, na.rm = TRUE),
    educ_tertiary_hh = sum(gov_edu_tertiary, na.rm = TRUE),
    educ_masterphd_hh = sum(gov_edu_masterphd, na.rm = TRUE)
  ) %>%
  ungroup()


# Creating household health variables
baseline_data <- baseline_data %>%
  group_by(hhid) %>%
  mutate(
    ## Government expenses
    health_gov_hiv_hh  = sum(gov_expense_HIV, na.rm = TRUE),
    health_gov_ncd_hh  = sum(gov_expense_NCD, na.rm = TRUE),
    health_gov_rep_hh  = sum(gov_expense_Repr, na.rm = TRUE),
    health_gov_mal_hh  = sum(gov_expense_Mal, na.rm = TRUE),
    health_gov_tb_hh   = sum(gov_expense_TB, na.rm = TRUE),
    health_gov_nut_hh  = sum(gov_expense_NutritionalD, na.rm = TRUE),
    health_gov_other_hh = sum(gov_expense_Other, na.rm = TRUE),
    
    ## Donor expenses
    health_donor_hiv_hh  = sum(donor_expense_HIV, na.rm = TRUE),
    health_donor_ncd_hh  = sum(donor_expense_NCD, na.rm = TRUE),
    health_donor_rep_hh  = sum(donor_expense_Repr, na.rm = TRUE),
    health_donor_mal_hh  = sum(donor_expense_Mal, na.rm = TRUE),
    health_donor_tb_hh   = sum(donor_expense_TB, na.rm = TRUE),
    health_donor_nut_hh  = sum(donor_expense_NutritionalD, na.rm = TRUE),
    health_donor_other_hh = sum(donor_expense_Other, na.rm = TRUE),
    
    ## Household (private) expenses
    health_hh_hiv_hh  = sum(hh_expense_HIV, na.rm = TRUE),
    health_hh_ncd_hh  = sum(hh_expense_NCD, na.rm = TRUE),
    health_hh_rep_hh  = sum(hh_expense_Repr, na.rm = TRUE),
    health_hh_mal_hh  = sum(hh_expense_Mal, na.rm = TRUE),
    health_hh_tb_hh   = sum(hh_expense_TB, na.rm = TRUE),
    health_hh_nut_hh  = sum(hh_expense_NutritionalD, na.rm = TRUE),
    health_hh_other_hh = sum(hh_expense_Other, na.rm = TRUE)
  ) %>%
  ungroup()

# Taxes by household
baseline_data <- baseline_data %>%
  group_by(hhid) %>%
  mutate(p_tax_hh = sum(p_tax, na.rm = TRUE)) %>%
  ungroup()

# Health and education expenditures by household
baseline_data <- baseline_data %>%
  mutate(
    health_hh = gov_expense_hh %>% structure(label="Health (household)"),
    educ_hh = rowSums(across(c(
      educ_preschool_hh, educ_primary_hh, educ_secondary_hh,
      educ_postsecondary_hh, educ_tertiary_hh, educ_masterphd_hh
    ), ~ replace_na(., 0))) %>% structure(label="Education (household)")
  )


#  Rename variables
baseline_data <- baseline_data %>%
  rename(
    dct_gov_hh = tran_11,
    dct_ngo_hh = tran_12,
    dct_fips_hh = coupons_hh,
    
    dtr_frmz_hh = tran_1,
    dtr_nfra_hh = tran_2,
    dtr_masaf_hh = tran_31,
    dtr_ffwk_hh = tran_32,
    dtr_ifwp_hh = tran_4,
    dtr_ses_hh = tran_8,
    dtr_tes_hh = tran_91,
    dtr_onc_hh = tran_13,
    
    dtx_PIT_hh = p_tax_hh,
    
    sub_electri_hh = sub_electri,
    sub_fuel_hh = tfc_hh
  )


# Select variables to keep
baseline_data <- baseline_data %>%
  select(hhid, pid, ea_id, region, district, reside, 
         dtr_nct_hh, dtr_frmz_hh, dtr_nfra_hh, dtr_masaf_hh, dtr_ffwk_hh,
         dtr_ifwp_hh, dtr_ses_hh, dtr_tes_hh, dtr_onc_hh,
         dct_hh, dct_gov_hh, dct_ngo_hh, dct_fips_hh,
         dtx_payt_hh, dtx_PIT_hh,
         region, hhsize, rexpagg, pcrexp, weight, popweight,
         yn_hh, yg_hh, yt_hh, yd_hh, yc_hh, yf_hh,
         sub_electri_hh, itx_vatx_hh, itx_excx_hh, sub_fuel_hh,
         educ_preschool_hh, educ_primary_hh, educ_secondary_hh,
         educ_postsecondary_hh, educ_tertiary_hh, educ_masterphd_hh,
         health_gov_hiv_hh, health_gov_ncd_hh, health_gov_rep_hh,
         health_gov_mal_hh, health_gov_tb_hh, health_gov_nut_hh, health_gov_other_hh,
         health_donor_hiv_hh, health_donor_ncd_hh, health_donor_rep_hh,
         health_donor_mal_hh, health_donor_tb_hh, health_donor_nut_hh, health_donor_other_hh,
         health_hh_hiv_hh, health_hh_ncd_hh, health_hh_rep_hh,
         health_hh_mal_hh, health_hh_tb_hh, health_hh_nut_hh, health_hh_other_hh,
         health_hh, educ_hh,i22_return_a, i_ptax,
         Users_fee_hh, Education_fee_hh, Health_fee_hh, pline_mod) 

# Remove missing values in hhid
baseline_data <- baseline_data[!is.na(baseline_data$weight),] 
  
# Taxes rate
baseline_data <- baseline_data %>%
  mutate(
    dtx_all_hh = (dtx_PIT_hh + dtx_payt_hh) %>% structure(label="All direct taxes paid, HH total"),
    dtr_all_hh = (dct_hh + dtr_nct_hh) %>% structure(label= "All direct transfers, HH total"), 
    sub_all_hh = (sub_electri_hh + sub_fuel_hh) %>% structure(label="All indirect subsidies, HH total"), 
    itx_all_hh = (itx_vatx_hh + itx_excx_hh) %>% structure(label="All indirect taxes, HH total")
  )



# ********************************************************************************
#   *						GENERATE INCOME VARIABLES
# ********************************************************************************


baseline_data <- baseline_data %>%
  mutate(
    # Replace and generate household income variables
    yg_hh = (yd_hh + dtx_all_hh) %>% structure(label = "Gross Income"),
    yn_hh =( yd_hh - dtr_all_hh) %>% structure(label = "Net Market Income")
  ) %>% 
  mutate(
    yp_hh = (yd_hh + dtx_all_hh - dtr_all_hh) %>% structure(label="Market Income plus pensions"),
    yc_hh = (yd_hh + sub_all_hh - itx_all_hh) %>% structure(label = "Consumable Income")
  ) %>% 
  mutate(
    yf_hh = (yd_hh + sub_all_hh - itx_all_hh + educ_hh + health_hh - Users_fee_hh) %>% structure(label = "Final Income")
  ) %>% 
  mutate(
    
    # Per capita variables
    yd_pc = (yd_hh / hhsize) %>% structure(label="Disposable Income (per capita)"),
    yg_pc = (yg_hh / hhsize) %>% structure(label="Gross Income (per capita)"),
    yn_pc = (yn_hh / hhsize) %>% structure(label="Net Market Income (per capita)"),
    yp_pc = (yp_hh / hhsize) %>% structure(label="Market Income plus pensions (per capita)"),
    yc_pc = (yc_hh / hhsize) %>% structure(label="Consumable Income (per capita)"),
    yf_pc = (yf_hh / hhsize) %>% structure(label="Final Income (per capita)")
  ) %>% 
  mutate(pline_mod_low = 656.7*365,
         pline_mod_middle = 1115*365)
# Source: https://databankfiles.worldbank.org/public/ddpext_download/poverty/987B9C90-CB9F-4D93-AE8C-750588BF00QA/current/Global_POVEQ_MWI.pdf
# Based on available historical data, the average exchange rate for the Malawian Kwacha to the US Dollar in 2019 was approximately 730 MWK to 1 USD.
write_rds(baseline_data, paste0(here(),output_folder, "/Shiny Data/baseline_data.rds"))


get_pov_indicator_bl <- function(curr_pline, curr_area, df) {
  
  if(curr_area == "Country"){
    curr_df <- df
  }else{
    curr_df <- df %>% 
      filter(reside==curr_area)
  }

  # Poverty Gap indicator as proportion of poverty line
  curr_df <- curr_df %>%
    mutate(
      # Individual poverty gap (how much income falls short of poverty line)
      pov_gap_yd_pc = pmax(0, !!sym(curr_pline) - yd_pc) / !!sym(curr_pline),
      pov_gap_yp_pc = pmax(0, !!sym(curr_pline) - yp_pc) / !!sym(curr_pline),
      pov_gap_yg_pc = pmax(0, !!sym(curr_pline) - yg_pc) / !!sym(curr_pline),
      pov_gap_yn_pc = pmax(0, !!sym(curr_pline) - yn_pc) / !!sym(curr_pline),
      pov_gap_yc_pc = pmax(0, !!sym(curr_pline) - yc_pc) / !!sym(curr_pline),
      pov_gap_yf_pc = pmax(0, !!sym(curr_pline) - yf_pc) / !!sym(curr_pline)
  
    ) %>% 
    mutate(
      
      # Squared poverty gap (for FGT2 measure)
      pov_gap_yd_pc_sqrd = (pov_gap_yd_pc)^2,
      pov_gap_yp_pc_sqrd = (pov_gap_yp_pc)^2,
      pov_gap_yg_pc_sqrd = (pov_gap_yg_pc)^2,
      pov_gap_yn_pc_sqrd = (pov_gap_yn_pc)^2,
      pov_gap_yc_pc_sqrd = (pov_gap_yc_pc)^2,
      pov_gap_yf_pc_sqrd = (pov_gap_yf_pc)^2
    )
  
  
  
  # Compute poverty headcount ratio
  poverty_headcount_ratio <- curr_df %>%
    dplyr::summarise(
      pov_rate_yp_pc  = sum(weight[yp_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yn_pc  = sum(weight[yn_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yg_pc  = sum(weight[yg_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yd_pc  = sum(weight[yd_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yc_pc  = sum(weight[yc_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yf_pc  = sum(weight[yf_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE)
    )
  
  # Compute number of poor 
  nbr_poor <- curr_df %>%
    dplyr::summarise(
      nbr_poor_yp_pc  = sum(weight[yp_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yn_pc  = sum(weight[yn_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yg_pc  = sum(weight[yg_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yd_pc  = sum(weight[yd_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yc_pc  = sum(weight[yc_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yf_pc  = sum(weight[yf_pc  < !!sym(curr_pline)], na.rm = TRUE)
    )
  
  # Poverty Gap
  poverty_gap <- tibble(
    pov_gap_yp_pc  = weighted.mean(curr_df$pov_gap_yp_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yn_pc  = weighted.mean(curr_df$pov_gap_yn_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yg_pc  = weighted.mean(curr_df$pov_gap_yg_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yd_pc  = weighted.mean(curr_df$pov_gap_yd_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yc_pc  = weighted.mean(curr_df$pov_gap_yc_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yf_pc  = weighted.mean(curr_df$pov_gap_yf_pc,  curr_df$weight, na.rm = TRUE)*100
  )
  
  # Poverty severity index - squared poverty gap measure
  poverty_sev <- tibble(
    pov_sev_yp_pc  = weighted.mean(curr_df$pov_gap_yp_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yn_pc  = weighted.mean(curr_df$pov_gap_yn_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yg_pc  = weighted.mean(curr_df$pov_gap_yg_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yd_pc  = weighted.mean(curr_df$pov_gap_yd_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yc_pc  = weighted.mean(curr_df$pov_gap_yc_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yf_pc  = weighted.mean(curr_df$pov_gap_yf_pc_sqrd,  curr_df$weight, na.rm = TRUE)
  )
  
  #browser()
  # Poverty measurements matrice
  summary_tab <- as.data.frame(cbind(t(poverty_headcount_ratio), t(nbr_poor), t(poverty_gap), t(poverty_sev), 
                                     Income = c("Market Income plus pensions", "Net Market Income",
                                                "Gross Income","Disposable Income","Consumable Income","Final Income"))) %>% 
    pivot_longer(cols = starts_with("V"),
                 names_to = "Parameter",
                 values_to = "Pre-reform",
                 values_drop_na = TRUE) %>% 
    mutate(Parameter = recode(Parameter,
                              V1 = "Rate of poverty",
                              V2 = "Number of poor",
                              V3 = "Poverty gap",
                              V4 = "Poverty severity"),
           `Pre-reform` = round(as.numeric(`Pre-reform`),2),
           Area = curr_area,
           `Poverty line` = curr_pline) 

  return(summary_tab)
}


lst_pline = c("pline_mod", "pline_mod_low", "pline_mod_middle")
lst_area = c("Country", "Rural", "Urban")

# Use map_dfr to iterate over Area and Pline to estimate different indicators
baseline_pov_estimates <- map_dfr(lst_pline, function(curr_pline) {    
  tempResults <- map_dfr(lst_area, function(curr_area) {
    bind_rows(
      get_pov_indicator_bl(curr_pline, curr_area, baseline_data) 
    )
  })
}) 



# Gini results
# gini_results <- list(
#   yp_pc  = wINEQ::Gini(baseline_data$yp_pc, W = baseline_data$weight, fast = TRUE, rounded.weights = TRUE),
#   yg_pc  = wINEQ::Gini(baseline_data$yg_pc, W = baseline_data$weight, fast = TRUE, rounded.weights = TRUE),
#   yn_pc  = wINEQ::Gini(baseline_data$yn_pc, W = baseline_data$weight, fast = TRUE, rounded.weights = TRUE),
#   yd_pc  = wINEQ::Gini(baseline_data$yd_pc, W = baseline_data$weight, fast = TRUE, rounded.weights = TRUE),
#   yc_pc  = wINEQ::Gini(baseline_data$yc_pc, W = baseline_data$weight, fast = TRUE, rounded.weights = TRUE),
#   yf_pc  = wINEQ::Gini(baseline_data$yf_pc, W = baseline_data$weight, fast = TRUE, rounded.weights = TRUE)
# )

baseline_data <- baseline_data %>%
  mutate(
    # Education
    educ_preschool_hh     = educ_preschool_hh     %>% structure(label = "Government spending on preschool education, at the household level"),
    educ_primary_hh       = educ_primary_hh       %>% structure(label = "Government spending on primary education, at the household level"),
    educ_secondary_hh     = educ_secondary_hh     %>% structure(label = "Government spending on secondary education, at the household level"),
    educ_postsecondary_hh = educ_postsecondary_hh %>% structure(label = "Government spending on post-secondary education, at the household level"),
    educ_tertiary_hh      = educ_tertiary_hh      %>% structure(label = "Government spending on tertiary education, at the household level"),
    educ_masterphd_hh     = educ_masterphd_hh     %>% structure(label = "Government spending on master's and PhD education, at the household level"),
    
    # Health - Government
    health_gov_hiv_hh     = health_gov_hiv_hh     %>% structure(label = "Health spending on HIV/AIDS by government, at the household level"),
    health_gov_ncd_hh     = health_gov_ncd_hh     %>% structure(label = "Health spending on NCDs by government, at the household level"),
    health_gov_rep_hh     = health_gov_rep_hh     %>% structure(label = "Health spending on reproductive health by government, at the household level"),
    health_gov_mal_hh     = health_gov_mal_hh     %>% structure(label = "Health spending on malaria by government, at the household level"),
    health_gov_tb_hh      = health_gov_tb_hh      %>% structure(label = "Health spending on tuberculosis by government, at the household level"),
    health_gov_nut_hh     = health_gov_nut_hh     %>% structure(label = "Health spending on nutritional deficiencies by government, at the household level"),
    health_gov_other_hh   = health_gov_other_hh   %>% structure(label = "Health spending on other conditions by government, at the household level"),
    
    # Health - Donors
    health_donor_hiv_hh   = health_donor_hiv_hh   %>% structure(label = "Health spending on HIV/AIDS by donors, at the household level"),
    health_donor_ncd_hh   = health_donor_ncd_hh   %>% structure(label = "Health spending on NCDs by donors, at the household level"),
    health_donor_rep_hh   = health_donor_rep_hh   %>% structure(label = "Health spending on reproductive health by donors, at the household level"),
    health_donor_mal_hh   = health_donor_mal_hh   %>% structure(label = "Health spending on malaria by donors, at the household level"),
    health_donor_tb_hh    = health_donor_tb_hh    %>% structure(label = "Health spending on tuberculosis by donors, at the household level"),
    health_donor_nut_hh   = health_donor_nut_hh   %>% structure(label = "Health spending on nutritional deficiencies by donors, at the household level"),
    health_donor_other_hh = health_donor_other_hh %>% structure(label = "Health spending on other conditions by donors, at the household level"),
    
    # Health - Households
    health_hh_hiv_hh      = health_hh_hiv_hh      %>% structure(label = "Health spending on HIV/AIDS by households, at the household level"),
    health_hh_ncd_hh      = health_hh_ncd_hh      %>% structure(label = "Health spending on NCDs by households, at the household level"),
    health_hh_rep_hh      = health_hh_rep_hh      %>% structure(label = "Health spending on reproductive health by households, at the household level"),
    health_hh_mal_hh      = health_hh_mal_hh      %>% structure(label = "Health spending on malaria by households, at the household level"),
    health_hh_tb_hh       = health_hh_tb_hh       %>% structure(label = "Health spending on tuberculosis by households, at the household level"),
    health_hh_nut_hh      = health_hh_nut_hh      %>% structure(label = "Health spending on nutritional deficiencies by households, at the household level"),
    health_hh_other_hh    = health_hh_other_hh    %>% structure(label = "Health spending on other conditions by households, at the household level")
  )



# Compute deciles using weighted quantiles
baseline_data <- baseline_data %>%
  mutate(
    
    # Weighted decile using Hmisc::wtd.quantile (numeric decile labels 1 to 10)
    decile = as.numeric(cut(
      yp_pc,
      breaks = c(-Inf, wtd.quantile(yp_pc, weights = weight, probs = seq(0.1, 1, 0.1), na.rm = TRUE)),
      labels = 1:10,
      include.lowest = TRUE,
      right = TRUE
    )),
    
    curr_level = "Baseline"
  )

# Saving 
write_rds(baseline_data, paste0(here(),output_folder, "/Shiny Data/baseline_data.rds"))
write_rds(baseline_pov_estimates, paste0(here(),output_folder, "/Shiny Data/baseline_pov_estimates.rds"))

######################## Geospatial analysis #########################################

get_geo_pov_indicator_bl <- function(curr_pline, curr_df) {
  
  # Poverty Gap indicator as proportion of poverty line
  curr_df <- curr_df %>%
    mutate(
      # Individual poverty gap (how much income falls short of poverty line)
      pov_gap_yd_pc = pmax(0, !!sym(curr_pline) - yd_pc) / !!sym(curr_pline),
      pov_gap_yp_pc = pmax(0, !!sym(curr_pline) - yp_pc) / !!sym(curr_pline),
      pov_gap_yg_pc = pmax(0, !!sym(curr_pline) - yg_pc) / !!sym(curr_pline),
      pov_gap_yn_pc = pmax(0, !!sym(curr_pline) - yn_pc) / !!sym(curr_pline),
      pov_gap_yc_pc = pmax(0, !!sym(curr_pline) - yc_pc) / !!sym(curr_pline),
      pov_gap_yf_pc = pmax(0, !!sym(curr_pline) - yf_pc) / !!sym(curr_pline)
      
    ) %>% 
    mutate(
      
      # Squared poverty gap (for FGT2 measure)
      pov_gap_yd_pc_sqrd = (pov_gap_yd_pc)^2,
      pov_gap_yp_pc_sqrd = (pov_gap_yp_pc)^2,
      pov_gap_yg_pc_sqrd = (pov_gap_yg_pc)^2,
      pov_gap_yn_pc_sqrd = (pov_gap_yn_pc)^2,
      pov_gap_yc_pc_sqrd = (pov_gap_yc_pc)^2,
      pov_gap_yf_pc_sqrd = (pov_gap_yf_pc)^2
    )
  
  
  
  # Compute poverty headcount ratio
  poverty_headcount_ratio <- curr_df %>%
    dplyr::summarise(
      pov_rate_yp_pc  = sum(weight[yp_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yn_pc  = sum(weight[yn_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yg_pc  = sum(weight[yg_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yd_pc  = sum(weight[yd_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yc_pc  = sum(weight[yc_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yf_pc  = sum(weight[yf_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE)
    )
  
  # Compute number of poor 
  nbr_poor <- curr_df %>%
    dplyr::summarise(
      nbr_poor_yp_pc  = sum(weight[yp_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yn_pc  = sum(weight[yn_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yg_pc  = sum(weight[yg_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yd_pc  = sum(weight[yd_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yc_pc  = sum(weight[yc_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yf_pc  = sum(weight[yf_pc  < !!sym(curr_pline)], na.rm = TRUE)
    )
  
  # Poverty Gap
  poverty_gap <- tibble(
    pov_gap_yp_pc  = weighted.mean(curr_df$pov_gap_yp_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yn_pc  = weighted.mean(curr_df$pov_gap_yn_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yg_pc  = weighted.mean(curr_df$pov_gap_yg_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yd_pc  = weighted.mean(curr_df$pov_gap_yd_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yc_pc  = weighted.mean(curr_df$pov_gap_yc_pc,  curr_df$weight, na.rm = TRUE)*100,
    pov_gap_yf_pc  = weighted.mean(curr_df$pov_gap_yf_pc,  curr_df$weight, na.rm = TRUE)*100
  )
  
  # Poverty severity index - squared poverty gap measure
  poverty_sev <- tibble(
    pov_sev_yp_pc  = weighted.mean(curr_df$pov_gap_yp_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yn_pc  = weighted.mean(curr_df$pov_gap_yn_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yg_pc  = weighted.mean(curr_df$pov_gap_yg_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yd_pc  = weighted.mean(curr_df$pov_gap_yd_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yc_pc  = weighted.mean(curr_df$pov_gap_yc_pc_sqrd,  curr_df$weight, na.rm = TRUE),
    pov_sev_yf_pc  = weighted.mean(curr_df$pov_gap_yf_pc_sqrd,  curr_df$weight, na.rm = TRUE)
  )
  
  
  # Poverty measurements matrice
  summary_tab <- as.data.frame(cbind(t(poverty_headcount_ratio), t(nbr_poor), t(poverty_gap), t(poverty_sev), 
                                     Income = c("Market Income plus pensions", "Net Market Income",
                                                "Gross Income","Disposable Income","Consumable Income","Final Income"))) %>% 
    pivot_longer(cols = starts_with("V"),
                 names_to = "Parameter",
                 values_to = "Pre-reform",
                 values_drop_na = TRUE) %>% 
    mutate(Parameter = recode(Parameter,
                              V1 = "Rate of poverty",
                              V2 = "Number of poor",
                              V3 = "Poverty gap",
                              V4 = "Poverty severity"),
           `Pre-reform` = round(as.numeric(`Pre-reform`),2),
           `Poverty line` = curr_pline) 
  
  return(summary_tab)
}

# Analysis per region
geo_pov_per_region <- baseline_data %>% 
  group_by(region) %>% 
  group_split() %>%
  map_dfr(., function(splitted_df){
    
    curr_region = first(splitted_df$region)
    
    pov_per_region <-  map_dfr(lst_pline, function(curr_pline) {    
        bind_rows(
          get_geo_pov_indicator_bl(curr_pline, splitted_df) %>%
            mutate(Code_area = as.character(curr_region), Area = "region") 
        )
    }) 
    
    pov_per_region
  })


# Analysis per district
geo_pov_per_district <- baseline_data %>% 
  group_by(district) %>% 
  group_split() %>%
  map_dfr(., function(splitted_df){
    
    curr_district= first(splitted_df$district)
    
    pov_per_district <-  map_dfr(lst_pline, function(curr_pline) {    
      bind_rows(
        get_geo_pov_indicator_bl(curr_pline, splitted_df) %>%
          mutate(Code_area =  as.character(curr_district), Area = "district") 
      )
    }) 
    
    pov_per_district
  })

# Merging to get admin_name
baseline_geo_pov_estimates <- plyr::rbind.fill(geo_pov_per_region,geo_pov_per_district)

baseline_geo_pov_estimates <- full_join(baseline_geo_pov_estimates, 
                                        correspondance_table %>% 
                                          select(Admin_Code, Admin_Label) %>% 
                                          rename(admin_code = Admin_Code, 
                                                 admin_name = Admin_Label) %>% 
                                          mutate(admin_code = as.character(admin_code),
                                                 admin_name = admin_name),   
                                          by=c("Code_area"="admin_code")) %>% 
  left_join(correspondance_table %>% 
              select(Region_Code, Region) %>% 
              rename(admin_code = Region_Code, 
                     admin_name_region = Region) %>% 
              mutate(admin_code = as.character(admin_code)) %>% 
              distinct(.keep_all = TRUE),   
            by=c("Code_area"="admin_code")) %>% 
  mutate(admin_name = ifelse(is.na(admin_name), admin_name_region, admin_name)) %>% 
  select(Income,Parameter,`Pre-reform`,`Poverty line`,Code_area,Area,admin_name,admin_name)
  

# Saving 
write_rds(baseline_geo_pov_estimates, paste0(here(),output_folder, "/Shiny Data/baseline_geo_pov_estimates.rds"))



# Merge geospatial dataset with insurgency information
mlw_bound <- full_join(mlw_bound, correspondance_table,   by=c("GID_1"="Shapefile_ID")) 


# Merge geometry and appropriate shp
mlw_bound_region <- mlw_bound %>% 
  group_by(Region_Code) %>% 
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% 
  distinct(Region_Code, Region, .keep_all = T) %>%
  dplyr::select(Region_Code, Region, geometry) %>% 
  rename(admin_code = Region_Code, 
         admin_name = Region)%>% 
  mutate(admin_code = as.character(admin_code),
         admin_name = as.character(admin_name)
  )

mlw_bound_district <- mlw_bound %>% 
  select(Admin_Code, Admin_Label, geometry) %>% 
  rename(admin_code = Admin_Code, 
         admin_name = Admin_Label) %>% 
  mutate(admin_code = as.character(admin_code),
         admin_name = as.character(admin_name)
         )


# Saving
st_write(mlw_bound_region, paste0(here(),output_folder, "/Shapefile/region_geo.shp"), quiet=TRUE, delete_layer=TRUE)
st_write(mlw_bound_district, paste0(here(),output_folder, "/Shapefile/district_geo.shp"), quiet=TRUE, delete_layer=TRUE)
