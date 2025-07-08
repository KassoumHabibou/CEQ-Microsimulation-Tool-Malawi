
################################################################################
######### CEQ initial data wrangling file for MALAWI ###########################
################################################################################

# Data wrangling

######################## Importing library and external files ##################
### List of required packages
required_packages <- c("tidyverse", "dplyr","haven","here","DescTools","labelled",
                       "survey","wINEQ","Hmisc")

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

hh_data_path  <- paste0(data_folder,"/Household Surveys/IHS5 2019-20/Household")
int_data_path <- paste0(data_folder, "/Intermediate")
pdata_path <- paste0(output_folder, "/Output")
agri_data_path <- paste0(data_folder, "/Household Surveys/IHS5 2019-20/Agriculture")
fish_data_path <- paste0(data_folder, "/Household Surveys/IHS5 2019-20/Fisheries")
#dofile_path <- paste0(data_folder, "Script/income_aggregates")
script_path <- paste0(data_folder, "/Script")


# shape_files <- paste0(here(),data_folder, "/Shapefiles")
# shiny_files <- paste0(here(),output_folder, "/Shiny Data") #folder which contains indicator data files ready for inclusion in live profiles tool

## Importing data 
# HH_vars <-  read_dta(paste0(here(),data_folder, "/HH_vars.dta"))
# poor <- read_dta(paste0(here(),data_folder, "/poor.dta"))
# iincome_ihs5 <- read_dta(paste0(here(),data_folder, "/iincome_ihs5.dta"))

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

before_data <- left_join(yc, yd) %>% 
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

write_rds(before_data, paste0(here(),output_folder, "/Shiny Data/before.rds"))
# 
# before_data_wtht_na <- before_data %>% mutate(across(c(  "yd_pc", "ym_hh", "yp_hh", "yn_hh", "yg_hh", "yt_hh", "yd_hh", "yc_hh",
#                                                  "yf_hh", "coupons_hh", "i_coupons", "tfc_ben", "i_health", "illness_cat",
#                                                  "gov_expense_individual", "donor_expense_individual", "hh_expense_individual",
#                                                  "i_Health_hh", "gov_expense_hh", "donor_expense_hh", "hh_expense_hh",
#                                                  "illness_cat_hh", "gov_expense_HIV", "gov_expense_NCD", "gov_expense_Repr",
#                                                  "gov_expense_Mal", "gov_expense_TB", "gov_expense_NutritionalD", "gov_expense_Other",
#                                                  "donor_expense_HIV", "donor_expense_NCD", "donor_expense_Repr", "donor_expense_Mal",
#                                                  "donor_expense_TB", "donor_expense_NutritionalD", "donor_expense_Other",
#                                                  "hh_expense_HIV", "hh_expense_NCD", "hh_expense_Repr", "hh_expense_Mal",
#                                                  "hh_expense_TB", "hh_expense_NutritionalD", "hh_expense_Other", "edu_unit_cost_total",
#                                                  "i_edu_unit_cost_total", "i_Education_hh", "gov_edu_preschool", "gov_edu_primary",
#                                                  "gov_edu_secondary", "gov_edu_postsecondary", "gov_edu_tertiary", "gov_edu_masterphd",
#                                                  "at_school", "type_school", "Education_fee", "Health_fee", "Users_fee", "illness",
#                                                  "Users_fee_hh", "Education_fee_hh", "Health_fee_hh", "i_Usersfee", "i_Usersfee_hh",
#                                                  "i_Education_fee", "i_Education_fee_hh", "i_Health_fee", "i_Health_fee_hh",
#                                                  "dtx_payt_hh", "i_dtx_payt_hh", "p_tax", "i_ptax", "tran_1", "tran_2", "tran_32",
#                                                  "tran_31", "tran_4", "tran_8", "tran_91", "tran_13", "tran_11", "tran_12",
#                                                  "dct_hh", "dtr_nct_hh"), ~ replace_na(.x, 0)))


before_data_wtht_na <- before_data
write_rds(before_data_wtht_na, paste0(here(),output_folder, "/Shiny Data/before_data_wtht_na.rds"))


before_data_wtht_na <- readRDS("~/Dropbox/PhD thesis/Internship World Bank/CEQ - Dashboard/CEQ - assessement tool/output/Shiny Data/before_data_wtht_na.rds")
before_data_wtht_na <- curr_df %>% select(hhid, pid, i22_return_a) %>% right_join(before_data_wtht_na, by=c("hhid"="hhid", "pid"="pid"))

######################## Computing some agregates concepts ##############################

# Creating household education variables
before_data_wtht_na <- before_data_wtht_na %>%
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
before_data_wtht_na <- before_data_wtht_na %>%
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
before_data_wtht_na <- before_data_wtht_na %>%
  group_by(hhid) %>%
  mutate(p_tax_hh = sum(p_tax, na.rm = TRUE)) %>%
  ungroup()

# Health and education expenditures by household
before_data_wtht_na <- before_data_wtht_na %>%
  mutate(
    health_hh = gov_expense_hh %>% structure(label="Health (household)"),
    educ_hh = rowSums(across(c(
      educ_preschool_hh, educ_primary_hh, educ_secondary_hh,
      educ_postsecondary_hh, educ_tertiary_hh, educ_masterphd_hh
    ), ~ replace_na(., 0))) %>% structure(label="Education (household)")
  )


#  Rename variables
before_data_wtht_na <- before_data_wtht_na %>%
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
before_data_wtht_na <- before_data_wtht_na %>%
  select(hhid, pid, 
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

before_data_wtht_na <- before_data_wtht_na[!is.na(before_data_wtht_na$weight),] 
  
# Taxes rate
before_data_wtht_na <- before_data_wtht_na %>%
  mutate(
    dtx_all_hh = (dtx_PIT_hh + dtx_payt_hh) %>% structure(label="All direct taxes paid, HH total"),
    dtr_all_hh = (dct_hh + dtr_nct_hh) %>% structure(label= "All direct transfers, HH total"), 
    sub_all_hh = (sub_electri_hh + sub_fuel_hh) %>% structure(label="All indirect subsidies, HH total"), 
    itx_all_hh = (itx_vatx_hh + itx_excx_hh) %>% structure(label="All indirect taxes, HH total")
  )


# ********************************************************************************
#   *						GENERATE INCOME VARIABLES
# ********************************************************************************


before_data_wtht_na <- before_data_wtht_na %>%
  mutate(
    # Replace and generate household income variables
    yg_hh = yd_hh + dtx_all_hh,
    yn_hh = yd_hh - dtr_all_hh,
    yp_hh = yg_hh - dtr_all_hh,
    yc_hh = yd_hh + sub_all_hh - itx_all_hh,
    yf_hh = yc_hh + educ_hh + health_hh - Users_fee_hh
  ) %>% 
  mutate(
    
    # Per capita variables
    yd_pc = (yd_hh / hhsize) %>% structure(label="Disposable Income (per capita)"),
    yg_pc = (yg_hh / hhsize) %>% structure(label="Gross Income (per capita)"),
    yn_pc = (yn_hh / hhsize) %>% structure(label="Net Market Income (per capita)"),
    yp_pc = (yp_hh / hhsize) %>% structure(label="Market Income plus pensions (per capita)"),
    yc_pc = (yc_hh / hhsize) %>% structure(label="Consumable Income (per capita)"),
    yf_pc = (yf_hh / hhsize) %>% structure(label="Final Income (per capita)")
  )

# Poverty Gap indicator as proportion of poverty line
before_data_wtht_na <- before_data_wtht_na %>%
  mutate(
    # Individual poverty gap (how much income falls short of poverty line)
    pov_gap_pcrexp = pmax(0, pline_mod - pcrexp) / pline_mod,
    pov_gap_yd_pc = pmax(0, pline_mod - yd_pc) / pline_mod,
    pov_gap_yp_pc = pmax(0, pline_mod - yp_pc) / pline_mod,
    pov_gap_yg_pc = pmax(0, pline_mod - yg_pc) / pline_mod,
    pov_gap_yc_pc = pmax(0, pline_mod - yc_pc) / pline_mod,
    pov_gap_yn_pc = pmax(0, pline_mod - yn_pc) / pline_mod,
    pov_gap_yf_pc = pmax(0, pline_mod - yf_pc) / pline_mod,

  ) %>% 
  mutate(
    
    # Squared poverty gap (for FGT2 measure)
    pov_gap_pcrexp_sqrd = (pov_gap_pcrexp)^2,
    pov_gap_yd_pc_sqrd = (pov_gap_yd_pc)^2,
    pov_gap_yp_pc_sqrd = (pov_gap_yp_pc)^2,
    pov_gap_yg_pc_sqrd = (pov_gap_yg_pc)^2,
    pov_gap_yn_pc_sqrd = (pov_gap_yn_pc)^2,
    pov_gap_yc_pc_sqrd = (pov_gap_yc_pc)^2,
    pov_gap_yf_pc_sqrd = (pov_gap_yf_pc)^2
  )



# Compute poverty headcount ratio
poverty_headcount_ratio <- before_data_wtht_na %>%
  summarise(
    pov_rate_pcrexp = sum(weight[pcrexp < pline_mod], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
    pov_rate_yd_pc = sum(weight[yd_pc < pline_mod], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
    pov_rate_yp_pc  = sum(weight[yp_pc  < pline_mod], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
    pov_rate_yg_pc  = sum(weight[yg_pc  < pline_mod], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
    pov_rate_yn_pc  = sum(weight[yn_pc  < pline_mod], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
    pov_rate_yc_pc  = sum(weight[yc_pc  < pline_mod], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
    pov_rate_yf_pc  = sum(weight[yf_pc  < pline_mod], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE)
  )

# Compute number of poor 
nbr_poor <- before_data_wtht_na %>%
  summarise(
    nbr_poor_pcrexp = sum(weight[pcrexp < pline_mod], na.rm = TRUE),
    nbr_poor_yd_pc  = sum(weight[yd_pc  < pline_mod], na.rm = TRUE),
    nbr_poor_yp_pc  = sum(weight[yp_pc  < pline_mod], na.rm = TRUE),
    nbr_poor_yg_pc  = sum(weight[yg_pc  < pline_mod], na.rm = TRUE),
    nbr_poor_yn_pc  = sum(weight[yn_pc  < pline_mod], na.rm = TRUE),
    nbr_poor_yc_pc  = sum(weight[yc_pc  < pline_mod], na.rm = TRUE),
    nbr_poor_yf_pc  = sum(weight[yf_pc  < pline_mod], na.rm = TRUE)
  )

# Poverty Gap
poverty_gap <- tibble(
    pov_gap_pcrexp = weighted.mean(before_data_wtht_na$pov_gap_pcrexp, before_data_wtht_na$weight, na.rm = TRUE)*100,
    pov_gap_yd_pc  = weighted.mean(before_data_wtht_na$pov_gap_yd_pc,  before_data_wtht_na$weight, na.rm = TRUE)*100,
    pov_gap_yp_pc  = weighted.mean(before_data_wtht_na$pov_gap_yp_pc,  before_data_wtht_na$weight, na.rm = TRUE)*100,
    pov_gap_yg_pc  = weighted.mean(before_data_wtht_na$pov_gap_yg_pc,  before_data_wtht_na$weight, na.rm = TRUE)*100,
    pov_gap_yn_pc  = weighted.mean(before_data_wtht_na$pov_gap_yn_pc,  before_data_wtht_na$weight, na.rm = TRUE)*100,
    pov_gap_yc_pc  = weighted.mean(before_data_wtht_na$pov_gap_yc_pc,  before_data_wtht_na$weight, na.rm = TRUE)*100,
    pov_gap_yf_pc  = weighted.mean(before_data_wtht_na$pov_gap_yf_pc,  before_data_wtht_na$weight, na.rm = TRUE)*100
  )

# Poverty severity index - squared poverty gap measure
poverty_sev <- tibble(
    pov_sev_pcrexp = weighted.mean(before_data_wtht_na$pov_gap_pcrexp_sqrd, before_data_wtht_na$weight, na.rm = TRUE),
    pov_sev_yd_pc  = weighted.mean(before_data_wtht_na$pov_gap_yd_pc_sqrd,  before_data_wtht_na$weight, na.rm = TRUE),
    pov_sev_yp_pc  = weighted.mean(before_data_wtht_na$pov_gap_yp_pc_sqrd,  before_data_wtht_na$weight, na.rm = TRUE),
    pov_sev_yg_pc  = weighted.mean(before_data_wtht_na$pov_gap_yg_pc_sqrd,  before_data_wtht_na$weight, na.rm = TRUE),
    pov_sev_yn_pc  = weighted.mean(before_data_wtht_na$pov_gap_yn_pc_sqrd,  before_data_wtht_na$weight, na.rm = TRUE),
    pov_sev_yc_pc  = weighted.mean(before_data_wtht_na$pov_gap_yc_pc_sqrd,  before_data_wtht_na$weight, na.rm = TRUE),
    pov_sev_yf_pc  = weighted.mean(before_data_wtht_na$pov_gap_yf_pc_sqrd,  before_data_wtht_na$weight, na.rm = TRUE)
  )

# Poverty measurements matrice
Baseline_data_tab <- as.data.frame(cbind(t(poverty_headcount_ratio), t(nbr_poor), t(poverty_gap), t(poverty_sev), 
              Income = c("Real expenditures","Disposable Income","Market Income plus pensions",
                         "Gross Income","Net Market Income","Consumable Income","Final Income"))) %>% 
  pivot_longer(cols = starts_with("V"),
               names_to = "Parameter",
               values_to = "Baseline",
               values_drop_na = TRUE) %>% 
  mutate(Parameter = recode(Parameter,
                            V1 = "Rate",
                            V2 = "Number of Poor",
                            V3 = "Poverty GAP",
                            V4 = "Poverty Severity"))



write_rds(Baseline_data_tab, paste0(here(),output_folder, "/Shiny Data/Baseline_data_tab.rds"))

# Gini results
# gini_results <- list(
#   yp_pc  = wINEQ::Gini(before_data_wtht_na$yp_pc, W = before_data_wtht_na$weight, fast = TRUE, rounded.weights = TRUE),
#   yg_pc  = wINEQ::Gini(before_data_wtht_na$yg_pc, W = before_data_wtht_na$weight, fast = TRUE, rounded.weights = TRUE),
#   yn_pc  = wINEQ::Gini(before_data_wtht_na$yn_pc, W = before_data_wtht_na$weight, fast = TRUE, rounded.weights = TRUE),
#   yd_pc  = wINEQ::Gini(before_data_wtht_na$yd_pc, W = before_data_wtht_na$weight, fast = TRUE, rounded.weights = TRUE),
#   yc_pc  = wINEQ::Gini(before_data_wtht_na$yc_pc, W = before_data_wtht_na$weight, fast = TRUE, rounded.weights = TRUE),
#   yf_pc  = wINEQ::Gini(before_data_wtht_na$yf_pc, W = before_data_wtht_na$weight, fast = TRUE, rounded.weights = TRUE)
# )

before_data_wtht_na <- before_data_wtht_na %>%
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
before_data_wtht_na <- before_data_wtht_na %>%
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
write_rds(before_data_wtht_na, paste0(here(),output_folder, "/Shiny Data/baseline_final_data.rds"))
write_rds(Baseline_data_tab, paste0(here(),output_folder, "/Shiny Data/Baseline_data_tab.rds"))

# Variable to compute some stat
vars_sum <- c("yp_hh", "dtx_all_hh", "yn_hh", "dtr_all_hh", "dtr_nct_hh", "dtr_frmz_hh", "dtr_nfra_hh", "dtr_masaf_hh",
              "dtr_ffwk_hh", "dtr_ifwp_hh", "dtr_ses_hh", "dtr_tes_hh", "dtr_onc_hh", "dct_hh", "dct_gov_hh", "dct_ngo_hh", "dct_fips_hh",
              "yg_hh", "yd_hh", "sub_all_hh", "sub_electri_hh", "sub_fuel_hh", "itx_all_hh", "yc_hh",
              "educ_hh", "educ_preschool_hh", "educ_primary_hh", "educ_secondary_hh", "educ_postsecondary_hh",
              "educ_tertiary_hh", "educ_masterphd_hh", "health_hh",
              "health_gov_hiv_hh", "health_gov_ncd_hh", "health_gov_rep_hh", "health_gov_mal_hh", "health_gov_tb_hh",
              "health_gov_nut_hh", "health_gov_other_hh", "Users_fee_hh", "Education_fee_hh", "Health_fee_hh", "yf_hh")

vars_bin <- c("yp_hh", "dtx_all_hh", "yn_hh", "dtr_all_hh", "dtr_nct_hh", "dtr_frmz_hh", "dtr_nfra_hh", "dtr_masaf_hh",
              "dtr_ffwk_hh", "dtr_ifwp_hh", "dtr_ses_hh", "dtr_tes_hh", "dtr_onc_hh", "dct_hh", "dct_gov_hh", "dct_ngo_hh", "dct_fips_hh",
              "yg_hh", "yd_hh", "sub_all_hh", "sub_electri_hh", "sub_fuel_hh", "itx_all_hh", "yc_hh",
              "educ_hh", "health_hh", "Users_fee_hh", "Education_fee_hh", "Health_fee_hh", "yf_hh")


# Create binary indicators and Generate sum by decile (in millions)
before_data_wtht_na <- before_data_wtht_na %>%
  mutate(across(all_of(vars_bin), ~ as.integer(. > 0), .names = "i_{.col}")) %>% 
  mutate(across(all_of(vars_sum), ~ . / 1e6))


i_vars_bin <- paste0("i_", vars_bin)

# Sum by decile (weighted)
sum_by_decile <- before_data_wtht_na %>%
  filter(pid == 1) %>%
  group_by(decile) %>%
  summarise(across(all_of(vars_sum), ~ sum(. * weight, na.rm = TRUE), .names = "{.col}")) %>%
  arrange(decile)

# Export as plain CSV
write.csv(sum_by_decile, paste0(here(),output_folder, "/Shiny Data/sum_deciles_MWB_DM.csv"))

# Mean by decile (weighted)
mean_by_decile <- before_data_wtht_na %>%
  filter(pid == 1) %>%
  group_by(decile) %>%
  summarise(across(all_of(vars_sum), ~ weighted.mean(., weight, na.rm = TRUE), .names = "{.col}")) %>%
  arrange(decile)

write.csv(mean_by_decile,  paste0(here(),output_folder, "/Shiny Data/mean_deciles_MWB_DM.csv"))


# Count of households with positive value by decile
count_hh_by_decile <- before_data_wtht_na %>%
  filter(pid == 1) %>%
  group_by(decile) %>%
  summarise(across(all_of(i_vars_bin), ~ sum(. * weight, na.rm = TRUE), .names = "{.col}")) %>%
  arrange(decile)

write.csv(count_hh_by_decile, paste0(here(),output_folder, "/Shiny Data/count_hogares_MWB_DM.csv"))


