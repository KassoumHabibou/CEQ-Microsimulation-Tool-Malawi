
################################################################################
######### CEQ Indirect taxes ###################################################
################################################################################
# Load household consumption data
Elec_subsidy <- read_dta(paste0(here(), data_folder, "/Intermediate/Elec_subsidy.dta"))

indirect_tax <- read_dta(paste0(here(), data_folder, "/Intermediate/indirect_tax.dta"))
Fuel_subsidy <- read_dta(paste0(here(), data_folder, "/Intermediate/Fuel_subsidy.dta"))
agric_subsidy <- read_dta(paste0(here(), data_folder, "/Intermediate/final/agric_subsidy.dta"))


################################################################################
######### PAYE TAX PRE-REFORM ##################################################
################################################################################

curr_df_3 <- indirect_tax 

# %>% 
#   full_join(Fuel_subsidy) %>% 
#   full_join(agric_subsidy) %>% 
#   full_join(Elec_subsidy) %>% 
#   full_join(yd %>% select(hhid,pid))


## exporting
write_rds(curr_df_3, paste0(here(),output_folder, "/Shiny Data/baseline_data_itx.rds"))

