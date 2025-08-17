# 
# ################################################################################
# ######### CEQ Indirect taxes ###################################################
# ################################################################################
# # Load household consumption data
# Elec_subsidy <- read_dta(paste0(here(), data_folder, "/Intermediate/Elec_subsidy.dta"))
# 
# indirect_tax <- read_dta(paste0(here(), data_folder, "/Intermediate/indirect_tax.dta"))
# Fuel_subsidy <- read_dta(paste0(here(), data_folder, "/Intermediate/Fuel_subsidy.dta"))
# agric_subsidy <- read_dta(paste0(here(), data_folder, "/Intermediate/final/agric_subsidy.dta"))
# 
# 
# ################################################################################
# ######### PAYE TAX PRE-REFORM ##################################################
# ################################################################################
# 
# ################################## For Some tables #############################
# 
# ################################# VAT table. ##################################
# # Load catalog (expects columns: Code, itemcode)
# vat_catalog <- indirect_tax
# 
# # Keep unique
# vat_catalog <- vat_catalog %>% 
#   select(code, itemcode, vat_rate) %>% 
#   distinct(.keep_all = TRUE) %>% 
#   rename(pre_rate=vat_rate)
# 
# 
# # Drop missing
# vat_catalog <- vat_catalog %>% 
#   drop_na() %>% 
#   arrange(code, itemcode) %>% 
#   filter(itemcode!="")
# 
# ################################# Excise table. ##################################
# excise_catalog <- indirect_tax
# 
# # Keep unique
# excise_catalog <- excise_catalog %>% 
#   select(code, itemcode, excise_rate) %>% 
#   distinct(.keep_all = TRUE) %>% 
#   rename(pre_rate=excise_rate)
# 
# # Drop missing
# excise_catalog <- excise_catalog %>% 
#   drop_na() %>% 
#   arrange(code, itemcode) %>% 
#   filter(itemcode!="")
# 
# # Deduplicate + order for nicer display
# 
# ################################# Direct cash tr table. #########################
# dct_catalog <- yn
# 
# # Keep unique
# dct_catalog <- dct_catalog %>% 
#   select(hhid, decile, tran_11) %>% 
#   distinct(.keep_all = TRUE) %>% 
#   rename(pre_rate=excise_rate)
# 
# # Drop missing
# excise_catalog <- excise_catalog %>% 
#   drop_na() %>% 
#   arrange(code, itemcode) %>% 
#   filter(itemcode!="")
# 
# ## exporting
# write_rds(indirect_tax, paste0(here(),output_folder, "/shiny_data/baseline_data_itx.rds"))
# write_rds(vat_catalog, paste0(here(),output_folder, "/shiny_data/vat_catalog.rds"))
# write_rds(excise_catalog, paste0(here(),output_folder, "/shiny_data/excise_catalog.rds"))