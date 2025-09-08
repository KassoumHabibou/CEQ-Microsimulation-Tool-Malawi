####################################################################################
######################## Main simulation estimates #################################
####################################################################################

simulate_main_df <- function(
    # Direct taxes
  # PAYE
  tax_rate_lowest, tax_rate_second, tax_rate_middle, tax_rate_top,
  # Corporate
  corp_tax_1,  corp_tax_2, corp_tax_3, corp_tax_4, corp_tax_5,
  corp_tax_6, corp_tax_7, corp_tax_8, corp_tax_9, corp_tax_10,
  corp_tax_11, corp_tax_12, corp_tax_13, corp_tax_14, corp_tax_15,
  corp_tax_16, corp_tax_17, corp_tax_18,
  remove_agriculture_exemption,
  remove_electricity_exemption,
  
  # Indirect taxes

  vat_group,vat_codes, vat_name, vat_post,
  excise_group,excise_codes, excise_name, excise_post,
  
  # Subsidies (cash)
  dct_gov_hh_decile,  dct_gov_hh,
  dct_fips_hh_decile, dct_fips_hh,
  
  # Near-cash transfers (one by one)
  dtr_frmz_hh_decile,  dtr_frmz_hh,
  dtr_nfra_hh_decile,  dtr_nfra_hh,
  dtr_masaf_hh_decile, dtr_masaf_hh,
  dtr_ffwk_hh_decile,  dtr_ffwk_hh,
  dtr_ifwp_hh_decile,  dtr_ifwp_hh,
  dtr_ses_hh_decile,   dtr_ses_hh,
  dtr_tes_hh_decile,   dtr_tes_hh,
  dtr_onc_hh_decile,   dtr_onc_hh,
  
  # Electricity and fuel subsidies
  elec_rate_subsidized,
  elec_block1_kwh,
  elec_rate_block2,
  shr_sub_firm,
  
  # Fuel subsidies
  fuel_subsidy_pct_gdp
) {
  
  sim_df <- bl_df %>% 
    dplyr::select(hhid, pid, decile, region, district, weight, i22_return_a, 
                  yp_hh, elec_cons_m, elec_enter,
                  educ_hh, health_hh, Users_fee_hh, hhsize, reside, pline_mod)
  
  
  ######################## Direct taxes estimates #########################################
  # STEP 1: Apply the payroll tax logic to individual-level income (i22_return_a) # nolint
  sim_df <- sim_df %>%
    mutate(
      p_tax = case_when(
        i22_return_a <= 1800000 ~ i22_return_a * tax_rate_lowest / 100,
        i22_return_a > 1800000 & i22_return_a <= 6000000 ~ i22_return_a * tax_rate_second / 100, # nolint
        i22_return_a > 6000000 & i22_return_a <= 30600000 ~ i22_return_a * tax_rate_middle / 100,
        i22_return_a > 30600000 ~ i22_return_a * tax_rate_top / 100,
        TRUE ~ 0
      )
    )
  
  # STEP 2: Aggregate payroll taxes to household level
  sim_df <- sim_df %>%
    group_by(hhid) %>%
    mutate(p_tax_hh = sum(p_tax, na.rm = TRUE)) %>%
    ungroup()
  
  # STEP 3: Compute total direct taxes at household level
  sim_df <- sim_df %>%
    rename(dtx_PIT_hh = p_tax_hh)
  
  
  ######################## Corporate taxe ######################################### corp_tax_0,
  temp_d1 <- get_coorporate_tx(
    corp_tax_1, corp_tax_2, corp_tax_3, corp_tax_4, 
    corp_tax_5,corp_tax_6,corp_tax_7,corp_tax_8, corp_tax_9,
    corp_tax_10,corp_tax_11,corp_tax_12,corp_tax_13,corp_tax_14,
    corp_tax_15,corp_tax_16,corp_tax_17,corp_tax_18, 
    remove_agriculture_exemption, remove_electricity_exemption)
  
  
  sim_df <- sim_df %>%
    left_join(temp_d1) 
  
  ## Missing values
  sim_df <- sim_df %>%
    mutate(dtx_payt_hh = ifelse(is.na(dtx_payt_hh),0,dtx_payt_hh),
           i_dtx_payt_hh = ifelse(is.na(i_dtx_payt_hh),0,i_dtx_payt_hh),
           dtx_PIT_hh = ifelse(is.na(dtx_PIT_hh),0,dtx_PIT_hh))
  

  ######################## Indirect taxes (VAT/Excise) ################
  temp_d2 <- get_tx(
    curr_df      = bl_itx,          # baseline item-level spending table
    vat_group = vat_group,
    vat_codes    = vat_codes,
    vat_name    = vat_name,
    vat_post     = vat_post,        # % per item
    excise_group = excise_group,
    excise_codes = excise_codes,
    excise_name = excise_name,
    excise_post  = excise_post      # % per item
  )
  
  sim_df <- sim_df %>%
    dplyr::left_join(temp_d2, by = "hhid") 

  ######################## Direct cash transfert ################  
  
  temp_d3 <- get_dct(
    curr_df      = bl_df %>% 
      select(hhid, pid, decile, dct_gov_hh, dct_fips_hh, dct_hh) %>% 
      filter(dct_hh > 0),          # baseline item-level spending table
    dct_gov_hh_decile    = dct_gov_hh_decile,
    dct_gov_hh     = dct_gov_hh,        # % per item
    dct_fips_hh_decile = dct_fips_hh_decile,
    dct_fips_hh  = dct_fips_hh      # % per item
  )

  sim_df <- sim_df %>%
    dplyr::left_join(temp_d3, by = c("hhid","pid","decile")) 
  
  ## Missing values
  sim_df <- sim_df %>%
    mutate(dct_hh = ifelse(is.na(dct_hh),0,dct_hh))

  # after (round everything to 2 decimals)
  #sim_df <- sim_df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  
  ######################## Near-cash transfert ##################################    
  # Build a minimal baseline slice and (optionally) filter to near-cash recipients
  temp_d4 <- get_dtr_nearcash(
    curr_df = bl_df %>%
      dplyr::select(
        hhid, pid, decile,
        dtr_frmz_hh, dtr_nfra_hh, dtr_masaf_hh, dtr_ffwk_hh,
        dtr_ifwp_hh, dtr_ses_hh,  dtr_tes_hh,   dtr_onc_hh,
        dtr_nct_hh
      ) %>%
      dplyr::filter(dtr_nct_hh > 0),
    
    dtr_frmz_hh_decile,  dtr_frmz_hh,
    dtr_nfra_hh_decile,  dtr_nfra_hh,
    dtr_masaf_hh_decile, dtr_masaf_hh,
    dtr_ffwk_hh_decile,  dtr_ffwk_hh,
    dtr_ifwp_hh_decile,  dtr_ifwp_hh,
    dtr_ses_hh_decile,   dtr_ses_hh,
    dtr_tes_hh_decile,   dtr_tes_hh,
    dtr_onc_hh_decile,   dtr_onc_hh
  )

  # Merge into the simulation df
  sim_df <- sim_df %>%
    dplyr::left_join(temp_d4, by = c("hhid","pid","decile"))
  
  # Final safety: replace any remaining NAs with 0
  sim_df <- sim_df %>%
    dplyr::mutate(
      dplyr::across(
        c(dtr_frmz_hh, dtr_nfra_hh, dtr_masaf_hh, dtr_ffwk_hh,
          dtr_ifwp_hh, dtr_ses_hh,  dtr_tes_hh,   dtr_onc_hh,
          dtr_nct_hh),
        ~ ifelse(is.na(.x), 0, .x)
      )
    )
  
  
  ######################## Electricity sub ##################################    
  # Build a minimal baseline slice and (optionally) filter to near-cash recipients
  # Electricity: use only the 3 UI inputs (reference = 50 MWK/kWh handled inside)
 
  temp_elec <- get_electricity_subsidy(
    curr_df              = bl_df %>% dplyr::select(hhid, pid, decile, cwsa, elec_cons, 
                                                   elec_cons_m, elec_enter, i_air, 
                                                   hh_l01, hh_l02, hh_n41e),
    elec_rate_subsidized = elec_rate_subsidized,
    elec_block1_kwh      = elec_block1_kwh,
    elec_rate_block2     = elec_rate_block2,
    shr_sub_firm = shr_sub_firm
  )
  

  # Fuel: UI gives only % of GDP; GDP pulled via get_gdp_mwk()
  temp_fuel <- get_fuel_subsidy_from_pct_gdp(
    curr_df              = bl_df %>% 
      dplyr::select(hhid, pid, decile, tfc1, fuel_con, fuel_enter, weight) %>% 
      filter(pid==1),
    fuel_subsidy_pct_gdp = fuel_subsidy_pct_gdp
  )

  # Merge back into the simulation frame
  ## Electricity
  sim_df <- sim_df %>%
    dplyr::left_join(temp_elec, by = c("hhid","pid")) %>%
    dplyr::mutate(sub_electri_hh = ifelse(is.na(sub_electri_hh), 0, sub_electri_hh))
  
  ## Fueld
  sim_df <- sim_df %>%
    dplyr::left_join(temp_fuel, by = c("hhid")) %>%
    dplyr::mutate(sub_fuel_hh = ifelse(is.na(sub_fuel_hh), 0, sub_fuel_hh))

  ####################################################################################
  ######################## Income concepts estimates #################################
  ####################################################################################
  
  
  ######## Direct and inderect taxes ###############################################
  sim_df <- sim_df %>%
    dplyr::mutate(
      dtx_all_hh = pmax(0, (dtx_PIT_hh + dtx_payt_hh)) %>% structure(label="All direct taxes paid, HH total"),
      dtr_all_hh = pmax(0, (dct_hh + dtr_nct_hh)) %>% structure(label="All direct transfers, HH total"),
      sub_all_hh = pmax(0, (sub_electri_hh + sub_fuel_hh)) %>% structure(label="All indirect subsidies, HH total"),
      itx_all_hh = pmax(0, (itx_vatx_hh + itx_excx_hh)) %>% structure(label="All indirect taxes, HH total")
    )
  
  ######################## Income concepts ############################
  # 

  # STEP 4: Recalculate household-level income concepts
  sim_df <- sim_df %>%
    mutate(
      yg_hh = (yp_hh + dtr_all_hh) %>% structure(label = "Gross Income"),
      yn_hh = (yp_hh - dtx_all_hh) %>% structure(label = "Net Market Income")
    ) %>%
    mutate(
      yd_hh = (yn_hh + dtr_all_hh) %>% structure(label = "Disposable Income")
    ) %>%
    mutate(
      yc_hh = (yd_hh + sub_all_hh - itx_all_hh) %>% structure(label = "Consumable Income")
    ) %>%
    mutate(
      yf_hh = (yc_hh + educ_hh + health_hh - Users_fee_hh) %>% structure(label = "Final Income")
    ) %>% 
    mutate(
      yg_hh = ifelse(yg_hh < 0, 0, yg_hh),
      yn_hh = ifelse(yn_hh < 0, 0, yn_hh),
      yd_hh = ifelse(yd_hh < 0, 0, yd_hh),
      yp_hh = ifelse(yp_hh < 0, 0, yp_hh),
      yc_hh = ifelse(yc_hh < 0, 0, yc_hh),
      yf_hh = ifelse(yf_hh < 0, 0, yf_hh)
      
    )
  

  # STEP 5: Compute per capita versions of income concepts
  sim_df <- sim_df %>%
    mutate(
      yd_pc = (yd_hh / hhsize) %>% structure(label = "Disposable Income (per capita)"),
      yg_pc = (yg_hh / hhsize) %>% structure(label = "Gross Income (per capita)"),
      yn_pc = (yn_hh / hhsize) %>% structure(label = "Net Market Income (per capita)"),
      yp_pc = (yp_hh / hhsize) %>% structure(label = "Market Income plus pensions (per capita)"),
      yc_pc = (yc_hh / hhsize) %>% structure(label = "Consumable Income (per capita)"),
      yf_pc = (yf_hh / hhsize) %>% structure(label = "Final Income (per capita)")
    ) %>% 
    mutate(pline_mod_low = 656.7*365,
           pline_mod_middle = 1115*365)
  
  # Rounding numbers
  sim_df <- sim_df %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  return(sim_df)
}



get_dtr_nearcash <- function(
    curr_df,                        
    dtr_frmz_hh_decile,  dtr_frmz_hh,
    dtr_nfra_hh_decile,  dtr_nfra_hh,
    dtr_masaf_hh_decile, dtr_masaf_hh,
    dtr_ffwk_hh_decile,  dtr_ffwk_hh,
    dtr_ifwp_hh_decile,  dtr_ifwp_hh,
    dtr_ses_hh_decile,   dtr_ses_hh,
    dtr_tes_hh_decile,   dtr_tes_hh,
    dtr_onc_hh_decile,   dtr_onc_hh
) {
  
  # Build mapping tables (post amounts in MWK to be ADDED by decile)
  frmz_map  <- tibble::tibble(decile = dtr_frmz_hh_decile,  dtr_frmz_hh_add  = dtr_frmz_hh)
  nfra_map  <- tibble::tibble(decile = dtr_nfra_hh_decile,  dtr_nfra_hh_add  = dtr_nfra_hh)
  masaf_map <- tibble::tibble(decile = dtr_masaf_hh_decile, dtr_masaf_hh_add = dtr_masaf_hh)
  ffwk_map  <- tibble::tibble(decile = dtr_ffwk_hh_decile,  dtr_ffwk_hh_add  = dtr_ffwk_hh)
  ifwp_map  <- tibble::tibble(decile = dtr_ifwp_hh_decile,  dtr_ifwp_hh_add  = dtr_ifwp_hh)
  ses_map   <- tibble::tibble(decile = dtr_ses_hh_decile,   dtr_ses_hh_add   = dtr_ses_hh)
  tes_map   <- tibble::tibble(decile = dtr_tes_hh_decile,   dtr_tes_hh_add   = dtr_tes_hh)
  onc_map   <- tibble::tibble(decile = dtr_onc_hh_decile,   dtr_onc_hh_add   = dtr_onc_hh)
  
  # Work on a minimal copy
  temp_df <- curr_df %>%
    dplyr::select(
      hhid, pid, decile,
      dtr_frmz_hh, dtr_nfra_hh, dtr_masaf_hh, dtr_ffwk_hh,
      dtr_ifwp_hh, dtr_ses_hh,  dtr_tes_hh,   dtr_onc_hh,
      dtr_nct_hh
    ) %>%
    # Join all add-on maps by decile
    dplyr::left_join(frmz_map,  by = "decile") %>%
    dplyr::left_join(nfra_map,  by = "decile") %>%
    dplyr::left_join(masaf_map, by = "decile") %>%
    dplyr::left_join(ffwk_map,  by = "decile") %>%
    dplyr::left_join(ifwp_map,  by = "decile") %>%
    dplyr::left_join(ses_map,   by = "decile") %>%
    dplyr::left_join(tes_map,   by = "decile") %>%
    dplyr::left_join(onc_map,   by = "decile") %>%
    # Replace NAs with 0 for both baseline and add-on columns
    dplyr::mutate(
      dplyr::across(
        c(dtr_frmz_hh, dtr_nfra_hh, dtr_masaf_hh, dtr_ffwk_hh,
          dtr_ifwp_hh, dtr_ses_hh,  dtr_tes_hh,   dtr_onc_hh,
          dtr_nct_hh),
        ~ ifelse(is.na(.x), 0, .x)
      ),
      dplyr::across(
        c(dtr_frmz_hh_add, dtr_nfra_hh_add, dtr_masaf_hh_add, dtr_ffwk_hh_add,
          dtr_ifwp_hh_add, dtr_ses_hh_add,  dtr_tes_hh_add,   dtr_onc_hh_add),
        ~ ifelse(is.na(.x), 0, .x)
      )
    ) %>%
    # Compute new component amounts and the new total; clamp to >= 0 just in case
    dplyr::mutate(
      dtr_frmz_hh_new  = pmax(0, dtr_frmz_hh  + dtr_frmz_hh_add),
      dtr_nfra_hh_new  = pmax(0, dtr_nfra_hh  + dtr_nfra_hh_add),
      dtr_masaf_hh_new = pmax(0, dtr_masaf_hh + dtr_masaf_hh_add),
      dtr_ffwk_hh_new  = pmax(0, dtr_ffwk_hh  + dtr_ffwk_hh_add),
      dtr_ifwp_hh_new  = pmax(0, dtr_ifwp_hh  + dtr_ifwp_hh_add),
      dtr_ses_hh_new   = pmax(0, dtr_ses_hh   + dtr_ses_hh_add),
      dtr_tes_hh_new   = pmax(0, dtr_tes_hh   + dtr_tes_hh_add),
      dtr_onc_hh_new   = pmax(0, dtr_onc_hh   + dtr_onc_hh_add),
      dtr_nct_hh_new   = pmax(0, dtr_frmz_hh_new + dtr_nfra_hh_new + dtr_masaf_hh_new + dtr_ffwk_hh_new +
                                dtr_ifwp_hh_new + dtr_ses_hh_new  + dtr_tes_hh_new   + dtr_onc_hh_new
      )
    ) %>%
    dplyr::select(
      hhid, pid, decile,
      dtr_frmz_hh_new, dtr_nfra_hh_new, dtr_masaf_hh_new, dtr_ffwk_hh_new,
      dtr_ifwp_hh_new, dtr_ses_hh_new,  dtr_tes_hh_new,   dtr_onc_hh_new,
      dtr_nct_hh_new
    ) %>%
    dplyr::rename(
      dtr_frmz_hh = dtr_frmz_hh_new,
      dtr_nfra_hh = dtr_nfra_hh_new,
      dtr_masaf_hh = dtr_masaf_hh_new,
      dtr_ffwk_hh = dtr_ffwk_hh_new,
      dtr_ifwp_hh = dtr_ifwp_hh_new,
      dtr_ses_hh = dtr_ses_hh_new,
      dtr_tes_hh = dtr_tes_hh_new,
      dtr_onc_hh = dtr_onc_hh_new,
      dtr_nct_hh = dtr_nct_hh_new
    )
  
  return(temp_df)
}




get_dct <- function(curr_df, dct_gov_hh_decile, dct_gov_hh, dct_fips_hh_decile, dct_fips_hh) {
  

  # Build mapping tables (post rates in FRACTIONS)
  dct_gov_map <- tibble::tibble(
    decile = dct_gov_hh_decile,
    dct_gov_hh_new = dct_gov_hh
  )
  
  dct_fips_map <- tibble::tibble(
    decile = dct_fips_hh_decile,
    dct_fips_hh_new = dct_fips_hh
  )
  

  temp_df <- curr_df %>%
    select(hhid, decile, pid, dct_gov_hh, dct_fips_hh, dct_hh) 
 
  temp_df <- temp_df %>% 
    dplyr::left_join(dct_gov_map, by = "decile") %>%
    dplyr::left_join(dct_fips_map, by = "decile") %>%
    # Fallback to baseline if a post value is missing
    dplyr::mutate(
      dct_hh_new = (dct_hh + dct_gov_hh_new + dct_fips_hh_new),
      dct_gov_hh_new    = (dct_gov_hh_new + dct_gov_hh),
      dct_fips_hh_new = (dct_fips_hh_new + dct_fips_hh)
    ) %>%
    select(hhid, pid, decile, dct_fips_hh_new, dct_gov_hh_new, dct_hh_new) %>% 
    rename(dct_gov_hh = dct_gov_hh_new,
          dct_fips_hh = dct_fips_hh_new,
          dct_hh = dct_hh_new
          )
    
    ## Missing values
  temp_df <- temp_df %>%
    mutate(dct_gov_hh = ifelse(is.na(dct_gov_hh) | dct_gov_hh < 0, 0,dct_gov_hh),
           dct_fips_hh = ifelse(is.na(dct_fips_hh) | dct_fips_hh < 0 ,0,dct_fips_hh),
           dct_hh = ifelse(is.na(dct_hh) | dct_hh < 0 ,0,dct_hh))
  
  return(temp_df)
}


get_tx <- function(curr_df, vat_group, vat_codes, vat_name, vat_post, 
                   excise_group, excise_codes, excise_name, excise_post) {
  

  # Build mapping tables (post rates in FRACTIONS)
  vat_map <- tibble::tibble(
    group = vat_group,
    code = vat_codes,
    item = vat_name,
    vat_rate_new = round(as.numeric(vat_post)/100,3),
  )
  
  excise_map <- tibble::tibble(
    group = excise_group,
    code = excise_codes,
    item = excise_name,
    excise_rate_new = round(as.numeric(excise_post)/100,3),
  )


  curr_df <- curr_df %>%
    select(hhid, group, code, item, hh_g05, vat_rate, excise_rate) %>% 
    mutate(group = as.character(group),
           code = as.character(code),
           item = as.character(item))
  
  curr_df <- curr_df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
  
  
  temp_df <- curr_df %>%
    dplyr::left_join(vat_map,    by = c("group","code","item")) %>%
    dplyr::left_join(excise_map, by = c("group","code","item")) %>%
    # Fallback to baseline if a post value is missing
    dplyr::mutate(
      vat_rate_new    = ifelse(is.na(vat_rate_new), vat_rate, vat_rate_new),
      excise_rate_new = ifelse(is.na(excise_rate_new), excise_rate, excise_rate_new)
    ) %>%
    select(hhid, code, hh_g05, vat_rate_new, excise_rate_new) %>% 
    # Remove indirect taxes from taxed spending to get base
    dplyr::mutate(
      pre_excise = hh_g05 / (1 + vat_rate_new)
    ) %>%
    dplyr::mutate(
      spending_wo_indirect = pre_excise / (1 + excise_rate_new)
    ) %>%
    dplyr::mutate(
      excise = spending_wo_indirect * (excise_rate_new)
    ) %>%
    dplyr::mutate(
      vat = (spending_wo_indirect + excise) * (vat_rate_new)
    )     %>%
    dplyr::group_by(hhid) %>%
    dplyr::summarise(
      itx_vatx_hh = sum(vat,    na.rm = TRUE),  # keep your periodicity factor
      itx_excx_hh = sum(excise, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      itx_vatx_hh = ifelse(is.na(itx_vatx_hh), 0, itx_vatx_hh),
      itx_excx_hh = ifelse(is.na(itx_excx_hh), 0, itx_excx_hh)
    )
  
  temp_df <- temp_df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
  
  return(temp_df)
}


# curr_df needs: hhid, pid, decile, elec_cons_m (monthly MWK, domestic), elec_enter (annual MWK, enterprise)
get_electricity_subsidy <- function(
    curr_df,
    elec_rate_subsidized,   # MWK/kWh (UI)
    elec_block1_kwh,        # kWh/month (UI)
    elec_rate_block2,       # MWK/kWh (UI)
    elec_ref_rate_block1 = 50,     # MWK/kWh, note (“without subsidies”)
    shr_sub_firm = 15  # Assumption from your doc
) {
  
  tmp <- curr_df %>%
    dplyr::mutate(
      elec_cons_m = ifelse(is.na(elec_cons_m), 0, elec_cons_m),
      elec_enter  = ifelse(is.na(elec_enter),  0, elec_enter),
      elec_cons  = ifelse(is.na(elec_cons),  0, elec_cons),
      elec_rate_subsidized = elec_rate_subsidized,
      elec_block1_kwh = elec_block1_kwh,
      elec_rate_block2 = elec_rate_block2,
      elec_ref_rate_block1 = elec_ref_rate_block1,
      shr_sub_firm = shr_sub_firm
    ) 
  


    tmp <- tmp %>%
    mutate(hh_l01 = ifelse(is.na(hh_l01),0,hh_l01),
           hh_l02 = ifelse(is.na(hh_l02),0,hh_l02)) %>% 
    mutate(
      
      EC = ifelse((elec_cons_m <= (elec_block1_kwh * elec_rate_subsidized)) & (hh_l01 != 1) & (hh_l02 != 506), 
                  elec_cons_m/elec_rate_subsidized,
                  ifelse(
                    (elec_cons_m > (elec_block1_kwh * elec_rate_subsidized)) & !(hh_l01 == 1) & !(hh_l02 == 506), 
                    elec_block1_kwh + (elec_cons_m - elec_block1_kwh * elec_rate_subsidized) / elec_rate_block2, 
                    ifelse((hh_l01 == 1) & (hh_l02 == 506), elec_cons_m / 96, 0)
                  ))) %>% 
    mutate(
      ## Cosh without subsidy
      cwsa = ifelse(i_air == 0, EC * elec_rate_block2*12, 
                    ifelse(i_air == 1, EC * 96*12, 0))
    ) 
  
  tmp <- tmp %>%
    mutate(
      elec_subsidy = cwsa - elec_cons,
      elec_enter_sub = elec_enter*shr_sub_firm/100 # For firm
    ) %>% 
    mutate(
      elec_enter_sub = ifelse(is.na(elec_enter_sub), 0, elec_enter_sub),
      elec_subsidy  = ifelse(is.na(elec_subsidy),  0, elec_subsidy)
    ) %>%
    mutate(sub_electri_hh = elec_enter + elec_subsidy)
  
  tmp
}


# Central place to fetch GDP (MWK) once. Replace the fallback with your baseline constant if you like.
get_gdp_mwk <- function() {
  if (!is.null(getOption("ceq.gdp_mwk"))) return(getOption("ceq.gdp_mwk"))
  # Fallback to your Stata constant: 8,518,000,000,000 MWK
  8518000000000
}

# curr_df needs: hhid, pid, decile, fuel_con (annual MWK), fuel_enter (annual MWK), optional: weight
get_fuel_subsidy_from_pct_gdp <- function(
    curr_df,
    fuel_subsidy_pct_gdp    # e.g., 0.243
) {
  gdp_mwk <- get_gdp_mwk()


  subsidy_total <- gdp_mwk * (fuel_subsidy_pct_gdp / 100)
  tfc_total     <- sum(curr_df$tfc1 * curr_df$weight, na.rm = TRUE)
  share         <- if (tfc_total > 0) subsidy_total / tfc_total else 0

  curr_df <- curr_df %>%
    dplyr::mutate(tfc_hh = pmax(0, tfc1 * share)) %>%
    dplyr::select(hhid, tfc_hh)
  
  out <- curr_df %>%
    dplyr::mutate(tfc_hh = ifelse(is.na(tfc_hh), 0, tfc_hh)) %>% 
    rename(sub_fuel_hh = tfc_hh) 

  # out <- curr_df %>%
  #   dplyr::select(hhid, pid, decile) %>%
  #   dplyr::left_join(hh, by = "hhid") %>%
  #   dplyr::mutate(tfc_hh = ifelse(is.na(tfc_hh), 0, tfc_hh)) %>% 
  #   rename(sub_fuel_hh = tfc_hh) %>% 
  #   group_by(hhid) %>% 
  #   dplyr::summarise(
  #     sub_fuel_hh = sum(sub_fuel_hh,  na.rm = TRUE)
  #   ) %>% 
  #   select(hhid, sub_fuel_hh) 

  out
}




get_coorporate_tx <- function(corp_tax_1, corp_tax_2, corp_tax_3, corp_tax_4, 
                              corp_tax_5,corp_tax_6,corp_tax_7,corp_tax_8, corp_tax_9,
                              corp_tax_10,corp_tax_11,corp_tax_12,corp_tax_13,corp_tax_14,
                              corp_tax_15,corp_tax_16,corp_tax_17,corp_tax_18,
                              remove_agriculture_exemption,remove_electricity_exemption) {
  
  
  #browser()
  # Assign tax rates to enterprises
  temp_tx <- bl_df_firm %>% 
    dplyr::select(hhid, pid, industry_sectors, hh_n09a, hh_n21a, hh_n21b, hh_n15b, hh_n40, tax_rate) %>%
    mutate(
      # Assign sector-specific rate
      tax_rate_new = dplyr::case_when(
        industry_sectors == 1 ~ corp_tax_1,
        industry_sectors == 2 ~ corp_tax_2,
        industry_sectors == 3 ~ corp_tax_3,
        industry_sectors == 4 ~ corp_tax_4,
        industry_sectors == 5 ~ corp_tax_5,
        industry_sectors == 6 ~ corp_tax_6,
        industry_sectors == 7 ~ corp_tax_7,
        industry_sectors == 8 ~ corp_tax_8,
        industry_sectors == 9 ~ corp_tax_9,
        industry_sectors == 10 ~ corp_tax_10,
        industry_sectors == 11 ~ corp_tax_11,
        industry_sectors == 12 ~ corp_tax_12,
        industry_sectors == 13 ~ corp_tax_13,
        industry_sectors == 14 ~ corp_tax_14,
        industry_sectors == 15 ~ corp_tax_15,
        industry_sectors == 16 ~ corp_tax_16,
        industry_sectors == 17 ~ corp_tax_17,
        industry_sectors == 18 ~ corp_tax_18,
        .default = tax_rate
      )) 
  
  temp_tx <- temp_tx %>%
    mutate(
      tax_rate_new = ifelse((industry_sectors == 1) & (hh_n15b >= 2010) & (remove_agriculture_exemption=="No"), 0,
                            ifelse((industry_sectors == 1) & (hh_n15b >= 2010) & (remove_agriculture_exemption=="Yes"), 15, 
                                   ifelse((industry_sectors == 1) & (hh_n15b < 2010), 15, tax_rate_new))))
  
  
  temp_tx <- temp_tx %>%
    mutate(
      tax_rate_new = ifelse((industry_sectors == 4) & (hh_n15b >= 2010) & (remove_electricity_exemption=="No"), 0,
                            ifelse((industry_sectors == 4) & (hh_n15b >= 2010) & (remove_electricity_exemption=="Yes"), 15, 
                                   ifelse((industry_sectors == 4) & (hh_n15b < 2010), 15, tax_rate_new))))
  
  temp_tx <- temp_tx %>%
    #
    mutate(tax_rate_new = ifelse((hh_n21a == 1) & (hh_n21b == 1), 30,tax_rate_new))
  
  temp_tx <- temp_tx %>%
    # mutate(
    #   tax_rate_new = ifelse(is.na(tax_rate_new), 15, tax_rate_new)
    # )  %>%
    mutate(tax_rate_new = ifelse((hh_n40 <= 1000), 0, tax_rate_new) %>% structure(label="Corporate tax rate"))
  
  
  
  temp_tx <- temp_tx %>% 
    mutate(
      tax = hh_n40 * (tax_rate_new / 100),
      pid=1
    ) 
  
  
  
  temp_tx <- temp_tx %>%
    dplyr::group_by(hhid) %>%
    mutate(
      dtx_payt_hh = sum(tax, na.rm = TRUE) %>% structure(label="Corporate direct tax")
    ) %>% 
    ungroup() %>% 
    filter(hh_n09a==1) %>% 
    filter(!is.na(hh_n09a)) %>% 
    dplyr::select(hhid, pid, dtx_payt_hh) %>% 
    mutate(i_dtx_payt_hh = ifelse(dtx_payt_hh>0,1,0) %>% structure(label="HH payed corporate tax")) 
  
  
  
  temp_tx
}

