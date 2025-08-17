####################################################################################
######################## Main simulation estimates #################################
####################################################################################

simulate_main_df <- function(
    # Direct taxes
  # PAYE
  tax_rate_lowest, tax_rate_second, tax_rate_middle, tax_rate_top,
  # Corporate
  corp_tax_1, corp_tax_2, corp_tax_3, corp_tax_4, corp_tax_5,
  corp_tax_6, corp_tax_7, corp_tax_8, corp_tax_9, corp_tax_10,
  corp_tax_11, corp_tax_12, corp_tax_13, corp_tax_14, corp_tax_15,
  corp_tax_16, corp_tax_17, corp_tax_18,
  remove_agriculture_exemption,
  remove_electricity_exemption,
  
  # Indirect taxes
  vat_codes, vat_post,
  excise_codes, excise_post,
  
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
  dtr_onc_hh_decile,   dtr_onc_hh
  
  # Electricity and fuel subsidies
  # elec_rate_subsidized,
  # elec_block1_kwh,
  # elec_rate_block2,
  # fuel_subsidy_pct_gdp
) {
  
  sim_df <- bl_df %>% 
    dplyr::select(hhid, pid, decile, region, district, weight, i22_return_a, 
                  sub_electri_hh, sub_fuel_hh, yp_hh, elec_cons_m, elec_enter,
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
    vat_codes    = vat_codes,
    vat_post     = vat_post,        # % per item
    excise_codes = excise_codes,
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
  sim_df <- sim_df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  
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
  browser()
  temp_elec <- get_electricity_subsidy(
    curr_df              = bl_df %>% dplyr::select(hhid, pid, decile, elec_cons_m, elec_enter),
    elec_rate_subsidized = input$elec_rate_subsidized,
    elec_block1_kwh      = input$elec_block1_kwh,
    elec_rate_block2     = input$elec_rate_block2
  )
  

  # Fuel: UI gives only % of GDP; GDP pulled via get_gdp_mwk()
  temp_fuel <- get_fuel_subsidy_from_pct_gdp(
    curr_df              = bl_df %>% dplyr::select(hhid, pid, decile, fuel_con, fuel_enter, dplyr::any_of("weight")),
    fuel_subsidy_pct_gdp = input$fuel_subsidy_pct_gdp,
    weight_col           = if ("weight" %in% names(bl_df)) "weight" else NULL
  )
  
  
  # Merge back into the simulation frame
  sim_df <- sim_df %>%
    dplyr::left_join(temp_elec, by = c("hhid","pid","decile")) %>%
    dplyr::mutate(sub_electri = ifelse(is.na(sub_electri), 0, sub_electri)) %>%
    dplyr::left_join(temp_fuel, by = c("hhid","pid","decile")) %>%
    dplyr::mutate(tfc_hh = ifelse(is.na(tfc_hh), 0, tfc_hh)) %>%
    dplyr::mutate(indirect_subsidies_hh = pmax(0, sub_electri + tfc_hh))
  
  ####################################################################################
  ######################## Income concepts estimates #################################
  ####################################################################################
  
  
  ######## Direct and inderect taxes ###############################################
  sim_df <- sim_df %>%
    dplyr::mutate(
      dtx_all_hh = (dtx_PIT_hh + dtx_payt_hh) %>% structure(label="All direct taxes paid, HH total"),
      dtr_all_hh = (dct_hh + dtr_nct_hh) %>% structure(label="All direct transfers, HH total"),
      sub_all_hh = (sub_electri_hh + sub_fuel_hh) %>% structure(label="All indirect subsidies, HH total"),
      itx_all_hh = (itx_vatx_hh + itx_excx_hh) %>% structure(label="All indirect taxes, HH total")
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
  
  sim_df <- sim_df %>% mutate(across(where(is.numeric), ~ round(.x, 10)))
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


get_tx <- function(curr_df, vat_codes, vat_post, excise_codes, excise_post) {
  
  
  # Build mapping tables (post rates in FRACTIONS)
  vat_map <- tibble::tibble(
    code = vat_codes,
    vat_rate_new = pmax(0, pmin(100, as.numeric(vat_post))) / 100
  )
  excise_map <- tibble::tibble(
    code = excise_codes,
    excise_rate_new = pmax(0, pmin(1000, as.numeric(excise_post))) / 100
  )

  temp_df <- curr_df %>%
    dplyr::left_join(vat_map,    by = "code") %>%
    dplyr::left_join(excise_map, by = "code") %>%
    # Fallback to baseline if a post value is missing
    dplyr::mutate(
      vat_rate_new    = dplyr::coalesce(vat_rate_new,    vat_rate),
      excise_rate_new = dplyr::coalesce(excise_rate_new, excise_rate)
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
      excise = spending_wo_indirect * excise_rate_new
    ) %>%
    dplyr::mutate(
      vat = (spending_wo_indirect + excise) * vat_rate_new
    )     %>%
    dplyr::group_by(hhid) %>%
    dplyr::summarise(
      itx_vatx_hh = sum(vat * 53,    na.rm = TRUE),  # keep your periodicity factor
      itx_excx_hh = sum(excise* 53, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      itx_vatx_hh = ifelse(is.na(itx_vatx_hh), 0, itx_vatx_hh),
      itx_excx_hh = ifelse(is.na(itx_excx_hh), 0, itx_excx_hh)
    )
  
  return(temp_df)
}

# 
# # curr_df needs: hhid, pid, decile, elec_cons_m (monthly MWK, domestic), elec_enter (annual MWK, enterprise)
# get_electricity_subsidy <- function(
#     curr_df,
#     elec_rate_subsidized,   # MWK/kWh (UI)
#     elec_block1_kwh,        # kWh/month (UI)
#     elec_rate_block2,       # MWK/kWh (UI)
#     elec_ref_rate_block1 = 50,     # MWK/kWh, note (“without subsidies”)
#     elec_enterprise_sub_share = 0.15  # Assumption from your doc
# ) {
#   tmp <- curr_df %>%
#     dplyr::select(hhid, pid, decile, elec_cons_m, elec_enter) %>%
#     dplyr::mutate(
#       elec_cons_m = ifelse(is.na(elec_cons_m), 0, elec_cons_m),
#       elec_enter  = ifelse(is.na(elec_enter),  0, elec_enter)
#     ) %>%
#     mutate(
#            # Estimate monthly kWh from spending under your 2-block pricing
#            kwh_domestic = dplyr::case_when(
#              elec_cons_m <= elec_block1_kwh * elec_rate_subsidized ~ elec_cons_m / elec_rate_subsidized,
#              TRUE ~ elec_block1_kwh + (elec_cons_m - elec_block1_kwh * elec_rate_subsidized) / elec_rate_block2
#            )) %>% 
#     mutate(
# 
#       # Subsidy only on block1: (reference - subsidized) * min(kWh, block1)
#       sub_domestic_m =
#         pmax(0, (elec_ref_rate_block1 - elec_rate_subsidized) * pmin(kwh_domestic, elec_block1_kwh))
#       
#     ) %>% 
#     
#     mutate(
#       
#       # Enterprise component (annual): 15% of reported enterprise electricity spending
#       sub_enterprise_y = pmax(0, elec_enter * elec_enterprise_sub_share),
#       
#       # Total electricity subsidy (annual MWK)
#       sub_electri = pmax(0, sub_domestic_m * 12 + sub_enterprise_y)
#       
#     ) %>% 
#     dplyr::select(hhid, pid, decile, sub_electri)%>% 
#     rename(sub_electri_hh = sub_electri)
#   
#   tmp
# }
# 
# 
# # Central place to fetch GDP (MWK) once. Replace the fallback with your baseline constant if you like.
# get_gdp_mwk <- function() {
#   if (!is.null(getOption("ceq.gdp_mwk"))) return(getOption("ceq.gdp_mwk"))
#   # Fallback to your Stata constant: 8,518,000,000,000 MWK
#   8518000000000
# }
# 
# # curr_df needs: hhid, pid, decile, fuel_con (annual MWK), fuel_enter (annual MWK), optional: weight
# get_fuel_subsidy_from_pct_gdp <- function(
#     curr_df,
#     fuel_subsidy_pct_gdp,    # e.g., 0.243
#     weight_col = "weight"        # e.g., "weight"; NULL for unweighted
# ) {
#   gdp_mwk <- get_gdp_mwk()
#   
#   hh <- curr_df %>%
#     dplyr::mutate(
#       fuel_con   = ifelse(is.na(fuel_con),   0, fuel_con),
#       fuel_enter = ifelse(is.na(fuel_enter), 0, fuel_enter)
#     ) %>%
#     dplyr::group_by(hhid) %>%
#     dplyr::summarise(
#       tfc1 = fuel_con + fuel_enter,
#       w    = if (is.null(weight_col)) 1 else mean(.data[[weight_col]], na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   subsidy_total <- gdp_mwk * (fuel_subsidy_pct_gdp / 100)
#   tfc_total     <- sum(hh$tfc1 * hh$w, na.rm = TRUE)
#   share         <- if (tfc_total > 0) subsidy_total / tfc_total else 0
#   
#   hh <- hh %>%
#     dplyr::mutate(tfc_hh = pmax(0, tfc1 * share)) %>%
#     dplyr::select(hhid, tfc_hh)
#   
#   out <- curr_df %>%
#     dplyr::select(hhid, pid, decile) %>%
#     dplyr::distinct() %>%
#     dplyr::left_join(hh, by = "hhid") %>%
#     dplyr::mutate(tfc_hh = ifelse(is.na(tfc_hh), 0, tfc_hh)) %>% 
#     rename(sub_fuel_hh = tfc_hh)
#   
#   out
# }
