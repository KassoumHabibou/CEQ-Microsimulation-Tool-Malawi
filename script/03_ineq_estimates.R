####################################################################################
######################## Inequality estimates ######################################
####################################################################################

simulate_ineq_est <- function(
    # Direct taxes
  ## Income taxes
  tax_rate_lowest,tax_rate_second,tax_rate_middle,tax_rate_top,
  
  ## Coorporate tax
  corp_tax_1, corp_tax_2, corp_tax_3, corp_tax_4, 
  corp_tax_5,corp_tax_6,corp_tax_7,corp_tax_8, corp_tax_9,
  corp_tax_10,corp_tax_11,corp_tax_12,corp_tax_13,corp_tax_14,
  corp_tax_15,corp_tax_16,corp_tax_17,corp_tax_18, 
  remove_agriculture_exemption, remove_electricity_exemption,
  
  ## Indirect taxes
  
  vat_rate,
  excise_item_914,   # Wine or commercial liquor (250%)
  excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
  excise_item_913,   # Traditional beer (masese) (40%)
  excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
  excise_item_5801,  # Radio with flash drive/micro CD (20%)
  excise_item_211,   # Diesel (10%)
  excise_item_813,   # Tomato sauce (bottle) (10%)
  excise_item_333,   # Umbrella (10%)
  excise_item_909,   # Bottled water (5%)
  excise_item_519,   # Mini-bus (5%)
  excise_item_517,   # Motorcycle/scooter (5%)
  excise_item_611,   # Tractor (5%)
  
  # Items with zero excise rates
  excise_item_506,   # Pork (0%)
  excise_item_507,   # Mutton (0%)
  excise_item_106,   # Rice (0%)
  excise_item_111,   # Bread (0%)
  excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
  excise_item_801,   # Sugar (0%)
  excise_item_803,   # Cooking oil (0%)
  excise_item_810    # Salt (0%)
) {
  
  sim_df <- bl_df %>% 
    dplyr::select(hhid, pid, weight, i22_return_a, dct_hh, dtr_nct_hh, 
                  sub_electri_hh, sub_fuel_hh, yp_hh, 
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
  
  
  ######################## Corporate taxe #########################################  
  temp_d1 <- get_coorporate_tx(
    corp_tax_1, corp_tax_2, corp_tax_3, corp_tax_4, 
    corp_tax_5,corp_tax_6,corp_tax_7,corp_tax_8, corp_tax_9,
    corp_tax_10,corp_tax_11,corp_tax_12,corp_tax_13,corp_tax_14,
    corp_tax_15,corp_tax_16,corp_tax_17,corp_tax_18, 
    remove_agriculture_exemption, remove_electricity_exemption
  )
  
  sim_df <- sim_df %>%
    left_join(temp_d1)
  
  ## Missing values
  sim_df <- sim_df %>%
    mutate(dtx_payt_hh = ifelse(is.na(dtx_payt_hh),0,dtx_payt_hh),
           i_dtx_payt_hh = ifelse(is.na(i_dtx_payt_hh),0,i_dtx_payt_hh),
           dtx_PIT_hh = ifelse(is.na(dtx_PIT_hh),0,dtx_PIT_hh)
    )
  
  temp_d2 <- get_tx(bl_itx, vat_rate,
                    excise_item_914,   # Wine or commercial liquor (250%)
                    excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
                    excise_item_913,   # Traditional beer (masese) (40%)
                    excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
                    excise_item_5801,  # Radio with flash drive/micro CD (20%)
                    excise_item_211,   # Diesel (10%)
                    excise_item_813,   # Tomato sauce (bottle) (10%)
                    excise_item_333,   # Umbrella (10%)
                    excise_item_909,   # Bottled water (5%)
                    excise_item_519,   # Mini-bus (5%)
                    excise_item_517,   # Motorcycle/scooter (5%)
                    excise_item_611,   # Tractor (5%)
                    
                    # Items with zero excise rates
                    excise_item_506,   # Pork (0%)
                    excise_item_507,   # Mutton (0%)
                    excise_item_106,   # Rice (0%)
                    excise_item_111,   # Bread (0%)
                    excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
                    excise_item_801,   # Sugar (0%)
                    excise_item_803,   # Cooking oil (0%)
                    excise_item_810    # Salt (0%)
                    
  )
  
  
  
  sim_df <- sim_df %>%
    left_join(temp_d2)
  
  sim_df <- sim_df %>%
    mutate(
      dtx_all_hh = (dtx_PIT_hh + dtx_payt_hh) %>% structure(label="All direct taxes paid, HH total"),
      dtr_all_hh = (dct_hh + dtr_nct_hh) %>% structure(label= "All direct transfers, HH total"), 
      sub_all_hh = (sub_electri_hh + sub_fuel_hh) %>% structure(label="All indirect subsidies, HH total"), 
      itx_all_hh = (itx_vatx_hh + itx_excx_hh) %>% structure(label="All indirect taxes, HH total")
    )
  
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
  
  lst_pline = c("pline_mod", "pline_mod_low", "pline_mod_middle")
  lst_area = c("Country", "Rural", "Urban")
  
  # Use map_dfr to iterate over Area and Pline to estimate different indicators
  sim_data_tab <- map_dfr(lst_area, function(curr_area) {
      bind_rows(
        get_ineq_indicator(curr_area, sim_df)
      )
    })

  
  sim_data_tab <- sim_data_tab %>%
    mutate(across(c(Income, Parameter, Area), as.character))
  
  bl_ineq <- bl_ineq %>%
    mutate(across(c(Income, Parameter, Area), as.character))
  
  
  sim_data_tab <- bl_ineq %>% 
    left_join(sim_data_tab, by=c("Income","Parameter","Area")) 
  
  
  return(sim_data_tab)
}



get_ineq_indicator <- function(curr_area, df) {
  
  if(curr_area == "Country"){
    curr_df <- df
  }else{
    curr_df <- df %>% 
      filter(reside==curr_area)
  }
  
  # Gini coefficient for each welfare concept
  gini_tab <- tibble(
    gini_yp_pc = DescTools::Gini(curr_df$yp_pc, weights = curr_df$weight, na.rm = TRUE),
    gini_yn_pc = DescTools::Gini(curr_df$yn_pc, weights = curr_df$weight, na.rm = TRUE),
    gini_yg_pc = DescTools::Gini(curr_df$yg_pc, weights = curr_df$weight, na.rm = TRUE),
    gini_yd_pc = DescTools::Gini(curr_df$yd_pc, weights = curr_df$weight, na.rm = TRUE),
    gini_yc_pc = DescTools::Gini(curr_df$yc_pc, weights = curr_df$weight, na.rm = TRUE),
    gini_yf_pc = DescTools::Gini(curr_df$yf_pc, weights = curr_df$weight, na.rm = TRUE)
  )
  
  # Theil index for each welfare concept
  theil_tab <- tibble(
    theil_yp_pc = wINEQ::Theil_L(curr_df$yp_pc, W = curr_df$weight),
    theil_yn_pc = wINEQ::Theil_L(curr_df$yn_pc, W = curr_df$weight),
    theil_yg_pc = wINEQ::Theil_L(curr_df$yg_pc, W = curr_df$weight),
    theil_yd_pc = wINEQ::Theil_L(curr_df$yd_pc, W = curr_df$weight),
    theil_yc_pc = wINEQ::Theil_L(curr_df$yc_pc, W = curr_df$weight),
    theil_yf_pc = wINEQ::Theil_L(curr_df$yf_pc, W = curr_df$weight)
  )
  
  # 90/10 ratio for each welfare concept
  p9010_tab <- tibble(
    p9010_yp_pc = {
      q <- wtd.quantile(curr_df$yp_pc, weights = curr_df$weight, probs = c(0.10, 0.90), na.rm = TRUE)
      q[2] / q[1]
    },
    p9010_yn_pc = {
      q <- wtd.quantile(curr_df$yn_pc, weights = curr_df$weight, probs = c(0.10, 0.90), na.rm = TRUE)
      q[2] / q[1]
    },
    p9010_yg_pc = {
      q <- wtd.quantile(curr_df$yg_pc, weights = curr_df$weight, probs = c(0.10, 0.90), na.rm = TRUE)
      q[2] / q[1]
    },
    p9010_yd_pc = {
      q <- wtd.quantile(curr_df$yd_pc, weights = curr_df$weight, probs = c(0.10, 0.90), na.rm = TRUE)
      q[2] / q[1]
    },
    p9010_yc_pc = {
      q <- wtd.quantile(curr_df$yc_pc, weights = curr_df$weight, probs = c(0.10, 0.90), na.rm = TRUE)
      q[2] / q[1]
    },
    p9010_yf_pc = {
      q <- wtd.quantile(curr_df$yf_pc, weights = curr_df$weight, probs = c(0.10, 0.90), na.rm = TRUE)
      q[2] / q[1]
    }
  )
  

  # Poverty measurements matrice
  summary_tab <- as.data.frame(cbind(t(gini_tab), t(theil_tab), t(p9010_tab),
                                     Income =  c("Market Income plus pensions", "Net Market Income",
                                                 "Gross Income","Disposable Income","Consumable Income","Final Income"))) %>% 
    pivot_longer(cols = starts_with("V"),
                 names_to = "Parameter",
                 values_to = "Current Policy",
                 values_drop_na = TRUE) %>% 
    mutate(Parameter = recode(Parameter,
                              V1 = "Gini index",
                              V2 = "Theil index",
                              V3 = "90/10 income ratio"),
           `Current Policy` = round(as.numeric(`Current Policy`),2),
           Area = curr_area) 
  
  return(summary_tab)
}





