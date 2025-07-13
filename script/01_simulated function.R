

simulate_tax_policy <- function(bl_df,bl_cncpts,tax_rate_lowest,tax_rate_second,tax_rate_middle,tax_rate_top) {
  
  sim_df <- bl_df

  # STEP 1: Apply the payroll tax logic to individual-level income (i22_return_a) # nolint
  sim_df <- sim_df %>%
    mutate(
      p_tax = case_when(
        i22_return_a <= 1800000 ~ i22_return_a * tax_rate_lowest,
        i22_return_a > 1800000 & i22_return_a <= 6000000 ~ i22_return_a * tax_rate_second, # nolint
        i22_return_a > 6000000 & i22_return_a <= 30600000 ~ i22_return_a * tax_rate_middle,
        i22_return_a > 30600000 ~ i22_return_a * tax_rate_top,
        TRUE ~ NA
      )
    )
  
  # STEP 2: Aggregate payroll taxes to household level
  sim_df <- sim_df %>%
    group_by(hhid) %>%
    mutate(dtx_PIT_hh = sum(p_tax, na.rm = TRUE)) %>%
    ungroup()
  
  # STEP 3: Compute total direct taxes at household level
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
      yg_hh = yp_hh + dtr_all_hh,
      yn_hh = yp_hh - dtx_all_hh
    ) %>%
    mutate(
      yd_hh = yg_hh - dtx_all_hh
    ) %>%
    mutate(
      yc_hh = yd_hh + sub_all_hh - itx_all_hh
    ) %>%
    mutate(
      yf_hh = yc_hh + educ_hh + health_hh - Users_fee_hh
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
           pline_mod_middle = 1114.8*365)
  
  lst_pline = c("pline_mod", "pline_mod_low", "pline_mod_middle")
  lst_area = c("Country", "Rural", "Urban")
  
  # Use map_dfr to iterate over Area and Pline to estimate different indicators
  Sim_data_tab <- map_dfr(lst_pline, function(curr_pline) {    
    map_dfr(lst_area, function(curr_area) {
        compute_poverty_indicators(curr_pline, curr_area, sim_df) 
    })
  }) %>% 
    mutate(`Poverty line` = recode(`Poverty line`,
                                   pline_mod = "National poverty line (454 MWK per day)",
                                   pline_mod_low = "Lower income class poverty line (656.7 MKW per day)",
                                   pline_mod_middle = "Middle income class poverty line (1114.8 MKW per day)")
    )
  
  Sim_data_tab <- Sim_data_tab %>%
    mutate(across(c(Income, Parameter, Area, `Poverty line`), as.character))
  
  bl_cncpts <- bl_cncpts %>%
    mutate(across(c(Income, Parameter, Area, `Poverty line`), as.character))
  

  Sim_data_tab <- bl_cncpts %>% 
    left_join(Sim_data_tab, by=c("Income","Parameter","Area","Poverty line"))
  
  
  return(Sim_data_tab)
}



compute_poverty_indicators <- function(curr_pline, curr_area, df) {

  if(curr_area == "Country"){
    tempdf <- df
  }else{
    tempdf <- df %>% 
      filter(reside==curr_area)
  }
  # Poverty Gap indicator as proportion of poverty line
  tempdf <- tempdf %>%
    mutate(
      # Individual poverty gap (how much income falls short of poverty line)
      pov_gap_yd_pc = pmax(0, !!sym(curr_pline) - yd_pc) / !!sym(curr_pline),
      pov_gap_yp_pc = pmax(0, !!sym(curr_pline) - yp_pc) / !!sym(curr_pline),
      pov_gap_yg_pc = pmax(0, !!sym(curr_pline) - yg_pc) / !!sym(curr_pline),
      pov_gap_yc_pc = pmax(0, !!sym(curr_pline) - yc_pc) / !!sym(curr_pline),
      pov_gap_yn_pc = pmax(0, !!sym(curr_pline) - yn_pc) / !!sym(curr_pline),
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
  poverty_headcount_ratio <- tempdf %>%
    summarise(
      pov_rate_yd_pc = sum(weight[yd_pc < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yp_pc  = sum(weight[yp_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yg_pc  = sum(weight[yg_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yn_pc  = sum(weight[yn_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yc_pc  = sum(weight[yc_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE),
      pov_rate_yf_pc  = sum(weight[yf_pc  < !!sym(curr_pline)], na.rm = TRUE) * 100/ sum(weight, na.rm = TRUE)
    )
  
  # Compute number of poor 
  nbr_poor <- tempdf %>%
    summarise(
      nbr_poor_yd_pc  = sum(weight[yd_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yp_pc  = sum(weight[yp_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yg_pc  = sum(weight[yg_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yn_pc  = sum(weight[yn_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yc_pc  = sum(weight[yc_pc  < !!sym(curr_pline)], na.rm = TRUE),
      nbr_poor_yf_pc  = sum(weight[yf_pc  < !!sym(curr_pline)], na.rm = TRUE)
    )
  
  # Poverty Gap
  poverty_gap <- tibble(
    pov_gap_yd_pc  = weighted.mean(tempdf$pov_gap_yd_pc,  tempdf$weight, na.rm = TRUE)*100,
    pov_gap_yp_pc  = weighted.mean(tempdf$pov_gap_yp_pc,  tempdf$weight, na.rm = TRUE)*100,
    pov_gap_yg_pc  = weighted.mean(tempdf$pov_gap_yg_pc,  tempdf$weight, na.rm = TRUE)*100,
    pov_gap_yn_pc  = weighted.mean(tempdf$pov_gap_yn_pc,  tempdf$weight, na.rm = TRUE)*100,
    pov_gap_yc_pc  = weighted.mean(tempdf$pov_gap_yc_pc,  tempdf$weight, na.rm = TRUE)*100,
    pov_gap_yf_pc  = weighted.mean(tempdf$pov_gap_yf_pc,  tempdf$weight, na.rm = TRUE)*100
  )
  
  # Poverty severity index - squared poverty gap measure
  poverty_sev <- tibble(
    pov_sev_yd_pc  = weighted.mean(tempdf$pov_gap_yd_pc_sqrd,  tempdf$weight, na.rm = TRUE),
    pov_sev_yp_pc  = weighted.mean(tempdf$pov_gap_yp_pc_sqrd,  tempdf$weight, na.rm = TRUE),
    pov_sev_yg_pc  = weighted.mean(tempdf$pov_gap_yg_pc_sqrd,  tempdf$weight, na.rm = TRUE),
    pov_sev_yn_pc  = weighted.mean(tempdf$pov_gap_yn_pc_sqrd,  tempdf$weight, na.rm = TRUE),
    pov_sev_yc_pc  = weighted.mean(tempdf$pov_gap_yc_pc_sqrd,  tempdf$weight, na.rm = TRUE),
    pov_sev_yf_pc  = weighted.mean(tempdf$pov_gap_yf_pc_sqrd,  tempdf$weight, na.rm = TRUE)
  )
  
  #browser()
  # Poverty measurements matrice
  summary_tab <- as.data.frame(cbind(t(poverty_headcount_ratio), t(nbr_poor), t(poverty_gap), t(poverty_sev), 
                                Income = c("Disposable Income","Market Income plus pensions",
                                          "Gross Income","Net Market Income","Consumable Income","Final Income"))) %>% 
    pivot_longer(
      cols = starts_with("V"),
      names_to = "Parameter",
      values_to = "Current Policy",
      values_drop_na = TRUE) %>%
    mutate(Parameter = recode(Parameter,
                              V1 = "Rate of poverty",
                              V2 = "Number of poor",
                              V3 = "Poverty gap",
                              V4 = "Poverty severity"),
           `Current Policy` = round(as.numeric(`Current Policy`),2),
            Area = curr_area,
           `Poverty line` = curr_pline) 

  return(summary_tab)
}



