
######################## Poverty estimates #########################################

simulate_tax_policy <- function(tax_rate_lowest,tax_rate_second,tax_rate_middle,tax_rate_top) {
  
  sim_df <- bl_df %>% 
    dplyr::select(hhid, weight, i22_return_a, dtx_payt_hh, dct_hh, dtr_nct_hh, 
                  sub_electri_hh, sub_fuel_hh, itx_vatx_hh, itx_excx_hh, yp_hh, 
                  educ_hh, health_hh, Users_fee_hh, hhsize, reside, pline_mod)

  # STEP 1: Apply the payroll tax logic to individual-level income (i22_return_a) # nolint
  sim_df <- sim_df %>%
    mutate(
      p_tax = case_when(
        i22_return_a <= 1800000 ~ i22_return_a * tax_rate_lowest / 100,
        i22_return_a > 1800000 & i22_return_a <= 6000000 ~ i22_return_a * tax_rate_second / 100, # nolint
        i22_return_a > 6000000 & i22_return_a <= 30600000 ~ i22_return_a * tax_rate_middle / 100,
        i22_return_a > 30600000 ~ i22_return_a * tax_rate_top / 100,
        TRUE ~ NA
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
  sim_data_tab <- map_dfr(lst_pline, function(curr_pline) {    
    tempResults <- map_dfr(lst_area, function(curr_area) {
      bind_rows(
        get_pov_indicator(curr_pline, curr_area, sim_df)
      )
    })
  })
  
  sim_data_tab <- sim_data_tab %>%
    mutate(across(c(Income, Parameter, Area, `Poverty line`), as.character))
  
  bl_cncpts <- bl_cncpts %>%
    mutate(across(c(Income, Parameter, Area, `Poverty line`), as.character))
  

  sim_data_tab <- bl_cncpts %>% 
    left_join(sim_data_tab, by=c("Income","Parameter","Area","Poverty line"))  %>% 
    mutate(`Poverty line` = recode(`Poverty line`,
                                   pline_mod = "National poverty line (454 MWK per day)",
                                   pline_mod_low = "Lower income class poverty line (656.7 MKW per day)",
                                   pline_mod_middle = "Middle income class poverty line (1115 MKW per day)")
    )
  
  
  return(sim_data_tab)
}



get_pov_indicator <- function(curr_pline, curr_area, df) {

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

######################## Geospatial analysis #########################################


simulate_geo_tax_policy <- function(tax_rate_lowest,tax_rate_second,tax_rate_middle,tax_rate_top) {
  
  sim_df <- bl_df %>% 
    dplyr::select(hhid, region, district, weight, i22_return_a, dtx_payt_hh, dct_hh, dtr_nct_hh, 
                  sub_electri_hh, sub_fuel_hh, itx_vatx_hh, itx_excx_hh, yp_hh, 
                  educ_hh, health_hh, Users_fee_hh, hhsize, reside, pline_mod)
  
  # STEP 1: Apply the payroll tax logic to individual-level income (i22_return_a) # nolint
  sim_df <- sim_df %>%
    mutate(
      p_tax = case_when(
        i22_return_a <= 1800000 ~ i22_return_a * tax_rate_lowest / 100,
        i22_return_a > 1800000 & i22_return_a <= 6000000 ~ i22_return_a * tax_rate_second / 100, # nolint
        i22_return_a > 6000000 & i22_return_a <= 30600000 ~ i22_return_a * tax_rate_middle / 100,
        i22_return_a > 30600000 ~ i22_return_a * tax_rate_top / 100,
        TRUE ~ NA
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
  
  # Analysis per region
  geo_pov_per_region <- sim_df %>% 
    group_by(region) %>% 
    group_split() %>%
    map_dfr(., function(splitted_df){
      
      curr_region = first(splitted_df$region)
      
      pov_per_region <-  map_dfr(lst_pline, function(curr_pline) {    
        bind_rows(
          get_geo_pov_indicator(curr_pline, splitted_df) %>%
            mutate(Code_area = as.character(curr_region), 
                   Area = "region") 
        )
      }) 
      
      pov_per_region
    })
  
  
  # Analysis per district
  geo_pov_per_district <- sim_df %>% 
    group_by(district) %>% 
    group_split() %>%
    map_dfr(., function(splitted_df){
      
      curr_district= first(splitted_df$district)
      
      pov_per_district <-  map_dfr(lst_pline, function(curr_pline) {    
        bind_rows(
          get_geo_pov_indicator(curr_pline, splitted_df) %>%
            mutate(Code_area = as.character(curr_district), 
                   Area = "district") 
        )
      }) 
      
      pov_per_district
    })
  
  sim_geo_data_tab <- plyr::rbind.fill(geo_pov_per_region,geo_pov_per_district)
  
  sim_geo_data_tab <- bl_geo_cncpts %>% 
    left_join(sim_geo_data_tab, by=c("Income","Parameter","Code_area","Area","Poverty line"))  %>% 
    mutate(`Poverty line` = recode(`Poverty line`,
                                   pline_mod = "National poverty line (454 MWK per day)",
                                   pline_mod_low = "Lower income class poverty line (656.7 MKW per day)",
                                   pline_mod_middle = "Middle income class poverty line (1115 MKW per day)")
    )
  
  
  return(sim_geo_data_tab)
}



get_geo_pov_indicator <- function(curr_pline, curr_df) {
  
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
           `Poverty line` = curr_pline) 
  
  return(summary_tab)
}



