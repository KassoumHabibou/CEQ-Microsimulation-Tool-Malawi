####################################################################################
######################## Poverty estimates #########################################
####################################################################################

simulate_pov_est <- function(simulated_df) {
  sim_df <- simulated_df()
  
  # List 
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
  
  # Tot welfare
  poverty_welf <- tibble(
    pov_welf_yp_pc  = weighted.mean(curr_df$yp_pc,  curr_df$weight, na.rm = TRUE),
    pov_welf_yn_pc  = weighted.mean(curr_df$yn_pc,  curr_df$weight, na.rm = TRUE),
    pov_welf_yg_pc  = weighted.mean(curr_df$yg_pc,  curr_df$weight, na.rm = TRUE),
    pov_welf_yd_pc  = weighted.mean(curr_df$yd_pc,  curr_df$weight, na.rm = TRUE),
    pov_welf_yc_pc  = weighted.mean(curr_df$yc_pc,  curr_df$weight, na.rm = TRUE),
    pov_welf_yf_pc  = weighted.mean(curr_df$yf_pc,  curr_df$weight, na.rm = TRUE)
  )
  
  # Poverty measurements matrice
  summary_tab <- as.data.frame(cbind(t(poverty_headcount_ratio), t(nbr_poor), t(poverty_gap), t(poverty_sev), t(poverty_welf), 
                                Income = c("Market Income plus pensions", "Net Market Income",
                                          "Gross Income","Disposable Income","Consumable Income","Final Income"))) %>% 
    pivot_longer(
      cols = starts_with("V"),
      names_to = "Parameter",
      values_to = "Post-reform",
      values_drop_na = TRUE) %>%
    mutate(Parameter = recode(Parameter,
                              V1 = "Rate of poverty",
                              V2 = "Number of poor",
                              V3 = "Poverty gap",
                              V4 = "Poverty severity",
                              V5 = "Welfare"
                              ),
           `Post-reform` = round(as.numeric(`Post-reform`),2),
            Area = curr_area,
           `Poverty line` = curr_pline) 

  return(summary_tab)
}

