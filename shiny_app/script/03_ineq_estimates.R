####################################################################################
######################## Inequality estimates ######################################
####################################################################################

simulate_ineq_est <- function(simulated_df) {
  sim_df <- simulated_df()
  
  # Poverty line
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
                 values_to = "Post-reform",
                 values_drop_na = TRUE) %>% 
    mutate(Parameter = recode(Parameter,
                              V1 = "Gini index",
                              V2 = "Theil index",
                              V3 = "90/10 income ratio"),
           `Post-reform` = round(as.numeric(`Post-reform`),2),
           Area = curr_area) 
  
  return(summary_tab)
}





