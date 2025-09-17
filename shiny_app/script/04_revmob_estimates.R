####################################################################################
######################## Revenue mobilisation estimates ############################
####################################################################################

simulate_revmob_est <- function(simulated_df) {
  sim_df <- simulated_df()
  
  #Areas
  lst_area = c("Country", "Rural", "Urban")
  
  # Use map_dfr to iterate over Area and Pline to estimate different indicators
  sim_data_tab <- map_dfr(lst_area, function(curr_area) {
      bind_rows(
        get_revmob(curr_area, sim_df)
      )
    })
  
  sim_data_tab <- sim_data_tab %>%
    mutate(across(c(Parameter, Area), as.character))
  

  sim_data_tab <- bl_revmob %>% 
    left_join(sim_data_tab, by=c("Parameter","Area"))  
  
  # Order
  sim_data_tab <- sim_data_tab %>% 
    select(Parameter,  Area, `Pre-reform`, `Post-reform`)
  
  sim_data_tab <- sim_data_tab %>% 
    mutate(
      diff = (`Post-reform` - `Pre-reform`),
      impact = round((`Post-reform` - `Pre-reform`)*100/(`Pre-reform`),2))
  
  return(sim_data_tab)
}





get_revmob <- function(curr_area, sim_df) {
  
  if(curr_area == "Country"){
    curr_df1 <- sim_df

  }else{
    
    curr_df1 <- sim_df %>% 
      filter(reside==curr_area)
  }
  
  rev_mob <- tibble(
    dtx_all  = sum(curr_df1$dtx_all_hh * curr_df1$weight, na.rm = TRUE),
    itx_all  = sum(curr_df1$itx_all_hh * curr_df1$weight, na.rm = TRUE),
    dtr_all  = sum(curr_df1$dtr_all_hh * curr_df1$weight, na.rm = TRUE),
    sub_all  = sum(curr_df1$sub_all_hh * curr_df1$weight, na.rm = TRUE)
  )
  
  
  

  # Poverty measurements matrice
  summary_tab <- as.data.frame(cbind(t(rev_mob),
                                     Parameter =  c("Direct taxes", "Indirect taxes",
                                                    "Direct transfers","Indirect subsidies"))) %>% 
    pivot_longer(cols = starts_with("V"),
                 #names_to = "Parameter",
                 values_to = "Post-reform",
                 values_drop_na = TRUE) %>% 
    mutate(`Post-reform` = round(as.numeric(`Post-reform`),2), Area = curr_area) %>% 
    select(Parameter, Area, `Post-reform`)
  
  return(summary_tab)
}


