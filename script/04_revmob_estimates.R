####################################################################################
######################## Revenue mobilisation estimates ############################
####################################################################################

simulate_revmob_est <- function(
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
           dtx_PIT_hh = ifelse(is.na(dtx_PIT_hh),0,dtx_PIT_hh))
  
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
  
  

  # sim_df <- sim_df %>%
  #   left_join(temp_d2)

  temp_d2 <- temp_d2 %>% 
    left_join(sim_df %>% select(hhid, reside, weight))
  
  temp_d2 <- temp_d2 %>% 
    mutate(itx_vatx_hh=ifelse(is.na(itx_vatx_hh),0,itx_vatx_hh),
           itx_excx_hh=ifelse(is.na(itx_excx_hh),0,itx_excx_hh))
  
  sim_df <- sim_df %>%
    mutate(
      dtx_all_hh = (dtx_PIT_hh + dtx_payt_hh) %>% structure(label="All direct taxes paid, HH total"),
      dtr_all_hh = (dct_hh + dtr_nct_hh) %>% structure(label= "All direct transfers, HH total"), 
      sub_all_hh = (sub_electri_hh + sub_fuel_hh) %>% structure(label="All indirect subsidies, HH total")
    )
  
  temp_d2 <- temp_d2 %>%
    mutate(
      itx_all_hh = (itx_vatx_hh + itx_excx_hh) %>% structure(label="All indirect taxes, HH total")
    )
  
  
  lst_area = c("Country", "Rural", "Urban")
  
  # Use map_dfr to iterate over Area and Pline to estimate different indicators
  sim_data_tab <- map_dfr(lst_area, function(curr_area) {
      bind_rows(
        get_revmob(curr_area, sim_df, temp_d2)
      )
    })
  
  sim_data_tab <- sim_data_tab %>%
    mutate(across(c(Parameter, Area), as.character))
  

  sim_data_tab <- bl_revmob %>% 
    left_join(sim_data_tab, by=c("Parameter","Area"))  
  
  
  return(sim_data_tab)
}





get_revmob <- function(curr_area, sim_df, temp_d2) {
  
  if(curr_area == "Country"){
    curr_df1 <- sim_df
    curr_df2 <- temp_d2
  }else{
    
    curr_df1 <- sim_df %>% 
      filter(reside==curr_area)
    
    curr_df2 <- temp_d2 %>% 
      filter(reside==curr_area)
  }
  
  rev_mob <- tibble(
    dtx_all  = sum(curr_df1$dtx_all_hh * curr_df1$weight, na.rm = TRUE),
    itx_all  = sum(curr_df2$itx_all_hh * curr_df2$weight, na.rm = TRUE),
    dtr_all  = sum(curr_df1$dtr_all_hh * curr_df1$weight, na.rm = TRUE),
    sub_all  = sum(curr_df1$sub_all_hh * curr_df1$weight, na.rm = TRUE)
  )
  
  
  

  # Poverty measurements matrice
  summary_tab <- as.data.frame(cbind(t(rev_mob),
                                     Parameter =  c("Direct taxes", "Indirect taxes",
                                                    "Direct transfers","Indirect subsidies"))) %>% 
    pivot_longer(cols = starts_with("V"),
                 #names_to = "Parameter",
                 values_to = "Current Policy",
                 values_drop_na = TRUE) %>% 
    mutate(`Current Policy` = round(as.numeric(`Current Policy`),2), Area = curr_area) %>% 
    select(Parameter, `Current Policy`, Area)
  
  return(summary_tab)
}


