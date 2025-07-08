
after_data_sim <- before_data_wtht_na

# Define tax rates for each income bracket
tax_rate_lowest = 10
tax_rate_second = 20
tax_rate_middle = 40 
tax_rate_top = 50

# ------------------------------------------------------------------------------
# STEP 1: Apply the payroll tax logic to individual-level income (i22_return_a)
# ------------------------------------------------------------------------------
after_data_sim <- after_data_sim %>%
  mutate(
    p_tax = case_when(
      i22_return_a <= 1800000 ~ i22_return_a * tax_rate_lowest,                                   # Apply lowest tax rate (flat amount)
      i22_return_a > 1800000 & i22_return_a <= 6000000 ~ i22_return_a * tax_rate_second, # Apply taxe to income
      i22_return_a > 6000000 & i22_return_a <= 30600000 ~ i22_return_a * tax_rate_middle, # Apply taxe to income
      i22_return_a > 30600000 ~ i22_return_a * tax_rate_top,                         # Apply taxe to of income
      TRUE ~ NA                                                                       # Catch-all for missing/invalid data
    )
  ) 

# ------------------------------------------------------------------------------
# STEP 2: Aggregate payroll taxes to household level
# ------------------------------------------------------------------------------
after_data_sim <- after_data_sim %>%
  group_by(hhid) %>%
  mutate(dtx_PIT_hh = sum(p_tax, na.rm = TRUE)) %>%  # Total personal income tax at HH level
  ungroup()

# ------------------------------------------------------------------------------
# STEP 3: Compute total direct taxes at household level
# ------------------------------------------------------------------------------
after_data_sim <- after_data_sim %>%
  mutate(
    dtx_all_hh = (dtx_PIT_hh + dtx_payt_hh) %>%      # Sum of personal income tax and other payroll taxes
      structure(label = "All direct taxes paid, HH total")
  )

# ------------------------------------------------------------------------------
# STEP 4: Recalculate household-level income concepts
# ------------------------------------------------------------------------------

after_data_sim <- after_data_sim %>%
  mutate(
    yg_hh = yp_hh + dtr_all_hh,                     # Gross Income = Market Income + Pensions + Transfers
    yn_hh = yp_hh - dtx_all_hh                      # Net Market Income = Market Income - Direct Taxes
  ) 

after_data_sim <- after_data_sim %>% 
  mutate(
    yd_hh = yg_hh - dtx_all_hh                      # Disposable Income = Gross Income - Direct Taxes
  ) 


after_data_sim <- after_data_sim %>% 
  mutate(
    yc_hh = yd_hh + sub_all_hh - itx_all_hh         # Consumable Income = Disposable Income + Subsidies - Indirect Taxes
  ) 


after_data_sim <- after_data_sim %>% 
  mutate(
    yf_hh = yc_hh + educ_hh + health_hh - Users_fee_hh  # Final Income = Consumable Income + In-kind Benefits - User Fees
  )

# ------------------------------------------------------------------------------
# STEP 5: Compute per capita versions of income concepts
# ------------------------------------------------------------------------------
after_data_sim <- after_data_sim %>%
  mutate(
    yd_pc = (yd_hh / hhsize) %>% structure(label = "Disposable Income (per capita)"),
    yg_pc = (yg_hh / hhsize) %>% structure(label = "Gross Income (per capita)"),
    yn_pc = (yn_hh / hhsize) %>% structure(label = "Net Market Income (per capita)"),
    yp_pc = (yp_hh / hhsize) %>% structure(label = "Market Income plus pensions (per capita)"),
    yc_pc = (yc_hh / hhsize) %>% structure(label = "Consumable Income (per capita)"),
    yf_pc = (yf_hh / hhsize) %>% structure(label = "Final Income (per capita)")
  )

after_data_sim <- after_data_sim %>%
  mutate(
    # Poverty gap: share of the poverty line by which income falls short (if below line)
    pov_gap_pcrexp = pmax(0, pline_mod - pcrexp) / pline_mod,
    pov_gap_yd_pc = pmax(0, pline_mod - yd_pc) / pline_mod,
    pov_gap_yp_pc = pmax(0, pline_mod - yp_pc) / pline_mod,  # (possibly should be yp_pc instead of yd_pc)
    pov_gap_yg_pc = pmax(0, pline_mod - yg_pc) / pline_mod,
    pov_gap_yc_pc = pmax(0, pline_mod - yc_pc) / pline_mod,
    pov_gap_yn_pc = pmax(0, pline_mod - yn_pc) / pline_mod,
    pov_gap_yf_pc = pmax(0, pline_mod - yf_pc) / pline_mod
  ) %>% 
  mutate(
    # Squared poverty gaps: for computing poverty severity (FGT2 index)
    pov_gap_pcrexp_sqrd = pov_gap_pcrexp^2,
    pov_gap_yd_pc_sqrd = pov_gap_yd_pc^2,
    pov_gap_yp_pc_sqrd = pov_gap_yp_pc^2,
    pov_gap_yg_pc_sqrd = pov_gap_yg_pc^2,
    pov_gap_yn_pc_sqrd = pov_gap_yn_pc^2,
    pov_gap_yc_pc_sqrd = pov_gap_yc_pc^2,
    pov_gap_yf_pc_sqrd = pov_gap_yf_pc^2
  )

poverty_headcount_ratio <- after_data_sim %>%
  summarise(
    pov_rate_pcrexp = sum(weight[pcrexp < pline_mod], na.rm = TRUE) * 100 / sum(weight, na.rm = TRUE),
    pov_rate_yd_pc  = sum(weight[yd_pc  < pline_mod], na.rm = TRUE) * 100 / sum(weight, na.rm = TRUE),
    pov_rate_yp_pc  = sum(weight[yp_pc  < pline_mod], na.rm = TRUE) * 100 / sum(weight, na.rm = TRUE),
    pov_rate_yg_pc  = sum(weight[yg_pc  < pline_mod], na.rm = TRUE) * 100 / sum(weight, na.rm = TRUE),
    pov_rate_yn_pc  = sum(weight[yn_pc  < pline_mod], na.rm = TRUE) * 100 / sum(weight, na.rm = TRUE),
    pov_rate_yc_pc  = sum(weight[yc_pc  < pline_mod], na.rm = TRUE) * 100 / sum(weight, na.rm = TRUE),
    pov_rate_yf_pc  = sum(weight[yf_pc  < pline_mod], na.rm = TRUE) * 100 / sum(weight, na.rm = TRUE)
  )


nbr_poor <- after_data_sim %>%
  summarise(
    nbr_poor_pcrexp = round(sum(weight[pcrexp < pline_mod], na.rm = TRUE)),
    nbr_poor_yd_pc  = round(sum(weight[yd_pc  < pline_mod], na.rm = TRUE)),
    nbr_poor_yp_pc  = round(sum(weight[yp_pc  < pline_mod], na.rm = TRUE)),
    nbr_poor_yg_pc  = round(sum(weight[yg_pc  < pline_mod], na.rm = TRUE)),
    nbr_poor_yn_pc  = round(sum(weight[yn_pc  < pline_mod], na.rm = TRUE)),
    nbr_poor_yc_pc  = round(sum(weight[yc_pc  < pline_mod], na.rm = TRUE)),
    nbr_poor_yf_pc  = round(sum(weight[yf_pc  < pline_mod], na.rm = TRUE))
  )

poverty_gap <- tibble(
  pov_gap_pcrexp = weighted.mean(after_data_sim$pov_gap_pcrexp, after_data_sim$weight, na.rm = TRUE) * 100,
  pov_gap_yd_pc  = weighted.mean(after_data_sim$pov_gap_yd_pc,  after_data_sim$weight, na.rm = TRUE) * 100,
  pov_gap_yp_pc  = weighted.mean(after_data_sim$pov_gap_yp_pc,  after_data_sim$weight, na.rm = TRUE) * 100,
  pov_gap_yg_pc  = weighted.mean(after_data_sim$pov_gap_yg_pc,  after_data_sim$weight, na.rm = TRUE) * 100,
  pov_gap_yn_pc  = weighted.mean(after_data_sim$pov_gap_yn_pc,  after_data_sim$weight, na.rm = TRUE) * 100,
  pov_gap_yc_pc  = weighted.mean(after_data_sim$pov_gap_yc_pc,  after_data_sim$weight, na.rm = TRUE) * 100,
  pov_gap_yf_pc  = weighted.mean(after_data_sim$pov_gap_yf_pc,  after_data_sim$weight, na.rm = TRUE) * 100
)



poverty_sev <- tibble(
  pov_sev_pcrexp = weighted.mean(after_data_sim$pov_gap_pcrexp_sqrd, after_data_sim$weight, na.rm = TRUE),
  pov_sev_yd_pc  = weighted.mean(after_data_sim$pov_gap_yd_pc_sqrd,  after_data_sim$weight, na.rm = TRUE),
  pov_sev_yp_pc  = weighted.mean(after_data_sim$pov_gap_yp_pc_sqrd,  after_data_sim$weight, na.rm = TRUE),
  pov_sev_yg_pc  = weighted.mean(after_data_sim$pov_gap_yg_pc_sqrd,  after_data_sim$weight, na.rm = TRUE),
  pov_sev_yn_pc  = weighted.mean(after_data_sim$pov_gap_yn_pc_sqrd,  after_data_sim$weight, na.rm = TRUE),
  pov_sev_yc_pc  = weighted.mean(after_data_sim$pov_gap_yc_pc_sqrd,  after_data_sim$weight, na.rm = TRUE),
  pov_sev_yf_pc  = weighted.mean(after_data_sim$pov_gap_yf_pc_sqrd,  after_data_sim$weight, na.rm = TRUE)
)

Sim_data_tab <- as.data.frame(cbind(
  t(poverty_headcount_ratio),  # Poverty rate (%)
  t(nbr_poor),                 # Number of poor (absolute count)
  t(poverty_gap),             # Poverty gap (%)
  t(poverty_sev),             # Poverty severity (index)
  Income = c("Real expenditures", "Disposable Income", "Market Income plus pensions",
             "Gross Income", "Net Market Income", "Consumable Income", "Final Income")
)) %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "Parameter",
    values_to = "Policy choice",
    values_drop_na = TRUE
  ) %>%
  mutate(Parameter = recode(Parameter,
                            V1 = "Rate",
                            V2 = "Number of Poor",
                            V3 = "Poverty GAP",
                            V4 = "Poverty Severity"))



#before_data_wtht_na <- curr_df %>% select(hhid, pid, i22_return_a, i_ptax) %>% right_join(before_data_wtht_na, by=c("hhid"="hhid", "pid"="pid"))

#dtr_nfra_hh + dtr_unif_hh + dtr_book_hh + dtr_bnet_hh