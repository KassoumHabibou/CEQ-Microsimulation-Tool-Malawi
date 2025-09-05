####################################################
# About Pov chart
####################################################
help_pov_chart_tab <- tagList(
  # --- What does this show? ---
  tags$div(
    tags$h6("What does the poverty simulation table show?", class = "chart-header"),
    tags$p(
      "This table summarizes how taxes, transfers, and in-kind services change poverty under different ",
      strong("income concepts"), " and across multiple ", strong("poverty lines"), "."
    ),
    tags$ul(
      tags$li(strong("Poverty Rate"), ": share of the population below the chosen line."),
      tags$li(strong("Number of Poor"), ": total individuals under the line."),
      tags$li(strong("Poverty Gap"), ": average shortfall (as % of the line) among the poor."),
      tags$li(strong("Poverty Severity"), ": squared-gap measure giving more weight to the poorest.")
    ),
    tags$p("Indicators are reported for standard CEQ income concepts:"),
    tags$ul(
      tags$li(strong("Market income"), " → before any taxes/transfers."),
      tags$li(strong("Gross income"), " → market + direct transfers."),
      tags$li(strong("Net market income"), " → gross − direct taxes."),
      tags$li(strong("Disposable income"), " → net market ± indirect subsidies/taxes."),
      tags$li(strong("Consumable income"), " → disposable − indirect taxes on consumption."),
      tags$li(strong("Final income"), " → consumable + in-kind education/health.")
    )
  ),
  # --- How are indicators computed? ---
  tags$div(
    tags$h6("How are the indicators calculated?", class = "chart-header"),
    tags$p(
      "We apply each poverty line (e.g., National line ", code("454 MWK/day"), ", ",
      "International lines ", code("US$2.15 PPP"), " and ", code("US$3.65 PPP"),
      ") to the selected income concept and compute the four FGT-style metrics."
    ),
    tags$p(
      "Results are produced for two scenarios: ", strong("Baseline"), " and ",
      strong("Simulated policy"), " to quantify policy changes."
    )
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Income concept selector"), ": choose the CEQ concept."),
      tags$li(strong("Poverty line"), ": national or international thresholds."),
      tags$li(strong("Scenario"), ": Baseline vs. Simulated policy."),
      tags$li(strong("Metric"), ": rate, headcount, gap, or severity.")
    )
  ),
  # --- Output variables (data layer) ---
  tags$div(
    tags$h6("What variables does the tool create?", class = "chart-header"),
    tags$ul(
      tags$li(code("pov_rate_*"), ", ", code("pov_headcount_*"), ", ", code("pov_gap_*"), ", ", code("pov_sev_*"),
              " for each income concept and scenario; results are available by line and subgroup.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How should I interpret the table?", class = "chart-header"),
    tags$ul(
      tags$li("Lower poverty at ", strong("disposable/final income"), " vs ", strong("market income"),
              " implies that fiscal policy reduces poverty."),
      tags$li("Changes in ", strong("gap/severity"), " show whether the poorest benefit the most."),
      tags$li("Comparing lines (national vs PPP) tests robustness to alternative thresholds."),
      tags$li("Focus on ", strong("Baseline → Simulated"), " differences to assess reform impact.")
    )
  ),
  # --- Notes & safeguards ---
  tags$div(
    tags$h6("Practical notes and safeguards", class = "chart-header"),
    tags$ul(
      tags$li("All poverty metrics use survey weights and eqUser Interfacevalently defined income concepts."),
      tags$li("Estimates may differ from official poverty profiles due to concept and line choices."),
      tags$li("Small area results can be noisy; prefer broader groupings for inference.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why are these poverty results important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "They reveal whether fiscal policy is poverty-reducing, for whom, and by how much, ",
      "providing an immediate test of policy progressivity in levels and depth."
    )
  )
)

####################################################
# Geospatial poverty analysis
####################################################
help_geospatial_pov_tab <- tagList(
  # --- What does this show? ---
  tags$div(
    tags$h6("What does this geospatial analysis show?", class = "chart-header"),
    tags$p(
      "A spatial view of poverty under different income concepts, poverty lines, and scenarios ",
      "to compare impacts across regions/districts."
    ),
    tags$ul(
      tags$li(strong("Geographic unit"), ": region or district."),
      tags$li(strong("Income concept"), ": from market to final income."),
      tags$li(strong("Poverty line"), ": national (454 MWK/day) and international (US$2.15, US$3.65 PPP)."),
      tags$li(strong("Metric"), ": rate, headcount, gap, or severity.")
    )
  ),
  # --- Tabs usage ---
  tags$div(
    tags$h6("How should I use the three tabs?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Histogram"), ": absolute levels by area to spot hotspots."),
      tags$li(strong("Map"), ": percentage change due to the reform, computed as ",
              code("(Pre − Post) / Pre"), 
              ". Positive values = reduction; negative = worsening."),
      tags$li(strong("Data"), ": the underlying table for filtering and export.")
    )
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Scenario"), ": Baseline vs Simulated policy."),
      tags$li(strong("Income concept & line"), ": align with your analysis focus."),
      tags$li(strong("Metric & color scale"), ": choose level or % change; adjust legend breaks.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How do I interpret the percentage change on the map?", class = "chart-header"),
    tags$p(
      "Colors reflect the policy-induced change in the selected indicator. ",
      "Darker positive shading implies larger reductions (better outcomes)."
    ),
    tags$p("Use side-by-side concepts to track whether gains persist after taxes and in-kind services.")
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why are spatial views useful in CEQ analysis?", class = "chart-header"),
    tags$p(
      "They test spatial eqUser Interfacety—whether benefits are concentrated or broad—",
      "and inform regionally targeted policy design."
    )
  )
)

####################################################
# Direct taxes
####################################################
help_direct_tax_tab <- tagList(
  # --- What are direct taxes? ---
  tags$div(
    tags$h6("What are direct taxes?", class = "chart-header"),
    tags$p(
      "Levies paid directly by individuals or firms on income or profits. ",
      "In this tool we model:"
    ),
    tags$ul(
      tags$li(strong("PAYE"), ": personal income tax on formal wage earnings."),
      tags$li(strong("Corporate income tax (CIT)"), ": tax on household-enterprise profits (non-agricultural).")
    )
  ),
  # --- How are they estimated? ---
  tags$div(
    tags$h6("How are direct taxes estimated?", class = "chart-header"),
    tags$p(
      "We use IHS 2019/20 microdata. For PAYE, identified formal employees are taxed using statutory brackets: ",
      code("0% ≤ 1.8m; 25%: 1.8–6m; 30%: 6–30.6m; 35%: >30.6m"), "."
    ),
    tags$p(
      "For CIT, reported annual net profits from household businesses are taxed by sector/type, ",
      "with statutory rates (e.g., ", code("15%"), " standard; ", code("30%"), " for mining/formal; ",
      code("0%"), " for priority sectors meeting criteria)."
    ),
    tags$p("Individual/business liabilities are aggregated to the household for incidence.")
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("PAYE brackets"), ": numeric inputs for post-reform rates."),
      tags$li(strong("CIT sector rates"), ": inputs ", code("corp_tax_*"), " per sector."),
      tags$li(strong("Policy toggles"), ": remove sector exemptions (e.g., agriculture/electricity).")
    )
  ),
  # --- Output variables ---
  tags$div(
    tags$h6("What variables does the tool create?", class = "chart-header"),
    tags$ul(
      tags$li(code("p_tax, p_tax_m, pp_tax"), ": individual PAYE (annual/monthly/share)."),
      tags$li(code("dtx_payt_hh"), ": household CIT liability."),
      tags$li(code("i_ptax, i_dtx_payt_hh"), ": incidence flags.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How should I interpret the results?", class = "chart-header"),
    tags$ul(
      tags$li("PAYE and CIT liabilities concentrate in upper deciles → progressive burden."),
      tags$li("Compare ", strong("Pre vs Post"), " to see who pays more/less under reform."),
      tags$li("Survey undercoverage of large formal firms implies lower simulated CIT totals than admin data.")
    )
  ),
  # --- Notes & safeguards ---
  tags$div(
    tags$h6("Practical notes and safeguards", class = "chart-header"),
    tags$ul(
      tags$li("Top incomes may be under-reported; consider sensitivity checks."),
      tags$li("Winsorization/outlier handling affects bracket allocations."),
      tags$li("Sector mapping for CIT should be reviewed if custom codes are used.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why are direct taxes important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "They fund transfers/services and shape progressivity. Modeling them clarifies trade-offs ",
      "between revenue needs and eqUser Interfacety."
    )
  )
)

####################################################
# Indirect taxes (VAT & Excise)
####################################################
help_indirect_tax_tab <- tagList(
  # --- What are indirect taxes? ---
  tags$div(
    tags$h6("What are indirect taxes?", class = "chart-header"),
    tags$p(
      "Consumption-based levies embedded in prices and collected by sellers. We model:",
      " ", strong("VAT"), " (broad-based) and ", strong("Excise"), " (item-specific)."
    )
  ),
  # --- How are they calculated? ---
  tags$div(
    tags$h6("How are VAT and excise taxes calculated?", class = "chart-header"),
    tags$p(
      "Each survey item is matched to statutory VAT/excise (Taxation Amendment Act 2024) and legal treatment ",
      "(taxable, exempt, zero-rated). We respect the cascade: ",
      strong("excise first"), ", then VAT on (net + excise)."
    ),
    tags$p(
      "By default, the User Interface assumes purchases are formal and fully taxed; advanced scenarios can incorporate informality."
    )
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("VAT table"), ": edit post-reform item rates in the interactive table ",
              "(inputs like ", code("vat_post_*"), ")."),
      tags$li(strong("Excise table"), ": edit post-reform item rates ",
              "(inputs like ", code("excise_post_*"), ").")
    )
  ),
  # --- Output variables ---
  tags$div(
    tags$h6("What variables does the tool create?", class = "chart-header"),
    tags$ul(
      tags$li(code("itx_vatx_hh, itx_excx_hh"), ": household VAT/excise under the baseline assumption."),
      tags$li(code("itx_vatx_hh2/3, itx_excx_hh2/3"), ": optional scenarios (e.g., informality/formality splits).")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("What do the simulation results show?", class = "chart-header"),
    tags$ul(
      tags$li("Assuming all consumption is formal tends to over-estimate revenues."),
      tags$li("Burden generally rises with income; excise often concentrates in top deciles.")
    )
  ),
  # --- Notes & safeguards ---
  tags$div(
    tags$h6("Practical notes and safeguards", class = "chart-header"),
    tags$ul(
      tags$li("Mind the tax order: excise → VAT (on net+excise)."),
      tags$li("Exempt/zero-rated items should have VAT = 0 and excise as per law."),
      tags$li("Item mapping drives results—review any custom code merges.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why are indirect taxes important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "They raise substantial revenue but may burden consumers. Incidence analysis tests whether ",
      "they align with eqUser Interfacety objectives."
    )
  )
)

####################################################
# Inequality measures
####################################################
help_ineq_chart_tab <- tagList(
  # --- What does this show? ---
  tags$div(
    tags$h6("What does this section show?", class = "chart-header"),
    tags$p(
      "Inequality levels under different income concepts to see how taxes/transfers shape the distribution."
    ),
    tags$p("Three standard measures are reported: Gini, Theil, and the 90/10 ratio.")
  ),
  # --- How are they calculated? ---
  tags$div(
    tags$h6("How are the inequality measures calculated?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Gini coefficient"), ": 0 (equality) → 1 (max inequality)."),
      tags$li(strong("Theil index"), ": entropy-based dispersion; decomposable by groups."),
      tags$li(strong("90/10 ratio"), ": income at P90 divided by income at P10.")
    ),
    tags$p("All measures use survey weights and are computed per selected income concept and scenario.")
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Income concept"), ": market, disposable, final, etc."),
      tags$li(strong("Scenario"), ": Baseline vs Simulated policy."),
      tags$li(strong("Measure"), ": Gini, Theil, or 90/10.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How should I interpret the results?", class = "chart-header"),
    tags$ul(
      tags$li("A lower Gini/Theil at disposable vs market income indicates redistributive impact."),
      tags$li("A falling 90/10 ratio signals narrowing extremes.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why are inequality measures important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "They reveal whether policy compresses the distribution and by how much, complementing poverty results."
    )
  )
)

####################################################
# Direct transfers
####################################################
help_direct_transf_tab <- tagList(
  # --- What are direct transfers? ---
  tags$div(
    tags$h6("What are direct transfers?", class = "chart-header"),
    tags$p(
      "Programs that provide cash or in-kind support. We track:"
    ),
    tags$ul(
      tags$li(strong("Direct cash transfers"), ": e.g., government cash and FISP."),
      tags$li(strong("Near-cash transfers"), ": in-kind or conditional benefits (food aid, scholarships, public works).")
    )
  ),
  # --- How are they estimated? ---
  tags$div(
    tags$h6("How are direct transfers estimated?", class = "chart-header"),
    tags$p(
      "Amounts come from IHS transfer modules and program rules. ",
      strong("Cash"), ": code ", code("111"), " plus monetized FISP coupons ",
      "(e.g., ", code("22,000 MWK"), " fertilizer; ", code("8,000 MWK"), " seed; scaled to admin totals)."
    ),
    tags$p(
      strong("Near-cash"), ": free maize, scholarships, work-for-aid, etc., monetized using reported quantities and ",
      "official prices (e.g., ", code("6,000 MWK/50kg"), " maize)."
    ),
    tags$p("Household totals: ", code("dct_hh"), " (cash) and ", code("dtr_nct_hh"), " (near-cash).")
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Program add-ons by decile"), ": inputs like ",
              code("dct_gov_hh_post_*"), " or generic near-cash tables to simulate reforms.")
    )
  ),
  # --- Output variables ---
  tags$div(
    tags$h6("What variables does the tool create?", class = "chart-header"),
    tags$ul(
      tags$li(code("dct_hh"), ": total direct cash per household."),
      tags$li(code("dtr_nct_hh"), ": total near-cash per household."),
      tags$li("Program-specific components (e.g., FISP coupons) available for diagnostics.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("What do the simulation results show?", class = "chart-header"),
    tags$ul(
      tags$li("Transfers are generally progressive—higher in lower deciles."),
      tags$li("FISP typically dominates cash support; near-cash is split across food aid and education.")
    )
  ),
  # --- Notes & safeguards ---
  tags$div(
    tags$h6("Practical notes and safeguards", class = "chart-header"),
    tags$ul(
      tags$li("Scaling to administrative totals improves comparability."),
      tags$li("In-kind valuation depends on local prices; adjust if market conditions change.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why are direct transfers important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "They directly raise household resources and can offset regressive elements elsewhere in the system."
    )
  )
)



####################################################
# Subsidies incidence chart
####################################################
help_incid_sub_chart_tab <- tagList(
  # --- What does this show? ---
  tags$div(
    tags$h6("What does the subsidies incidence chart show?", class = "chart-header"),
    tags$p(
      "How electricity and fuel subsidies are distributed by income decile."
    ),
    tags$ul(
      tags$li(strong("All indirect subsidies"), ": electricity + fuel."),
      tags$li(strong("Electricity subsidy"), ": implicit transfer from below-cost tariffs."),
      tags$li(strong("Fuel subsidy"), ": allocation from the national fuel envelope.")
    ),
    tags$p("Incidence can be shown as absolute (MWK/HH), relative (% of income/consumption), or total (aggregate MWK).")
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Scenario"), ": Pre-reform vs Post-reform."),
      tags$li(strong("Incidence type"), ": absolute / relative / total."),
      tags$li(strong("Component"), ": all, electricity, or fuel.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How should I interpret the chart?", class = "chart-header"),
    tags$ul(
      tags$li("Higher bars in upper deciles → pro-rich capture."),
      tags$li("Electricity often skews to richer groups where access is higher."),
      tags$li("Fuel typically most regressive due to higher spending among the rich."),
      tags$li("Pre → Post shifts show who gains/loses under reform.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why is this important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "Subsidies are costly. Incidence analysis identifies beneficiaries and informs retargeting toward pro-poor alternatives."
    )
  )
)

####################################################
# Direct tax incidence chart
####################################################
help_incid_dtx_chart_tab <- tagList(
  # --- What does this show? ---
  tags$div(
    tags$h6("What does the direct tax incidence chart show?", class = "chart-header"),
    tags$p(
      "Distribution of PAYE and CIT burdens by income decile."
    ),
    tags$ul(
      tags$li(strong("Absolute"), ": MWK/HH."),
      tags$li(strong("Relative"), ": % of income."),
      tags$li(strong("Total"), ": aggregate MWK by decile.")
    ),
    tags$p("Results for ", strong("Pre-reform"), " and ", strong("Post-reform"), " enable before/after comparisons.")
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Scenario"), ": Pre vs Post."),
      tags$li(strong("Incidence type"), ": absolute / relative / total."),
      tags$li(strong("Tax component"), ": all direct, PAYE only, CIT only.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How should I interpret the chart?", class = "chart-header"),
    tags$ul(
      tags$li("Rising bars in higher deciles → progressive burden."),
      tags$li("If lower deciles face high relative shares, the tax is regressive."),
      tags$li("Pre → Post gaps reveal distributional impact of the reform.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why is this important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "It tests whether tax changes align with eqUser Interfacety goals while meeting revenue needs."
    )
  )
)

####################################################
# Indirect tax incidence chart
####################################################
help_incid_itx_chart_tab <- tagList(
  # --- What does this show? ---
  tags$div(
    tags$h6("What does the indirect tax incidence chart show?", class = "chart-header"),
    tags$p(
      "Distribution of VAT and excise burdens by income decile."
    ),
    tags$ul(
      tags$li(strong("All indirect taxes"), ": VAT + excise."),
      tags$li(strong("VAT"), ": broad consumption tax."),
      tags$li(strong("Excise"), ": item-specific taxes (alcohol, tobacco, fuel, etc.).")
    ),
    tags$p("Incidence options: absolute (MWK/HH), relative (% of income/consumption), or total (aggregate MWK).")
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Scenario"), ": Pre vs Post."),
      tags$li(strong("Incidence type"), ": absolute / relative / total."),
      tags$li(strong("Tax component"), ": all, VAT, or excise.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How should I interpret the chart?", class = "chart-header"),
    tags$ul(
      tags$li("Burden typically rises with income; excise often concentrates at the top."),
      tags$li("If relative incidence rises for the poor, the reform is regressive."),
      tags$li("Pre → Post changes quantify distributional shifts.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why is this important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "It clarifies who ultimately pays consumption taxes and whether reforms protect low-income households."
    )
  )
)

####################################################
# Direct transfer incidence chart
####################################################
help_incid_dtr_chart_tab <- tagList(
  # --- What does this show? ---
  tags$div(
    tags$h6("What does the direct transfer incidence chart show?", class = "chart-header"),
    tags$p(
      "Distribution of direct cash and near-cash benefits by income decile."
    ),
    tags$ul(
      tags$li(strong("All direct transfers"), ": cash + near-cash."),
      tags$li(strong("Direct cash"), ": e.g., Government cash, FISP monetized coupons."),
      tags$li(strong("Near-cash"), ": food aid, scholarships, public works.")
    ),
    tags$p("Incidence options: absolute (MWK/HH), relative (% of income/consumption), or total (aggregate MWK).")
  ),
  # --- User Interface controls ---
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Scenario"), ": Pre vs Post."),
      tags$li(strong("Incidence type"), ": absolute / relative / total."),
      tags$li(strong("Component"), ": all, cash, or near-cash.")
    )
  ),
  # --- Interpretation ---
  tags$div(
    tags$h6("How should I interpret the chart?", class = "chart-header"),
    tags$ul(
      tags$li("Higher benefits in lower deciles → pro-poor targeting."),
      tags$li("Pre → Post shifts show whether the reform improves targeting.")
    )
  ),
  # --- Why it matters ---
  tags$div(
    tags$h6("Why is this important in CEQ analysis?", class = "chart-header"),
    tags$p(
      "It evaluates whether scarce fiscal resources reach vulnerable households and reduce poverty/inequality."
    )
  )
)

###########################################################
# SIMD – level & trend
###########################################################
about_simd <- tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p(
    "Bar comparisons of an indicator across deprivation groups, highlighting differences ",
    "between the most and least deprived and any monotonic gradient."
  )
)

about_simd_trend <- tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p(
    "Trends in the indicator by deprivation groups over time, showing whether gaps persist, narrow, or widen."
  )
)



help_subsidies_tab <- tagList(
  # ---- What are subsidies? ----
  tags$div(
    tags$h6("What are indirect subsidies?", class = "chart-header"),
    tags$p(
      "In this tool, “indirect subsidies” are price supports embedded in what households pay for ",
      "energy—specifically ", strong("fuel"), " and ", strong("electricity"), ". ",
      "A household’s subsidy is measured as the ", em("difference"), " between a ",
      "counterfactual bill at the full (unsubsidized) tariff and the amount the household actually pays."
    ),
    tags$ul(
      tags$li(strong("Fuel subsidy:"), " a national envelope (as % of GDP) is allocated across households ",
              "in proportion to their annual fuel spending (domestic + enterprise)."),
      tags$li(strong("Electricity subsidy:"), " based on ESCOM’s block tariff. We impute monthly kWh from reported bills, ",
              "price those kWh at a higher, no-subsidy rate, then take the gap as the subsidy.")
    )
  ),
  
  # ---- How does the app calculate them? ----
  tags$div(
    tags$h6("How are fuel and electricity subsidies calculated?", class = "chart-header"),
    tags$p(
      "The simulation uses household microdata from IHS 2019/20 and the 2018–2020 ESCOM tariff. ",
      "Key data sources and steps are:"
    ),
    tags$ul(
      tags$li(
        strong("Fuel (domestic use): "),
        "Module ", code("HH_MOD_I2"), ", item ", code("hh_i05 == 212"),
        " with spending ", code("hh_i06"), " → annualized to ", code("fuel_con")
      ),
      tags$li(
        strong("Fuel (enterprise use): "),
        "Module ", code("HH_MOD_N2"), ", spending ", code("hh_n41d"),
        " → annualized to ", code("fuel_enter"),
        ". For households with multiple businesses, we consolidate enterprise fuel by summing the ",
        "minimum and maximum values per ", code("hhid"), " to avoid double counting."
      ),
      tags$li(
        strong("Fuel allocation rule: "),
        "Set a national fuel subsidy pot ", code("FuelPot = GDP × fuel_subsidy_pct_gdp/100"),
        ". Compute each household’s annual fuel spend ", code("tfc1 = fuel_con_hh + fuel_enter_hh"),
        ", then allocate ", code("tfc_hh = tfc1 × FuelPot / Σ(tfc1 × weight)")
      ),
      tags$li(
        strong("Electricity (domestic): "),
        "Module ", code("HH_MOD_F"), " with monthly/weekly/daily frequency ",
        code("hh_f26a/hh_f26b"), " to annualize ", code("hh_f25 → elec_cons"),
        " and derive ", code("elec_cons_m = elec_cons/12")
      ),
      tags$li(
        strong("Imputing kWh from the bill: "),
        "If no air-conditioner: first ", code("elec_block1_kwh"), " at ",
        code("elec_rate_subsidized"), " then the remainder at ", code("elec_rate_block2"),
        " (we invert this schedule to recover kWh from spending). ",
        "If the household reports an air-conditioner (", code("HH_MOD_L: hh_l01 == 1 & hh_l02 == 506"), "), ",
        "we impute kWh at a flat ", code("96"), " MWK/kWh."
      ),
      tags$li(
        strong("Counterfactual (no-subsidy) bill: "),
        "price all imputed kWh at ", code("elec_rate_block2"), " (no AC) or ", code("96"), " (AC). ",
        "Annual electricity subsidy is ", code("(cws_month − elec_cons_m) × 12"), " floored at zero."
      ),
      tags$li(
        strong("Electricity (enterprise): "),
        "Module ", code("HH_MOD_N2"), " spending ", code("hh_n41e"),
        " → annual ", code("elec_enter"), "; subsidy approximated as ",
        code("elec_enter_sub = elec_enter × 0.15"), " due to limited tariff detail for businesses."
      )
    )
  ),
  
  # ---- User Interface controls ----
  tags$div(
    tags$h6("What can I change in the User Interface?", class = "chart-header"),
    tags$p("Use these inputs to stress-test reform scenarios:"),
    tags$ul(
      tags$li(strong("Fuel subsidy — national envelope: "),
              code("fuel_subsidy_pct_gdp"), " (default ", code("0.243"), ")"),
      tags$li(strong("Electricity tariff — subsidized block: "),
              code("elec_rate_subsidized"), " (default ", code("47.5"), " MWK/kWh), ",
              code("elec_block1_kwh"), " (default ", code("50"), " kWh/month), ",
              code("elec_rate_block2"), " (default ", code("67.25"), " MWK/kWh)"),
      tags$li("The air-conditioner proxy uses a fixed flat rate of ", code("96"), " MWK/kWh for imputing kWh.")
    )
  ),
  
  # ---- Output variables ----
  tags$div(
    tags$h6("What variables does the tool create?", class = "chart-header"),
    tags$p("Key outputs added to the analysis dataset:"),
    tags$ul(
      tags$li(code("tfc_hh"), ": household fuel subsidy (annual MWK; stored on ", code("pid == 1"), ")."),
      tags$li(code("elec_subsidy"), ": domestic electricity subsidy from tariff counterfactual (annual MWK)."),
      tags$li(code("elec_enter_sub"), ": enterprise electricity subsidy (= 15% of annual enterprise bill)."),
      tags$li(code("sub_electri"), ": total electricity subsidy = ", code("elec_subsidy + elec_enter_sub"),
              " (stored on ", code("pid == 1"), ").")
    )
  ),
  
  # ---- Interpretation ----
  tags$div(
    tags$h6("How should I interpret the results?", class = "chart-header"),
    tags$p(
      "Fuel subsidies tend to be ", strong("regressive"), " because higher-income households spend more on fuel. ",
      "Electricity subsidies are also often regressive, though less extremely, reflecting differential access to the grid ",
      "and higher consumption among richer households."
    )
  ),
  
  # ---- Practical notes & safeguards ----
  tags$div(
    tags$h6("Practical notes and safeguards", class = "chart-header"),
    tags$ul(
      tags$li(strong("Units must match:"), " GDP should be in MWK, and ", code("fuel_subsidy_pct_gdp"),
              " is a percent (not a share)."),
      tags$li(strong("Weights:"), " when forming national totals for the fuel allocation denominator, survey weights are used if available."),
      tags$li(strong("No double counting:"), " fuel and electricity totals are stored once per household on ", code("pid == 1"), "."),
      tags$li(strong("Zero floor:"), " electricity subsidies are truncated at zero (no negative subsidies)."),
      tags$li(strong("Enterprise electricity:"), " the 15% factor is a simplifying assumption due to limited tariff detail; ",
              "adjust in code if better information is available."),
      tags$li(strong("Multiple enterprises:"), " enterprise fuel uses a min+max consolidation within ", code("hhid"),
              " to avoid duplicate counts while preserving scale.")
    )
  ),
  
  # ---- Why it matters ----
  tags$div(
    tags$h6("Why model subsidies in CEQ analysis?", class = "chart-header"),
    tags$p(
      "Subsidies use fiscal space and can undermine eqUser Interfacety if they are captured by higher-income groups. ",
      "By quantifying who benefits and how much, the tool helps assess sustainability and the distributional impact ",
      "of alternative tariff and budget settings."
    ),
    tags$p(HTML(
      "Learn more about the CEQ approach at the ",
      "<a href='https://commitmenttoeqUser Interfacety.org' target='_blank'>CEQ Institute website</a>."
    ))
  )
)


