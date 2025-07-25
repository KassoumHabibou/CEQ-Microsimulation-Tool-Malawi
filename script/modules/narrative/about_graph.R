
####################################################
# About Pov chart
####################################################
help_pov_chart_tab <- tagList(
  tags$div(
    tags$h6("What does the poverty simulation table show?", class = "chart-header"),
    tags$p("This table summarizes the impact of tax and transfer policies on poverty outcomes across different income concepts 
            and poverty lines."),
    tags$p("For each poverty line — such as the National Poverty Line (454 MWK per day), the Lower Income Class Line 
            (2.15 USD PPP), and the Middle Income Class Line (3.65 USD PPP) — we compute four key poverty indicators:"),
    tags$ul(
      tags$li(strong("Poverty Rate:"), " The share of the population living below the poverty line."),
      tags$li(strong("Number of Poor:"), " The total number of individuals living in poverty."),
      tags$li(strong("Poverty Gap:"), " The average shortfall (as a percentage of the poverty line) among the poor."),
      tags$li(strong("Poverty Severity:"), " A weighted measure that gives more importance to those further below the line.")
    ),
    tags$p("These indicators are calculated under different income concepts to understand the role of the fiscal system:"),
    tags$ul(
      tags$li(strong("Market Income:"), " Income before any government intervention (taxes or transfers)."),
      tags$li(strong("Gross Income:"), " Market income plus direct transfers."),
      tags$li(strong("Net Market Income:"), " Gross income minus direct taxes."),
      tags$li(strong("Disposable Income:"), " Net income plus indirect subsidies minus indirect taxes."),
      tags$li(strong("Consumable Income:"), " Disposable income minus indirect taxes (e.g., VAT, excise)."),
      tags$li(strong("Final Income:"), " Consumable income plus monetized value of in-kind services (e.g., education, health).")
    )
  ),
  tags$div(
    tags$h6("How should I interpret the table?", class = "chart-header"),
    tags$p("By comparing results across income concepts, you can assess how taxes, transfers, and in-kind benefits reduce poverty."),
    tags$p("For example, a lower poverty rate at the disposable income level compared to the market income level indicates 
            that cash transfers and direct taxes help reduce poverty."),
    tags$p("Similarly, looking at the change in poverty gap or severity across concepts helps evaluate how deep poverty is and 
            whether policies are reaching the poorest households."),
    tags$p("Comparing poverty indicators across the three poverty lines helps understand the effect of policy under different 
            assumptions about the minimum income needed to escape poverty."),
    tags$p("To assess the effect of the new policy compared to the baseline, look at the difference in values for each indicator 
            between the two columns: ",
           strong("Baseline"), " and ", strong("Simulated Policy"), ". ",
           "A reduction in the poverty rate, gap, or severity in the simulated scenario signals an improvement in poverty outcomes 
            due to the reform. Conversely, an increase would indicate a worsening situation. The comparison helps identify 
            whether the policy reform is pro-poor and which income group or poverty line it benefits most.")
  )
)



help_geospatial_pov_tab <- tagList(
  tags$div(
    tags$h6("What does this geospatial analysis show?", class = "chart-header"),
    tags$p("This section explores how the simulated tax and transfer policy affects poverty outcomes 
            across regions or districts, allowing for a spatial comparison of policy impacts."),
    tags$p("Results are disaggregated by:"),
    tags$ul(
      tags$li(strong("Geographic Unit:"), " Results can be viewed at the region or district level."),
      tags$li(strong("Income Concept:"), " From market income to final income, showing the cumulative effect of fiscal policy."),
      tags$li(strong("Poverty Line:"), " Includes the national poverty line (454 MWK/day), lower income (2.15 USD PPP), and middle income (3.65 USD PPP) thresholds."),
      tags$li(strong("Poverty Metric:"), " Indicators include poverty rate, number of poor, poverty gap, and poverty severity.")
    )
  ),
  tags$div(
    tags$h6("How should I use the three tabs?", class = "chart-header"),
    tags$ul(
      tags$li(strong("Histogram Tab:"), " Displays bar charts showing the level of a selected poverty indicator 
              (e.g., poverty rate or number of poor) by region or district. This helps identify where poverty is most or least prevalent."),
      tags$li(strong("Map Tab:"), " Provides a spatial visualization of the percentage change in a selected indicator due to the simulated policy. 
              The formula used is: (Pre reform value - Current policy value) / Pre reform value Positive values reflect a reduction in the parameter (eg. poverty), while negative values indicate a worsening outcome."),
      tags$li(strong("Data Tab:"), " Shows the underlying data in tabular format, enabling comparisons and filtering across geographic areas, poverty lines, and income concepts.")
    )
  ),
  tags$div(
    tags$h6("How do I interpret the percentage change on the map?", class = "chart-header"),
    tags$p("The map colors each region or district according to the percentage change in a selected poverty indicator due to the simulated policy."),
    tags$p("For example, a 10% reduction in the number of poor in a district means the simulated policy reduced poverty there by 10% compared to the baseline."),
    tags$p("This visualization helps assess spatial equity — whether the policy benefits are equally distributed across the country or concentrated in certain areas.")
  ),
  tags$div(
    tags$h6("Learn more about CEQ analysis", class = "chart-header"),
    tags$p(HTML("This tool follows the <a href='https://commitmenttoequity.org' target='_blank'>Commitment to Equity (CEQ) methodology</a>, 
                which evaluates how taxes, transfers, and subsidies impact poverty and inequality."))
  )
)



help_direct_tax_tab <- tagList(
  tags$div(
    tags$h6("What are direct taxes?", class = "chart-header"),
    tags$p("Direct taxes are levied directly on individuals or businesses based on their income or profits. 
            In contrast to indirect taxes (like VAT), direct taxes are paid directly to the government and are 
            generally progressive — those with higher incomes pay a higher share."),
    tags$p("In this simulation tool, we include two main types of direct taxes:")
  ),
  
  tags$ul(
    tags$li(strong("Pay As You Earn (PAYE):"), " a personal income tax applied to formal wage earners."),
    tags$li(strong("Corporate Income Tax:"), " a tax on profits earned by non-agricultural household businesses.")
  ),
  
  tags$div(
    tags$h6("How are direct taxes estimated?", class = "chart-header"),
    tags$p("The estimation of direct taxes uses detailed income and business data from the 2019/2020 Malawi IHS survey."),
    tags$p("For PAYE, the model identifies individuals with formal paid employment and applies the official tax brackets:"),
    tags$ul(
      tags$li("0% for annual income ≤ MWK 1.8 million"),
      tags$li("25% for income between MWK 1.8 – 6 million"),
      tags$li("30% for income between MWK 6 – 30.6 million"),
      tags$li("35% for income > MWK 30.6 million")
    ),
    tags$p("For Corporate Tax, household business profits are taxed based on the business type and activity sector, using statutory rates 
            (e.g., 15% standard, 30% for mining and formal firms, and 0% for priority sectors like agriculture)."),
    tags$p("All tax liabilities are calculated at the individual or business level and aggregated to the household level for analysis.")
  ),
  
  tags$div(
    tags$h6("What do the simulation results show?", class = "chart-header"),
    tags$p("The model shows that direct taxes in Malawi are highly progressive — concentrated among the wealthiest households."),
    tags$p("Under Scenario 3 (realistic estimation):"),
    tags$ul(
      tags$li("Only households in deciles 6 through 10 pay PAYE, with decile 10 alone contributing over 92% of the total."),
      tags$li("Corporate tax liability is even more concentrated, with decile 10 responsible for over 50% of the total."),
      tags$li("The poorest half of the population (deciles 1–5) pays almost no direct tax, reflecting their low or informal earnings.")
    ),
    tags$p("Compared to official records, the model underestimates total revenue — especially for corporate tax — due to survey limitations 
            (e.g., underreporting and exclusion of large firms). Still, it captures the distributional effects of the tax system accurately.")
  ),
  
  tags$div(
    tags$h6("Why are direct taxes important in CEQ analysis?", class = "chart-header"),
    tags$p("Direct taxes are a key instrument for redistribution. They reduce disposable income but fund the public services and transfers 
            that can lower poverty and inequality."),
    tags$p("Understanding who pays taxes — and how much — is essential for evaluating fiscal fairness and aligning tax policy with social goals."),
    tags$p(HTML("To learn more about the CEQ methodology, visit the <a href='https://commitmenttoequity.org' target='_blank'>CEQ Institute website</a>."))
  )
)


help_indirect_tax_tab <- tagList(
  tags$div(
    tags$h6("What are indirect taxes?", class = "chart-header"),
    tags$p("Indirect taxes are taxes imposed on the consumption of goods and services rather than directly on income or profits. 
            Unlike direct taxes, they are not paid directly to the government by individuals, but are embedded in the prices of goods 
            and collected by sellers. The two main types of indirect taxes analyzed in this tool are:"),
    tags$ul(
      tags$li(strong("Value-Added Tax (VAT):"), " A broad-based consumption tax applied at each stage of production and distribution, 
              ultimately paid by consumers."),
      tags$li(strong("Excise Taxes:"), " Product-specific taxes applied to items like alcohol, tobacco, fuel, and other selected goods.")
    )
  ),
  
  tags$div(
    tags$h6("How are VAT and excise taxes calculated?", class = "chart-header"),
    tags$p("VAT and excise taxes are simulated using detailed household consumption data from the 2019/2020 IHS survey. 
            Each item purchased by households was matched to official tax rates from the Taxation Amendment Act 2024, and categorized 
            based on legal treatment (taxable, exempt, or zero-rated)."),
    tags$p("The simulation incorporates the distinction between consumption in the formal and informal sectors. Since a large share of 
            consumption in Malawi occurs informally and is not taxed, each household is assigned an ", strong("informality coefficient"), 
           " that adjusts the taxable share of consumption. This coefficient is based on income decile, expenditure type, and geographic location."),
    tags$p("Three scenarios are available for estimation:"),
    tags$ul(
      tags$li(strong("Scenario 1 (VAT1/Excise1):"), " Assumes all household consumption is formal and fully taxed."),
      tags$li(strong("Scenario 2 (VAT2/Excise2):"), " Applies taxes to a weighted share of informal consumption, using adjusted coefficients."),
      tags$li(strong("Scenario 3 (VAT3/Excise3):"), " Applies taxes only to the portion of consumption identified as formal.")
    ),
    tags$p("The estimation process respects the cascading tax structure: excise taxes are applied first, then VAT is calculated on the 
            sum of the product's value and excise tax. This ensures accurate modeling of the legal tax burden.")
  ),
  
  tags$div(
    tags$h6("What do the simulation results show?", class = "chart-header"),
    tags$p("The simulation confirms that assuming all consumption is formal (Scenario 1) significantly overestimates tax revenue. 
            Scenarios 2 and 3 provide more realistic estimates by accounting for informality."),
    tags$p("From a distributional perspective, both VAT and excise taxes are progressive in Malawi. The richest households contribute 
            the majority of indirect tax revenue under Scenario 3:"),
    tags$ul(
      tags$li("Decile 10 contributes over 40% of total VAT and nearly 58% of total excise revenue."),
      tags$li("Lower-income households face a lower tax burden due to their limited formal consumption."),
    ),
    tags$p("This pattern highlights the importance of considering informality in tax incidence analysis, especially in low-income countries.")
  ),
  
  tags$div(
    tags$h6("Why are indirect taxes important in CEQ analysis?", class = "chart-header"),
    tags$p("Indirect taxes play a dual role: they are a major source of government revenue and a potential burden on consumers. 
            Analyzing their incidence helps determine whether fiscal systems are equitable."),
    tags$p("The CEQ approach ensures that both revenue impacts and household-level effects are captured, 
            making it possible to assess whether taxation disproportionately affects the poor or supports redistributive goals."),
    tags$p(HTML("To learn more about the CEQ methodology, visit the <a href='https://commitmenttoequity.org' target='_blank'>CEQ Institute website</a>."))
  )
)



help_direct_transf_tab <- tagList(
  tags$div(
    tags$h6("What are direct transfers?", class = "chart-header"),
    tags$p("Direct transfers are government programs that provide cash or in-kind support to households. 
            These transfers aim to reduce poverty, improve food security, and support vulnerable populations."),
    tags$p("In this tool, we consider two main types of direct transfers:"),
    tags$ul(
      tags$li(strong("Direct Cash Transfers:"), " Monetary support programs like the unconditional government cash transfer and the Farm Input Subsidy Programme (FISP)."),
      tags$li(strong("Near-Cash Transfers:"), " In-kind support with a cash-equivalent value, such as food aid (e.g., free maize) and education scholarships.")
    )
  ),
  
  tags$div(
    tags$h6("How are direct transfers estimated?", class = "chart-header"),
    tags$p("Transfer amounts are derived from household survey data and official program details."),
    tags$p(strong("Direct Cash Transfers"), " include two components:"),
    tags$ul(
      tags$li(strong("Unconditional cash transfers:"), " identified in the survey using transfer code 111, summing both cash and in-kind components."),
      tags$li(strong("FISP (Farm Input Subsidy Programme):"), " based on reported receipt of fertilizer and seed coupons, monetized at MWK 22,000 (fertilizer) and MWK 8,000 (seeds) per coupon. A scaling factor aligns values with the program’s official budget.")
    ),
    tags$p("The total cash support is captured by the variable ", code("dct_hh"), ", which aggregates both components."),
    tags$p(strong("Near-Cash Transfers"), " are estimated using codes for specific forms of assistance (e.g., free maize, scholarships, food-for-work). Each is monetized based on reported quantities and official prices (e.g., MWK 6,000 per 50kg maize bag)."),
    tags$p("These values are summed into the variable ", code("dtr_nct_hh"), ", representing the household’s total near-cash benefit.")
  ),
  
  tags$div(
    tags$h6("What do the simulation results show?", class = "chart-header"),
    tags$p("Total direct transfer benefits in Malawi are estimated at MWK 71,365 million:"),
    tags$ul(
      tags$li("Direct cash transfers account for MWK 45,698 million — with FISP representing more than 75% of that total."),
      tags$li("Near-cash transfers amount to MWK 25,667 million — primarily from food aid and education support."),
      tags$li("The poorest households receive the largest share: 60% of total transfers go to the bottom four deciles.")
    ),
    tags$p("This highlights the progressive nature of the transfer system — most benefits reach the intended poor and vulnerable groups.")
  ),
  
  tags$div(
    tags$h6("Why are direct transfers important in CEQ analysis?", class = "chart-header"),
    tags$p("Transfers are a key mechanism for reducing poverty and inequality. By increasing household resources, they improve well-being and help meet basic needs."),
    tags$p("Analyzing both cash and near-cash transfers allows for a full understanding of the state’s redistributive efforts."),
    tags$p("The CEQ framework captures how these programs affect income distribution and poverty indicators under different policy scenarios."),
    tags$p(HTML("To learn more about the CEQ methodology, visit the <a href='https://commitmenttoequity.org' target='_blank'>CEQ Institute website</a>."))
  )
)


####################################################
# About Relative index of inequality (RII) measure 
####################################################



#########################################################
# About Slope index of inequality (SII)
########################################################

about_sii <- tagList(
  tags$div(
    tags$h6("What does the chart show?", class = "chart-header"),
    tags$p("The chart shows the absolute inequality has changed over time. Absolute inequality is
            measured using a value called the 'Slope Index of Inequality (SII)'. This is a measure of
            the gap between the most and least disadvantaged populations.")
  ),
  tags$div(
    tags$h6("How do I interpret the chart?", class = "chart-header"),
    tags$p("The larger the SII, the greater the disparity between the most and least deprived areas.
            An increasing trend suggests the gap between the most and least deprived areas is growing.")
  )
)

###########################################################
# About population attributable risk trends (PAR)
##########################################################


about_par <- tagList(
  tags$div(
    tags$h6("What does the chart show?", class = "chart-header"),
    tags$p("The bar chart shows indicator values split by the deprivation quintiles. The area shaded
       in purple is the same across all 5 quintiles, it shows the rate observed in the least deprived
       quintile. The area shaded in blue represents the additional activity the 4 remaining quintiles
       have over and above that seen in the least deprived quintile."),
    tags$p("Looking at data in this way illustrates the potential impact of removing deprivation (i.e. 
    in the hypothetical situation that all deprivation quintiles experienced the same rates).")
  ),
  tags$div(
    tags$h6("Related resources", class = "chart-header"),
    tags$p("You can read more about the Population Attributable Risk in the", tags$a("Measuring inequalities 
           section", href = "https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/", target = "_blank"),  "of the ScotPHO website.")
    
  )
)


about_par_trend <- tagList(
  tags$div(
    tags$h6("What does the chart show?", class = "chart-header"),
    tags$p("The line chart shows the Population Attributable Risk (PAR) also known as Population Attributable Fraction (PAF).
          The PAR is presented as a percentage, and describes by how much the overall rate of an indicator would increase or
          decrease if all areas were to experience the rates observed in the most favourable area. ")
  ),
  tags$div(
    tags$h6("How do I interpret the chart?", class = "chart-header"),
    tags$p("The higher the PAR values the greater the impact of inequality on that indicator and the greater the potential for
          improvement if this inequality could be removed."),
    tags$p("The PAF describes a hypothetical situation and makes the assumption that all of the association between the risk 
  factor and indicator is causal. In reality there could a number of other factors influencing the trends observed.")
  ),
  tags$div(
    tags$h6("Related resources", class = "chart-header"),
    tags$p("You can read more about the PAR in the", tags$a("Measuring inequalities 
         section", href = "https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/", target = "_blank")," of the ScotPHO website.")
  )
)

###########################################################
# About SIMD trend chart
###########################################################

about_simd <- tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p("The bar chart shows how the measure for a particular indicator varies according to the relative deprivation 
  of the area people live in. The chart illustrate how rates in the most and least deprived areas 
  compare and also whether there is a simple relationship between relative deprivation and a particular indicator.")
)



about_simd_trend <- tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p("The trend chart shows how the measure for a particular indicator varies according to the relative deprivation 
  of the area people live in, over a period of time. The chart illustrate how rates in the most and least deprived areas 
  compare over time and also whether there is a simple relationship between relative deprivation and a particular indicator.")
)


