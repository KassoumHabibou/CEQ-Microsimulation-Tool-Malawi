
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
            assumptions about the minimum income needed to escape poverty.")
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
              The formula used is: (Baseline - Simulation) / Baseline. Positive values reflect a reduction in poverty, while negative values indicate a worsening outcome."),
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


