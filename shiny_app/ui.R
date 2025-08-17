###############################################.
#
# App main ui script
#
##############################################.

#######################################.
# Initial structure  ------
#######################################.
library(shiny)
library(bslib)
library(shinyFeedback)
library(shinyjs)

# this first part of the UI creates a purple navigation bar to place individual tabs in
# it's also where some external script are sourced that are required for different part
# of the app to work
page_navbar(
  # controlling how items grow/shrink when browser different sizes
  fillable = TRUE,
  window_title = "Malawi CEQ Microsimulation Tool",
  # id required for profile buttons - works with profile_homepage_btn_mod to control navigation
  id = "nav",
  lang = "en",
  # dashboard theme - defined in global script
  theme = phs_theme,
  navbar_options = navbar_options(
    # background navbar colour
    bg = "#1E7F84",
    # collapse tabs on smaller screens
    collapsible = TRUE
  ),
  # place external scripts in footer argument to avoid warnings as recommended by package developers
  header = tags$head(
    # need to declare this to use functions from the shinyjs package, e.g. to show/hide parts of the UI
    useShinyjs(),
    # need to declare this to use functions from the shinyjs package, e.g. to show/hide parts of the UI

    # required for guided tours
    use_cicerone(),
    # required to specify formatting (particularly of landing page)
    includeCSS("www/styles.css"),
    
    # Add this for toast notifications
    useToastr()  # Initialize toastr
  ),
  
  #######################################.
  # Homepage ----
  ######################################.
  # this tab is the homepage of the app. Note that some of the code to create this landing
  # page sits in a seperate HTML file
  # the profile_homepage_btn_mod_UI is a module that creates a button to navigate to the
  # profiles tab of the app. It also updates the profile filter depending on what profile you select
  
  nav_panel(
    value = "home",
    style = "margin: 1; padding:1;",
    title = "Home",
    style = "background-color:#F2F2F2;",
    bslib::card(
      full_screen = FALSE,
      card_header(
        div(
          style = "text-align: center; font-size: 3em; font-weight: bold; padding: 10px;",
          "Malawi in 2019"
        )
      ),
      card_body(
        div(
        
        h2(class = "fw-bold text-primary", "Country Profile"),
        
        h3(class = "fw-bold", "Poverty"),
        fluidRow(
          column(width = 3,
                 bslib::value_box(
                   title = "Poverty Headcount Rate",
                   value = tags$span("50.7%", class = "fw-bold text-primary"),
                   showcase = bs_icon("person-fill-exclamation", fill = "primary"),
                   theme = value_box_theme(bg = "#e9f2f3", fg = "#006D77"),
                   p("Half of Malawians lived below the national poverty line (454 MWK per day per capita).")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Extreme Poverty Rate",
                   value = tags$span("70.1%", class = "fw-bold text-primary"),
                   showcase = bs_icon("exclamation-triangle-fill", fill = "primary"),
                   theme = value_box_theme(bg = "#e9f2f3", fg = "#006D77"),
                   p("Roughly 70% of Malawians lived on less than US$2.15 (2017 PPP) per day per capita.")
                 )
          ),
          
            column(width = 3,
                    bslib::value_box(
                    title = "Urban vs Rural Poverty",
                    value = tags$span("Urban: 30%  Rural: 77%", class = "fw-bold text-primary"),
                      showcase = bs_icon("house-door-fill", fill = "primary"),
                      theme = value_box_theme(bg = "#e9f2f3", fg = "#006D77"),
                      p("Extreme poverty is more than two times higher in rural areas.")
                            )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Food insecurity",
                   value = tags$span("23.8%", class = "fw-bold text-primary"),
                   showcase = bs_icon("cart4", fill = "primary"),
                   theme = value_box_theme(bg = "#e9f2f3", fg = "#006D77"),
                   p("One fourth of households are highly food insecure.")
                 )
          )
        ),
        fluidRow(
          
          h3(class = "fw-bold", "Inequality & Demographics"),
          
          column(width = 3,
                 bslib::value_box(
                   title = "Gini Index",
                   value = tags$span("0.39", class = "fw-bold"),
                   showcase = bs_icon("bar-chart-fill"), 
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#0078D4"),
                   p("Income inequality declined to around 0.39 from 0.45 in 2010.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "90/10 Income Ratio",
                   value = tags$span("8.6", class = "fw-bold"),
                   showcase = bs_icon("cash-coin"), 
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#0078D4"),
                   p("The richest 10% earned about 9 times more than the poorest 10%.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Dependency Ratio",
                   value = tags$span("1.2", class = "fw-bold"),
                   showcase = bs_icon("people-fill", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#0078D4"),
                   p("There were about 1.2 dependents for every working‑age.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Population Size",
                   value = tags$span("18.6 million", class = "fw-bold"),
                   showcase = bs_icon("person-vcard-fill", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#0078D4"),
                   p("The population was estimated to 18.6 million with 84% living in rural areas.")
                 )
          ))),
        
        tags$hr(),
        
        # ---------- Group 2 ----------
        div(
          h2(class = "fw-bold text-primary", "Government budget sources and uses"),
          
          h3(class = "fw-bold", "Revenue sources"),
          fluidRow(
            # Total tax revenue
            column(width = 3,
                   bslib::value_box(
                     title = "Tax Revenue",
                     value = tags$span("11.6%", class = "fw-bold"),
                     showcase = bs_icon("wallet", fill = "primary"),
                     theme = value_box_theme(bg = "#F0EFF3", fg = "#6B5C85"),
                     p("Tax-to-GDP was about 11.6% ≈ 1,225 billion MWK.")
                   )
            ),
            
            # Mix: Direct taxes
            column(width = 3,
                   bslib::value_box(
                     title = "Direct taxes",
                     value = tags$span("41%", class = "fw-bold"),
                     showcase = bs_icon("bank2", fill = "primary"),
                     theme = value_box_theme(bg = "#F0EFF3", fg = "#6B5C85"),
                     p("About 41% of total tax revenue (PAYE and coorporate income taxes).")
                   )
            ),
            
            # Indirect taxes (kept)
            column(width = 3,
                   bslib::value_box(
                     title = "Indirect taxes",
                     value = tags$span("59%", class = "fw-bold"),
                     showcase = bs_icon("cart-check", fill = "primary"),
                     theme = value_box_theme(bg = "#F0EFF3", fg = "#6B5C85"),
                     p("About 59% of total tax revenue (VAT and excises).")
                   )
            ),
            #  VAT 
            column(width = 3,
                   bslib::value_box(
                     title   = "VAT",
                     value   = tags$span("55%", class = "fw-bold"),
                     showcase = bs_icon("receipt", fill = "primary"),
                     theme   = value_box_theme(bg = "#F0EFF3", fg = "#6B5C85"),
                     p("VAT represents the largest tax source ≈ 55% of total tax revenue.")
                   )
            )

          ),
          h3(class = "fw-bold", "Transfers & Subsidies"),
          # Uses: transfers & subsidies
          fluidRow(
            column(width = 3,
                   bslib::value_box(
                     title = "Transfer payments",
                     value = tags$span("4.06%", class = "fw-bold"),
                     showcase = bs_icon("currency-exchange", fill = "primary"),
                     theme = value_box_theme(bg = "#fff7f0", fg = "#6b4b00"),
                     p("Transfers account for 4.06% of GDP ≈ 428 billion MWK.")
                   )
            ),
            column(width = 3,
                   bslib::value_box(
                     title = "Direct Transfers",
                     value = tags$span("64%", class = "fw-bold"),
                     showcase = bs_icon("cash-stack", fill = "primary"),
                     theme = value_box_theme(bg = "#fff7f0", fg = "#6b4b00"),
                     p("Direct transfers (cash and near-cash) make up 64% of all transfers.")
                   )
            ),
            column(width = 3,
                   bslib::value_box(
                     title = "Subsidies",
                     value = tags$span("36%", class = "fw-bold"),
                     showcase = bs_icon("droplet-half", fill = "primary"),
                     theme = value_box_theme(bg = "#fff7f0", fg = "#6b4b00"),
                     p("Electricity and fuel subsidies represent 36% of total transfers.")
                   )
            ),
            column(width = 3,
                   bslib::value_box(
                     title = "Direct cash transfers",
                     value = tags$span("42%", class = "fw-bold"),
                     showcase = bs_icon("gift", fill = "primary"),
                     theme = value_box_theme(bg = "#fff7f0", fg = "#6b4b00"),
                     p("Cash transfers account for 42% of all government transfer spending.")
                   )
            ))),
        div(
          fluidRow(
          nav_panel(
            title = tagList(bs_icon("question-circle-fill"), span("Indicator Help")),
            value = "help_indicators",
            bslib::card(
              full_screen = FALSE,
              card_header(
                tags$h4("Understanding the indicators", class = "fw-bold text-primary")
              ),
              card_body(
                tags$ul(
                  tags$li(strong("Poverty headcount rate:"), 
                          " share of the population below the national poverty line defined by the minimum food basket (2,215 calories) or 454 MWK per day per capita.",
                          "In 2019 it was about 50.7%."),
                  tags$li(strong("Extreme poverty (international poverty line):"),
                          " percentage living on less than 656.7 MWK (2019) or US$2.15 (2017 PPP) per day per capita. About 70% of Malawians fell below this threshold in 2019."),
                  tags$li(strong("Gini index:"),
                          " measures inequality on a 0–1 scale; 0.39 indicates moderate inequality."),
                  tags$li(strong("90/10 ratio:"),
                          " ratio of consumption or income of the richest 10% to that of the poorest 10%; around 8.6 in 2019."),
                  tags$li(strong("Urban vs rural poverty:"),
                          " under the 656.7 MWK (2019) or US$2.15 (2017 PPP) per day per capita, about 30% of urban dwellers and 77% of rural residents were poor in 2019."),
                  tags$li(strong("Dependency ratio:"),
                          " number of children (0–14) and older persons (65+) per working‑age adult (15–64); about 1.2 in 2019."),
                  tags$li(strong("Population size:"),
                          " the 2018 Population and Housing Census counted roughly 18.6 million people.")
                ),
                p(
                  tags$em("Sources: "),
                  tags$a(
                    "Malawi Fifth Integrated Household Survey 2019–20 (NSO) – Socioeconomic Report",
                    href = "https://microdata.worldbank.org/index.php/catalog/3818/related-materials",  # put this file in www/
                    target = "_blank", rel = "noopener"
                  ),
                  HTML("&nbsp;|&nbsp;"),
                  tags$a(
                    "World Bank – Poverty & Equity Brief: Malawi (Apr 2023)",
                    href = "https://databankfiles.worldbank.org/public/ddpext_download/poverty/987B9C90-CB9F-4D93-AE8C-750588BF00QA/current/Global_POVEQ_MWI.pdf",  # put this file in www/
                    target = "_blank", rel = "noopener"
                  )
                )
              )
            )
          )
        )
        )
        ),
        hr(),
        fluidRow(
          column(
            width = 3,
            tags$img(
              src = "WorldBank_Logo_optimized.png",
              height = "100px",
              style = "display: block; margin-left: auto; margin-right: auto;"
            )
          ),
          column(
            width = 9,
            div(
              style = "font-size: 1em; padding-top: 10px; text-align: left;",
              p(strong("About this tool:")),
              p("This CEQ microsimulation tool was developed by the Poverty and Equity Global Practice of the World Bank on the Eastern and Southern Africa region. It supports policy analysis to help governments understand the distributional impact of fiscal policies and strengthen equity-driven reforms."),
              p("The Poverty Team works closely with national statistical offices and ministries of finance to promote evidence-based decision-making.")
            )
          )
        )
      )
    ),
  ############################################.
  # Policy choice TAB ----
  ############################################.
  nav_panel(
    title = "Policy choice",
    value = "policy_choice",
    style = "margin: 1; padding:1;", # remove margin so no white space at top of landing page

    navset_pill(
      id = "sub_tabs_input",

      # --- Direct tax tab ---
      nav_panel(
        title = "Direct tax",
        value = "direct_tax",
        
        navset_pill_list(
          id = "sub_tabs_input_paye",
          widths = c(2, 10),
          nav_panel(title = "PAYE Income Tax", 
                    # Table-style input form
                    card(
                      full_screen = FALSE,
                      fill = TRUE,
                    card_header(
                      bs_icon("person-badge", size = "3em"),
                      span("PAYE Income Tax", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
                      class = "info-box-header"
                    ),
                    p(
                      "This tab allows you to adjust the Pay As You Earn (PAYE) tax rates applied to different income level. ",
                      "The Pre-reform rates reflect current tax rates.",
                      tags$a(" Learn more in the CEQ methodology guide.",
                             href = "https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm/file/1696511034124",
                             target = "_blank"),
                      "."
                    ),
                    tags$table(
                      class = "table table-borderless",
                      tags$thead(
                        tags$tr(
                          tags$th("Bracket"),
                          tags$th("Upper income threshold (in million kwacha) per year"),
                          tags$th("Pre-reform tax rate (%)"),
                          tags$th("Post-reform tax rate (%)")
                        )
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td("1"),
                          tags$td("Less than 1.8"),
                          tags$td("0"),
                          tags$td(
                            numericInput(
                              "tax_rate_lowest", NULL, value = 0, min = 0, max = 100, step = .01, width = "100px"
                            )
                          )
                        ),
                        tags$tr(
                          tags$td("2"),
                          tags$td("More than 1.8 and less than 6"),
                          tags$td("25"),
                          tags$td(
                            numericInput(
                              "tax_rate_second", NULL, value = 25, min = 0, max = 100, step = .01, width = "100px"
                            )
                          )
                        ),
                        tags$tr(
                          tags$td("3"),
                          tags$td("More than 6 and less than 30.6"),
                          tags$td("30"),
                          tags$td(
                            numericInput(
                              "tax_rate_middle", NULL, value = 30, min = 0, max = 100, step = .01, width = "100px"
                            )
                          )
                        ),
                        tags$tr(
                          tags$td("4"),
                          tags$td("More than 30.6"),
                          tags$td("35"),
                          tags$td(
                            numericInput(
                              "tax_rate_top", NULL, value = 35, min = 0, max = 100, step = .01, width = "100px"
                            )
                          )
                        )
                      )
                    )
                    )
                    ),
          nav_panel(title = "Corporate Income Tax",
                    
                    card(
                      full_screen = FALSE,
                      fill = TRUE,
                      card_header(
                        bs_icon("building", size = "3em"),
                        span("Corporate Income Tax", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
                        class = "info-box-header"
                      ),
                      p(
                        "This tab allows you to adjust the corporate income tax rates applied to different industries. ",
                        "The baseline rates reflect Post-reform classifications based on priority sectors, incorporation status, and business type.",
                        tags$a(" Learn more in the CEQ methodology guide.",
                               href = "https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm/file/1696511034124",
                               target = "_blank"),
                        "."
                      ),
                      
                      tags$div(
                        style = "margin-bottom: 15px;",
                        
                        # SelectInput for Agriculture sector
                        selectInput(
                          inputId = "remove_agriculture_exemption",
                          label = "Remove tax exemption for Agriculture companies created after 2010?",
                          choices = c("No", "Yes"),
                          selected = "No",
                          width = "50%"
                        ),
                        
                        # SelectInput for Electricity sector
                        selectInput(
                          inputId = "remove_electricity_exemption",
                          label = "Remove tax exemption for Electricity companies created after 2010?",
                          choices = c("No", "Yes"),
                          selected = "No",
                          width = "50%"
                        )
                      ),
                      
                      tags$table(
                        class = "table table-borderless",
                        tags$thead(
                          tags$tr(
                            tags$th("Sector Code"),
                            tags$th("Sector Name"),
                            tags$th("Pre-reform tax rate (%)"),
                            tags$th("Post-reform tax rate (%)")
                          )
                        ),
                        tags$tbody(
                          tagList(
                            tags$tr(
                              tags$td(1),
                              tags$td("Agriculture"),
                              tags$td(0),
                              tags$td(numericInput("corp_tax_1", label = NULL, value = 0, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(2),
                              tags$td("Mining"),
                              tags$td(30),
                              tags$td(numericInput("corp_tax_2", label = NULL, value = 30, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(3),
                              tags$td("Manufacture"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_3", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(4),
                              tags$td("Electricity, Gas and Water"),
                              tags$td(0),
                              tags$td(numericInput("corp_tax_4", label = NULL, value = 0, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(5),
                              tags$td("Construction"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_5", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(6),
                              tags$td("Retail"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_6", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(7),
                              tags$td("Transport"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_7", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(8),
                              tags$td("Hospitality"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_8", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(9),
                              tags$td("Communication"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_9", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(10),
                              tags$td("Financial activities"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_10", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(11),
                              tags$td("Real Estate Activities"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_11", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(12),
                              tags$td("Professional, scientific and Technical activities"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_12", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(13),
                              tags$td("Administrative and Support Service Activities"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_13", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(14),
                              tags$td("Public adminitration and Defense;Compulsory Social Security"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_14", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(15),
                              tags$td("Education"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_15", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(16),
                              tags$td("Human Health and Social Work Activities"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_16", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(17),
                              tags$td("Arts, Entertainment and Recreation"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_17", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            ),
                            tags$tr(
                              tags$td(18),
                              tags$td("Other Service Activities"),
                              tags$td(15),
                              tags$td(numericInput("corp_tax_18", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            )
                          )
                         )
                       )
                    )
                  ),
          nav_panel(title = "Help",
                    value = "Help",help_direct_tax_tab)
        )
          
      ),
      
      # --- Other placeholder tabs ---
      nav_panel(title = "Indirect taxes", value = "indirect_taxes", 

                navset_pill_list(
                  id = "sub_tabs_input_indirect",
                  widths = c(2, 10),
                  nav_panel(title = "VAT tax",  value = "vat_tax", 
                            card(
                              full_screen = FALSE,
                              fill = TRUE,
                              card_header(
                                bs_icon("receipt", size = "3em"),
                                span("VAT Input", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
                                class = "info-box-header"
                              ),
                              
                              p(
                                "Specify item-level VAR tax rates. These are pre-filled with official policy rates but can be adjusted for simulation scenarios.",
                                tags$a("More info", href = "https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm", target = "_blank")
                              ),
                              
                              # >>> UI-only table (no server render) <<<
                              build_vat_table_ui(vat_catalog)
                            )
                  ),
                  nav_panel(title = "Excise", value = "excise", 
                            card(
                              full_screen = FALSE,
                              fill = TRUE,
                              card_header(
                                bs_icon("percent", size = "3em"),
                                span("Excise Tax by Item", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
                                class = "info-box-header"
                              ),
                              p(
                                "Specify item-level excise tax rates. These are pre-filled with official policy rates but can be adjusted for simulation scenarios."
                              ),
                              
                              build_excise_table_ui(excise_catalog)
                                      )
                  ),
                  nav_panel(title = "Help",
                            value = "Help",help_indirect_tax_tab)
                )
        ),
      nav_panel(title = "Direct transferts", value = "direct_transferts", 
                navset_pill_list(
                  id = "sub_tabs_direct_cash",
                  widths = c(2, 10),
                  nav_panel(title = "Direct cash transfers",  value = "direct_cash_transfers", 
                        card(
                              full_screen = FALSE,
                              fill = TRUE,
                              card_header(
                                bs_icon("info-circle-fill", size = "3em"),
                                span("Direct transferts Input Form", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
                                class = "info-box-header"
                              ),
                              p(
                                "This tab presents the structure of the direct transfert system used in the microsimulation model: Government direct cash transfer and Farm Input Subsidy Programme."
                              ),
                              # >>> UI-only table (no server render) <<<
                              h4(class = "fw-bold text-primary", "Government direct cash transfer"),
                              build_dct_gov_table_ui(dct_catalog),
                              
                              h4(class = "fw-bold text-primary", "Farm Input Subsidy Programme(FISP)"),
                              build_fips_gov_table_ui(dct_fips_catalog)
                              
                            )
                            
                  ), 
                  nav_panel(
                    title = "Near-cash transfers ",
                    value = "near_cash_transfers",
                    card(
                      full_screen = FALSE,
                      fill = TRUE,
                      card_header(
                        bs_icon("info-circle-fill", size = "3em"),
                        span("Near-cash transfers Input Form", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
                        class = "info-box-header"
                      ),
                      p(
                        "Adjust post-reform average amounts by pre-reform decile for each near-cash program."
                      ),
                      
                      # --- Free Maize transfer ---
                      h4(class = "fw-bold text-primary", "Free Maize transfer"),
                      build_nearcash_table_ui(
                        df         = dtr_frmz_hh_catalog,
                        var_key    = "dtr_frmz_hh",
                        program_label = "Free Maize",
                        id_prefix  = "dtr_frmz_hh_post_"
                      ),
                      tags$hr(),
                      
                      # --- NFRA food aid ---
                      h4(class = "fw-bold text-primary", "NFRA food aid"),
                      build_nearcash_table_ui(
                        df         = dtr_nfra_hh_catalog,
                        var_key    = "dtr_nfra_hh",
                        program_label = "NFRA food aid",
                        id_prefix  = "dtr_nfra_hh_post_"
                      ),
                      tags$hr(),
                      
                      # --- MASAF public works ---
                      h4(class = "fw-bold text-primary", "MASAF public works"),
                      build_nearcash_table_ui(
                        df         = dtr_masaf_hh_catalog,
                        var_key    = "dtr_masaf_hh",
                        program_label = "MASAF public works",
                        id_prefix  = "dtr_masaf_hh_post_"
                      ),
                      tags$hr(),
                      
                      # --- Food or cash for work ---
                      h4(class = "fw-bold text-primary", "Food or cash for work"),
                      build_nearcash_table_ui(
                        df         = dtr_ffwk_hh_catalog,
                        var_key    = "dtr_ffwk_hh",
                        program_label = "Food/Cash for work",
                        id_prefix  = "dtr_ffwk_hh_post_"
                      ),
                      tags$hr(),
                      
                      # --- Input-for-work programme ---
                      h4(class = "fw-bold text-primary", "Input-for-work programme"),
                      build_nearcash_table_ui(
                        df         = dtr_ifwp_hh_catalog,
                        var_key    = "dtr_ifwp_hh",
                        program_label = "Input-for-work",
                        id_prefix  = "dtr_ifwp_hh_post_"
                      ),
                      tags$hr(),
                      
                      # --- Secondary education scholarship ---
                      h4(class = "fw-bold text-primary", "Secondary education scholarship"),
                      build_nearcash_table_ui(
                        df         = dtr_ses_hh_catalog,
                        var_key    = "dtr_ses_hh",
                        program_label = "Secondary scholarship",
                        id_prefix  = "dtr_ses_hh_post_"
                      ),
                      tags$hr(),
                      
                      # --- Tertiary education scholarship ---
                      h4(class = "fw-bold text-primary", "Tertiary education scholarship"),
                      build_nearcash_table_ui(
                        df         = dtr_tes_hh_catalog,
                        var_key    = "dtr_tes_hh",
                        program_label = "Tertiary scholarship",
                        id_prefix  = "dtr_tes_hh_post_"
                      ),
                      tags$hr(),
                      
                      # --- Other near-cash transfers ---
                      h4(class = "fw-bold text-primary", "Other near-cash transfers"),
                      build_nearcash_table_ui(
                        df         = dtr_onc_hh_catalog,
                        var_key    = "dtr_onc_hh",
                        program_label = "Other near-cash",
                        id_prefix  = "dtr_onc_hh_post_"
                      )
                    )
                  ),
                  
                  nav_panel(title = "Help",value = "Help",help_direct_transf_tab)
                )
                
                ),
      # nav_panel(title = "Subsidies", value = "subsidies", 
      #           navset_pill_list(
      #             id = "sub_tabs_subsidies",
      #             widths = c(2, 10),
      #             nav_panel(
      #             title = "Electricity",
      #             value = "electricity",
      #             bslib::card(
      #               full_screen = FALSE,
      #               fill = TRUE,
      #               bslib::card_header(
      #                 bs_icon("lightning-charge", size = "3em"),
      #                 span("Electricity tariff", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
      #                 class = "info-box-header"
      #               ),
      #               p("Edit the subsidized tariff and block parameters used to compute the electricity subsidy. The electricity rate without subsidies is 50 MWK/kWh."),
      #               fluidRow(
      #                 column(
      #                   3,
      #                   numericInput(
      #                     inputId = "elec_rate_subsidized",
      #                     label   = "Subsidized rate (MWK/kWh)",
      #                     value   = 47.5,   # from your text
      #                     min     = 0, max = 50, step = 0.01, width = "100%"
      #                   )
      #                 ),
      #                 column(
      #                   3,
      #                   numericInput(
      #                     inputId = "elec_block1_kwh",
      #                     label   = "Maximum subsidized consumption (kWh/month)",
      #                     value   = 50,     # first-block size
      #                     min     = 0, max = 1000, step = 1, width = "100%"
      #                   )
      #                 ),
      #                 column(
      #                   3,
      #                   numericInput(
      #                     inputId = "elec_rate_block2",
      #                     label   = "Block-2 rate (MWK/kWh)",
      #                     value   = 67.25,  # from your text
      #                     min     = 0, max = 1000, step = 0.01, width = "100%"
      #                   )
      #                 )
      #               )
      #             )
      #           ),
      #           nav_panel(
      #             title = "Fuel subsidy",
      #             value = "fuel_sub",
      #             bslib::card(
      #               full_screen = FALSE,
      #               fill = TRUE,
      #               bslib::card_header(
      #                 bs_icon("fuel-pump", size = "3em"),
      #                 span("Fuel subsidy — national envelope", style = "font-size: 1.8em; vertical-align: middle; margin-left: 10px;"),
      #                 class = "info-box-header"
      #               ),
      #               p("Allocate a national fuel subsidy pot (as % of GDP) across households in proportion to annual fuel spending (domestic + enterprise)."),
      #               fluidRow(
      #                 column(
      #                   4,
      #                   numericInput(
      #                     inputId = "fuel_subsidy_pct_gdp",
      #                     label   = "Fuel subsidy (% of GDP)",
      #                     value   = 0.243,     # 0.243% of GDP
      #                     min     = 0, max = 1, step = 0.001, width = "100%"
      #                   )
      #                 )
      #               )
      #               )
      #             )
      #           )
       #         ),
      nav_spacer(), # add space to navbar 
      # In your UI, replace the loadingButton with:

      nav_item(
        conditionalPanel(
          condition = paste(
            "input.tax_rate_lowest != 0",
            "|| input.tax_rate_second != 25",
            "|| input.tax_rate_middle != 30",
            "|| input.tax_rate_top != 35",
            "|| input.remove_agriculture_exemption != 'No'",
            "|| input.remove_electricity_exemption != 'No'",
            paste0("|| input.corp_tax_", 0:18, " != ", 
                   c(15, 0, 15, 15, 15, 15, 0, 15, 15, 15, 15, 30, 15, 15, 15, 15, 15, 15, 15), 
                   collapse = " "),
            # Excise tax conditions (ordered by excise rate priority)
            "|| input.vat_rate != 16.5",   # For VAT
            "|| input.excise_item_914 != 250",   # Wine or commercial liquor
            "|| input.excise_item_911 != 250",   # Bottled / canned beer
            "|| input.excise_item_913 != 40",    # Traditional beer (masese)
            "|| input.excise_item_330 != 20",    # Cooking utensils
            "|| input.excise_item_5801 != 20",   # Radio with flash drive/micro CD
            "|| input.excise_item_211 != 10",    # Diesel
            "|| input.excise_item_813 != 10",    # Tomato sauce (bottle)
            "|| input.excise_item_333 != 10",    # Umbrella
            "|| input.excise_item_909 != 5",     # Bottled water
            "|| input.excise_item_519 != 5",     # Mini-bus
            "|| input.excise_item_517 != 5",     # Motorcycle/scooter
            "|| input.excise_item_611 != 5",     # Tractor
            # Items with zero excise rates
            "|| input.excise_item_506 != 0",     # Pork
            "|| input.excise_item_507 != 0",     # Mutton
            "|| input.excise_item_106 != 0",     # Rice
            "|| input.excise_item_111 != 0",     # Bread
            "|| input.excise_item_101 != 0",     # Maize ufa mgaiwa (normal flour)
            "|| input.excise_item_801 != 0",     # Sugar
            "|| input.excise_item_803 != 0",     # Cooking oil
            "|| input.excise_item_810 != 0",     # Salt
            sep = " "
          ),
          loadingButton("simulate_button", label = "Simulate")
        )
      )
    ), # end navset_pill_list
    
    # ---- World Bank logo + About section ----
    fluidRow(
      column(
        width = 3,
        tags$img(
          src = "WorldBank_Logo_optimized.png",
          height = "100px",
          style = "display: block; margin-left: auto; margin-right: auto;"
        )
      ),
      column(
        width = 9,
        div(
          style = "font-size: 1em; padding-top: 10px; text-align: left;",
          p(strong("About this tool:")),
          p("This CEQ microsimulation tool was developed by the Poverty and Equity Global Practice of the World Bank on the Eastern and Southern Africa region. It supports policy analysis to help governments understand the distributional impact of fiscal policies and strengthen equity-driven reforms."),
          p("The Poverty Team works closely with national statistical offices and ministries of finance to promote evidence-based decision-making.")
        )
      )
    )
  ), # end navset_pill

  ############################################.
  # Policy Results TAB ----
  ############################################.
  nav_panel(title = "Results", value = "results", style = "margin: 1; padding:1;", # remove margin so no white space at top of landing page
            
            navset_pill(id = "sub_tabs_results",  
                
                # Summary
                 nav_panel(title = "Summary", value = "summary", sum_mod_ui("sim_summary")),
                # --- Direct tax tab ---
                nav_panel(title = "Poverty Indicators by Income Concept", value = "poverty", pov_mod_ui("sim_poverty")),
                # --- Geospatial analysis tab ---
                nav_panel(title = "Geospatial Poverty Analysis", value = "geospatial", geo_pov_mod_ui("sim_geo_poverty")),
                # --- Inequality analysis tab ---
                nav_panel(title = "Inequality", value = "inequality", ineq_mod_ui("sim_inequality")),
                # --- Net cash position tabs ---
                #nav_panel(title = "Net cash position", value = "net_cash_position", "Net cash position"),
                # --- Incidence tabs ---
                nav_panel(title = "Incidence", value = "incidence", incid_mod_ui("sim_incidence"))
                
         ) # end navset_pill_list
  ), # end nav_panel "Results choice"
  
  
  nav_spacer(), # add space to navbar 
  ########################################.
  # Link to github repo  -------
  ########################################.
  nav_item(
    div(
      style = "display: flex; align-items: center; gap: 12px; padding-right: 15px;",
      
      # Malawi flag
      tags$img(
        src = "malawi_flag.png",
        height = "30px",
        alt = "Malawi Flag",
        style = "border-radius: 4px;"
      ),
      
      # World Bank logo
      tags$img(
        src = "world_bank_logo.png",
        height = "30px",
        alt = "World Bank Logo"
      )
    )
  ),
  ########################################.
  # Menu with additional tabs  -------
  ########################################.
  
  # this section creates a drop-down menu containing 3 tabs which provide further information on ScotPHO
  # i.e. an about scotpho tab, an indicator definitions tab and an about profiles tab 
  
  nav_menu(
    title = "More information",
    align = "right", # ensures tab names inside the menu are not cut-off
    
    # about scotpho tab
    nav_panel(title = "About ScotPHO", value = "about_scotpho", "about_scotpho_text"),
    
    
    # indicator definitions tab
    nav_panel(title = "Indicator Definitions",
              value = "definitions",
              "definitions_tab_UI(metadata)"
    )
  ) # close nav menu
  
) #close page_navbar

### END





