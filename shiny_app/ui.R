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
    bg = phs_colours(colourname = "phs-teal"),
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
        fluidRow(
          column(
            width = 12,
            p(
              style = "font-size: 1.2em; text-align: center; padding: 10px;",
              "This microsimulation tool is designed to simulate the distributional impact of tax and transfer policies in Malawi using the Commitment to Equity (CEQ) methodology. 
              Users can explore how policy reforms affect poverty, inequality, and government revenue. The tool is built using data from the 2019–20 Fifth Integrated Household Survey (IHS5) of Malawi. To begin, users can click on the “Policy choice” tab to adjust tax and transfer parameters and simulate their effects."
            )
          )
        ),
        fluidRow(
          column(width = 3,
                 bslib::value_box(
                   title = "Poverty Headcount",
                   value = "50.7%",
                   showcase = bs_icon("person-fill-down", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("Half of Malawians lived below the national poverty line in 2019–20.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Extreme Poverty",
                   value = "20.4%",
                   showcase = bs_icon("exclamation-triangle-fill", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("One in five Malawians could not meet minimum food needs.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Gini Index",
                   value = "0.44",
                   showcase = bs_icon("cash", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("Income inequality was relatively high across regions.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "90/10 Income Ratio",
                   value = "8.6",
                   showcase = bs_icon("sort-up"), 
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("The richest 10% earned 8.6 times more than the poorest 10%.")
                 )
          )
        ),
        fluidRow(
          column(width = 3,
                 bslib::value_box(
                   title = "Urban vs Rural Poverty",
                   value = "Urban: 17.7% vs Rural: 56.6%",
                   showcase = bs_icon("house-door-fill", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("Rural poverty was more than 3 times higher.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Revenue Mobilization",
                   value = "13.1% of GDP",
                   showcase = bs_icon("bank", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("Tax revenue remained low relative to regional benchmarks.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Dependency Ratio",
                   value = "1.06",
                   showcase = bs_icon("people-fill", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("There were more dependents than working-age members in most households.")
                 )
          ),
          column(width = 3,
                 bslib::value_box(
                   title = "Population Size",
                   value = "18.1 million",
                   showcase = bs_icon("person-vcard-fill", fill = "primary"),
                   theme = value_box_theme(bg = "#e6f2fd", fg = "#006D77"),
                   p("Malawi's population in 2019 was predominantly rural and young.")
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
                          tags$th("Current policy tax rate (%)")
                        )
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td("1"),
                          tags$td("Less than 1.8"),
                          tags$td("0"),
                          tags$td(
                            numericInput(
                              "tax_rate_lowest", NULL, value = 0, min = 0, max = 50, step = .01, width = "100px"
                            )
                          )
                        ),
                        tags$tr(
                          tags$td("2"),
                          tags$td("More than 1.8 and less than 6"),
                          tags$td("25"),
                          tags$td(
                            numericInput(
                              "tax_rate_second", NULL, value = 25, min = 0, max = 50, step = .01, width = "100px"
                            )
                          )
                        ),
                        tags$tr(
                          tags$td("3"),
                          tags$td("More than 6 and less than 30.6"),
                          tags$td("30"),
                          tags$td(
                            numericInput(
                              "tax_rate_middle", NULL, value = 30, min = 0, max = 50, step = .01, width = "100px"
                            )
                          )
                        ),
                        tags$tr(
                          tags$td("4"),
                          tags$td("More than 30.6"),
                          tags$td("35"),
                          tags$td(
                            numericInput(
                              "tax_rate_top", NULL, value = 35, min = 0, max = 50, step = .01, width = "100px"
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
                        "The baseline rates reflect current policy classifications based on priority sectors, incorporation status, and business type.",
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
                            tags$th("Current policy tax rate (%)")
                          )
                        ),
                        tags$tbody(
                          tagList(
                            # tags$tr(
                            #   tags$td(0),
                            #   tags$td("Non specified"),
                            #   tags$td(15),
                            #   tags$td(numericInput("corp_tax_0", label = NULL, value = 15, min = 0, max = 100, step = 0.01, width = "100px"))
                            # ),
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
                              tags$td("Financial actitivies"),
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
                                "Specify the standard Value Added Tax (VAT) rate to be applied in the simulation. ",
                                "This rate affects taxable consumption based on survey-reported expenditure data. ",
                                tags$a("More info", href = "https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm", target = "_blank")
                              ),
                              
                              # VAT input field
                              tags$table(
                                class = "table table-borderless",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Description"),
                                    tags$th("Rate (%)")
                                  )
                                ),
                                tags$tbody(
                                  tags$tr(
                                    tags$td("Standard VAT Rate"),
                                    tags$td(
                                      numericInput(
                                        inputId = "vat_rate",
                                        label = NULL,
                                        value = 16.5,  # or your country-specific default
                                        min = 0,
                                        max = 100,
                                        step = 0.1,
                                        width = "100px"
                                      )
                                    )
                                  )
                                )
                              )
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
                              
                              # Table-style layout
                              tags$table(
                                class = "table table-borderless",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Item Code"),
                                    tags$th("Item Description"),
                                    tags$th("Excise Rate (%)")
                                  )
                                ),
                                    # Table body
                                    tags$tbody(
                                      tagList(
                                        # Items with non-zero excise rates (highest priority)
                                        tags$tr(tags$td("914"), tags$td("Wine or commercial liquor"), tags$td(numericInput("excise_item_914", label = NULL, value = 250, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("911"), tags$td("Bottled / canned beer (Carlsberg, etc)"), tags$td(numericInput("excise_item_911", label = NULL, value = 250, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("913"), tags$td("Traditional beer (masese)"), tags$td(numericInput("excise_item_913", label = NULL, value = 40, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("330"), tags$td("Cooking utensils (cookpots, stirring spoons and whisks, etc.)"), tags$td(numericInput("excise_item_330", label = NULL, value = 20, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("5801"), tags$td("Radio with flash drive/micro CD"), tags$td(numericInput("excise_item_5801", label = NULL, value = 20, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("211"), tags$td("Diesel"), tags$td(numericInput("excise_item_211", label = NULL, value = 10, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("813"), tags$td("Tomato sauce (bottle)"), tags$td(numericInput("excise_item_813", label = NULL, value = 10, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("333"), tags$td("Umbrella"), tags$td(numericInput("excise_item_333", label = NULL, value = 10, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("909"), tags$td("Bottled water"), tags$td(numericInput("excise_item_909", label = NULL, value = 5, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("519"), tags$td("Mini-bus"), tags$td(numericInput("excise_item_519", label = NULL, value = 5, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("517"), tags$td("Motorcycle/scooter"), tags$td(numericInput("excise_item_517", label = NULL, value = 5, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("611"), tags$td("Tractor"), tags$td(numericInput("excise_item_611", label = NULL, value = 5, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        
                                        # Items with zero excise rates (from your original UI that exist in the CSV)
                                        tags$tr(tags$td("506"), tags$td("Pork"), tags$td(numericInput("excise_item_506", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("507"), tags$td("Mutton"), tags$td(numericInput("excise_item_507", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        
                                        # If you want to include some common zero-rate items from the CSV:
                                        tags$tr(tags$td("106"), tags$td("Rice"), tags$td(numericInput("excise_item_106", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("111"), tags$td("Bread"), tags$td(numericInput("excise_item_111", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("101"), tags$td("Maize ufa mgaiwa (normal flour)"), tags$td(numericInput("excise_item_101", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("801"), tags$td("Sugar"), tags$td(numericInput("excise_item_801", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("803"), tags$td("Cooking oil"), tags$td(numericInput("excise_item_803", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px"))),
                                        tags$tr(tags$td("810"), tags$td("Salt"), tags$td(numericInput("excise_item_810", label = NULL, value = 0, min = 0, max = 1000, step = 0.01, width = "100px")))
                                      )
                                      )
                                      )
                                      )
                  ),
                  nav_panel(title = "Help",
                            value = "Help",help_indirect_tax_tab)
                )
        ),
      nav_panel(title = "Direct transferts", value = "direct_transferts", 
                
                card(
                  full_screen = FALSE,
                  fill = TRUE,
                  card_header(
                    bs_icon("info-circle-fill", size = "1.2em"),
                    "Direct transferts Input Form",
                    class = "info-box-header"
                  ),
                  p(
                    "This tab presents the structure of the direct transfert system used in the microsimulation model.",
                    tags$a(
                      " More information on the methodology is available here",
                      href = "https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm/file/1696511034124",
                      target = "_blank"
                    ), "."
                  )
                  
                ),
                navset_pill_list(
                  id = "sub_tabs_direct_cash",
                  nav_panel(title = "Direct cash transfers",  value = "direct_cash_transfers", 'Government direct cash transfer and Farm Input Subsidy Programme'
                            # Table-style input form
                            
                            
                  ), 
                  nav_panel(title = "Near-cash transfers ", value = "near_cash_transfers", "Free Maize"),
                  nav_panel(title = "Help",
                            value = "Help",help_direct_transf_tab)
                )
                
                ),
      nav_panel(title = "Subsidies", value = "subsidies", "Subsidies"),
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
    ) # end navset_pill_list
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
                # --- Geospatial analysis tab ---geo_pov_mod_ui("sim_geo_poverty")
                nav_panel(title = "Geospatial Poverty Analysis", value = "geospatial", geo_pov_mod_ui("sim_geo_poverty")),
                nav_panel(title = "Inequality", value = "inequality", ineq_mod_ui("sim_inequality")),
                # --- Other placeholder tabs ---
                nav_panel(title = "Income distribution", value = "income_distribution", "Income distribution"),
                nav_panel(title = "Net cash position", value = "net_cash_position", "Net cash position"),
                nav_panel(title = "Benefits in-kind", value = "benefits_in_kind", "Benefits in-kind")
                
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





