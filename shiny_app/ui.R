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

# this first part of the UI creates a purple navigation bar to place individual tabs in
# it's also where some external script are sourced that are required for different part of the app to work
page_navbar(
  fillable = TRUE, # controlling how items grow/shrink when browser different sizes
  window_title = "Malawi CEQ",
  id = "nav", # id required for profile buttons - works with profile_homepage_btn_mod to control navigation
  lang = "en",
  theme = phs_theme, # dashboard theme - defined in global script
  navbar_options = navbar_options(
    bg = phs_colours(colourname = "phs-teal"), # background navbar colour
    collapsible = TRUE # collapse tabs on smaller screens
  ),
  # place external scripts in footer argument to avoid warnings as recommended by package developers
  header = tags$head(
    useShinyjs(), # need to declare this to use functions from the shinyjs package, e.g. to show/hide parts of the UI
    use_cicerone(), # required for guided tours
    includeCSS("www/styles.css") # required to specify formatting (particularly of landing page)
  ),
  
  #######################################.
  # Homepage ----
  ######################################.
  # this tab is the homepage of the app. Note that some of the code to create this landing page sits in a seperate HTML file
  # the profile_homepage_btn_mod_UI is a module that creates a button to navigate to the profiles tab of the app.
  # It also updates the profile filter depending on what profile you select
  
  nav_panel(value = "home", style = "margin: 1; padding:1;", # remove margin so no white space at top of landing page
            title = "Home",
            style = "background-color:#F2F2F2;",
            p("First tab content. welcome page....")
  ), # close homepage
  
  ############################################.
  # Policy choice TAB ----
  ############################################.
  nav_panel(title = "Policy choice", value = "policy_choice", style = "margin: 1; padding:1;", # remove margin so no white space at top of landing page

              navset_pill(id = "sub_tabs_input",  

                             # --- Direct tax tab ---
                             nav_panel(title = "Direct tax", value = "direct_tax", 
                                       
                                       card(full_screen = TRUE,fill=TRUE,
                                            card_header(bs_icon("info-circle-fill", size = "1.2em"), 
                                                        "Direct Tax Input Form", class = "info-box-header"),
                                            
                                            p("Explanation..... ",
                                              tags$a("More information on the methodology",
                                                     href = "https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm/file/1696511034124",
                                                     target = "_blank"), "."
                                            )),
                                            
                                            h3("Income tax"),
                                            # Table-style input form
                                            tags$table(class = "table table-borderless",
                                                       tags$thead(
                                                         tags$tr(
                                                           tags$th("Bracket"),
                                                           tags$th("Upper income threshold (in million kwacha) per year"),
                                                           tags$th("Baseline tax rate in %"),
                                                           tags$th("Tax rate in %"),
                                                         )
                                                       ),
                                                       tags$tbody(
                                                         tags$tr(
                                                           tags$td("1"),
                                                           tags$td("Less than 1.8"),
                                                           tags$td("0"),
                                                           tags$td(numericInput("tax_rate_lowest", NULL, value = 0, min = 0, max=50, step = .01, width="100px"))
                                                         ),
                                                         tags$tr(
                                                           tags$td("2"),
                                                           tags$td("More than 1.8 and less than 6"),
                                                           tags$td("25"),
                                                           tags$td(numericInput("tax_rate_second", NULL, value = 25, min = 0, max=50, step = .01, width="100px"))
                                                         ),
                                                         tags$tr(
                                                           tags$td("3"),
                                                           tags$td("More than 6 and less than 30.6"),
                                                           tags$td("30"),
                                                           tags$td(numericInput("tax_rate_middle", NULL, value = 30, min = 0, max=50, step = .01, width="100px"))
                                                         ),
                                                         tags$tr(
                                                           tags$td("4"),
                                                           tags$td("More than 30.6"),
                                                           tags$td("35"),
                                                           tags$td(numericInput("tax_rate_top", NULL, value = 35, min = 0, max=50, step = .01, width="100px"))
                                                         )
                                                       )
                                            )
                                       
                             ),
                             
                             # --- Other placeholder tabs ---
                             nav_panel(title = "Indirect taxes", value = "indirect_taxes", "Indirect taxes"),
                             nav_panel(title = "Direct transferts", value = "direct_transferts", "Direct transferts"),
                             nav_panel(title = "Subsidies", value = "subsidies", "Subsidies"),
                             nav_panel(title = "Benefits in-kind", value = "benefits_in_kind", "Benefits in-kind"),
                             nav_panel(title = "Poverty line", value = "poverty_line", "Poverty line"),
                             nav_spacer(), # add space to navbar 
                             nav_item(loadingButton("simulate_button", label = "Simulate")),
                          # conditionalPanel(condition = "input.simulate_button > 0", h2("View"))
                          #  )
                             
            ) # end navset_pill_list
  ), # end nav_panel "Policy choice", # close entire profiles tab

  ############################################.
  # Policy Results TAB ----
  ############################################.
  nav_panel(title = "Results", value = "results", style = "margin: 1; padding:1;", # remove margin so no white space at top of landing page
            
            navset_pill(id = "sub_tabs_results",  
                         # nolint
                        # --- Direct tax tab ---
                        nav_panel(title = "Poverty", value = "poverty", 
                                  
                                  
                                  h3("Poverty")
                                  
                        ),
                        
                        # --- Other placeholder tabs ---
                        nav_panel(title = "Income distribution", value = "income_distribution", "Income distribution"),
                        nav_panel(title = "Inequality", value = "Inequality", "Inequality"),
                        nav_panel(title = "Net cash position", value = "net_cash_position", "Net cash position"),
                        nav_panel(title = "Benefits in-kind", value = "benefits_in_kind", "Benefits in-kind"),
                        nav_panel(title = "Poverty line", value = "poverty_line", "Poverty line"),
                        nav_spacer()
                        )
                        # conditionalPanel(condition = "input.simulate_button > 0", h2("View"))
                        #  )
                        
             # end navset_pill_list
  ), # end nav_panel "Results choice"
  
  
  nav_spacer(), # add space to navbar 
  ########################################.
  # Link to github repo  -------
  ########################################.
  nav_item(tags$a(icon("github"), "SourceCode", href = "https://github.com/KassoumHabibou/Malawi-CEQ-Assessment-app", target = "_blank")),
  
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
  
) #close main ui function

### END





