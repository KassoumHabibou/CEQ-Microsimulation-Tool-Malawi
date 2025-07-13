###########################################################################.
# MODULE: pov_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.


pov_mod_ui <- function(id) {
  ns <- NS(id)
  
  # Get global variables if they exist
  pov_parameter_list <- if (exists("pov_parameter_list")) pov_parameter_list else 
    c("Rate of poverty", "Number of poor", "Poverty gap", "Poverty severity")
  
  pov_area_list <- if (exists("pov_area_list")) pov_area_list else 
    c("Country", "Urban", "Rural")
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 500,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        accordion(
                          open = c("pov_line_filter_panel", "specification_filter_panel"), # guided tour panel closed by default
                          multiple = TRUE, # allow multiple panels to be open at once

                          # accordion panel with indicator filter and definitions button
                          accordion_panel(
                            value = "pov_line_filter_panel",
                            "Select a poverty line",
                            div(id = ns("pov_line_wrapper"), 
                            #indicator filter (note this is a module)
                            selectizeInput(ns("pov_line"), 
                                           label = "Poverty line",
                                           choices = c("National Poverty line" = "National poverty line (454 MWK per day)",
                                                       "Poverty line for lower income countries (2.15 USD per day PPP)" = "Lower income class poverty line (656.7 MKW per day)",
                                                       "Poverty line lower middle income countries (3.65 USD per day PPP)" = "Middle income class poverty line (1114.8 MKW per day)"),
                                           selected = "National poverty line (454 MWK per day)")

                          )),

                          # accordion panel with geography filters
                          accordion_panel(
                            value = "specification_filter_panel",
                            "Specify",

                            div(id = ns("specification_wrapper"), #wrapping for tour guide

                                # all other geography filters
                                # note these filters are enabled/disabled in the server function based on selected indicator
                                layout_columns(
                                  radioButtons(inputId = ns("parameter_filter"), label = "Parameter to show:", choices = pov_parameter_list),
                                  radioButtons(inputId = ns("areas_filter"), label = "Areas:", choices = pov_area_list)
                                )

                            ))
                        ) # close all accordion
      ), # close sidebar


      # create a multi-tab card
      div(id = ns("pov_card_wrapper"),
          navset_card_pill(
            id = ns("pov_navset_card_pill"),
            full_screen = TRUE,

            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("pov_chart_tab"), #id for guided tour
                      uiOutput(ns("pov_title")), # title
                      uiOutput(ns("pov_caveats")), # caveats
                      highchartOutput(outputId = ns("pov_chart")) %>% # chart
                        withSpinner() %>% 
                        bslib::as_fill_carrier()
            ),

            # data tab ------------------
            nav_panel("Data",
                      value = ns("pov_data_tab"), #id for guided tour
                      reactableOutput(ns("pov_table")) # table
            ),


            # footer with download buttons
            footer = card_footer(class = "d-flex justify-content-left",
                                 div(id = ns("pov_download_chart"), download_chart_mod_ui(ns("download_pov_chart"))),
                                 div(id = ns("pov_download_data"), download_data_btns_ui(ns("download_pov_data"))))
          )
      ) # close navset card pill

      # accordion panel with metadata table
      # div(id = ns("metadata_section"), metadata_panel_UI(ns("metadata_table")))
    ) # close layout sidebar


  ) # close taglist
} # close ui function



#######################################################.
## MODULE SERVER ----
#######################################################.


pov_mod_server <- function(id, simulated_data, root_session) {
  moduleServer(id, function(input, output, session) {

    # permits compatibility between shiny and cicerone tours
    ns <- session$ns

    # Define pov_parameter_list if not already defined
    if (!exists("pov_parameter_list")) {
      pov_parameter_list <- c("Rate of poverty", "Number of poor", "Poverty gap", "Poverty severity")
    }

    #######################################################.
    ## Reactive data / values ----
    #######################################################.

    # Create reactive microsim data
    # microsim_data <- create_reactive_microsim_data()

    # create reactive data - filtering by selected indicator
    filtered_microsim_data <- reactive({
      req(simulated_data())
      
      #print(str(simulated_data()))
      
      simulated_data()
      
      # Apply filters based on user inputs
      if(!is.null(input$parameter_filter)) {
        temp_data <- simulated_data() %>% filter(Parameter == input$parameter_filter)
      }
      
      if(!is.null(input$areas_filter)) {
        temp_data <- simulated_data() %>% filter(Area == input$areas_filter)
      }
      
      temp_data
    })




    #######################################################.
    ## Dynamic text  ----
    #######################################################.

    output$pov_title <- renderUI({
      req(filtered_microsim_data())

      # display 3 x titles
      div(
        tags$h5(first(filtered_microsim_data()$Parameter), class = "chart-header"), # selected Parameter
        tags$h6("National Poverty line") # selected 
      )

    })


    #############################################.
    # Charts/tables ----
    #############################################.

    # Poverty chart
    output$pov_chart <- renderHighchart({
      req(filtered_microsim_data())
      
      # Create a simple bar chart if create_pov_bar_chart is not available
      if (exists("create_pov_bar_chart")) {
        create_pov_bar_chart(data = filtered_microsim_data(),
                         xaxis_col = "Income",
                         yaxis_col = "Current Policy",
                         colour_palette = "simd") %>% 
          hc_exporting(
            filename = paste0("Poverty - ", first(filtered_microsim_data()$Parameter)),
            chartOptions = list(
              title = list(text = paste0(first(filtered_microsim_data()$Parameter))),
              subtitle = list(text = 'National Poverty line')
            )
          )
      } else {
        # Fallback chart
        highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = filtered_microsim_data()$Income) %>%
          hc_yAxis(title = list(text = "Value")) %>%
          hc_series(
            list(
              name = "Current Policy",
              data = filtered_microsim_data()$`Current Policy`
            )
          ) %>%
          hc_title(text = paste0("Poverty - ", first(filtered_microsim_data()$Parameter))) %>%
          hc_subtitle(text = "National Poverty line")
      }
    })


    # data table
    output$pov_table <- renderReactable({
      req(filtered_microsim_data())
      
      # Use the filtered data directly
      reactable(filtered_microsim_data(),
                columns = list(
                  Income = colDef(name = "Income concepts"),
                  Parameter = colDef(name = "Parameter"),
                  Area = colDef(name = "Area"),
                  `Poverty line` = colDef(name = "Poverty line"),
                  `Current Policy` = colDef(name = "Current Policy")
                )
      )

    })


    ###################################.
    # Downloads ----
    ###################################.

    # server for chart and data downloads
    download_chart_mod_server(id = "download_pov_chart", chart_id = ns("pov_chart"))

    download_data_btns_server(id = "download_pov_data",
                              data = simulated_data(),
                              file_name = "Poverty_data_extract") # rename column


 

  }) # close moduleServer
} # close server function
