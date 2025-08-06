###########################################################################.
# MODULE: ineq_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.


ineq_mod_ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 500,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        accordion(
                          open = c("ineq_line_filter_panel"), # guided tour panel closed by default
                          multiple = TRUE, # allow multiple panels to be open at once
                          tags$h2(textOutput(outputId = ns("selected_parameter_label")), style = "color: #2c3e50; font-weight: bold;"),
                          # accordion panel with indicator filter and definitions button
                          accordion_panel(
                            value = "ineq_line_filter_panel",
                            
                            div(id = ns("ineq_line_wrapper"), 
                                
                                # all other geography filters
                                # note these filters are enabled/disabled in the server function based on selected indicator
                                layout_columns(
                                  radioButtons(inputId = ns("parameter_filter"), label = "Parameter to show:", choices = ineq_parameter_list),
                                  radioButtons(inputId = ns("areas_filter"), label = "Areas:", choices = ineq_area_list)
                                )
                                
                            )
                          )) # close all accordion
      ), # close sidebar
      
      
      # create a multi-tab card
      div(id = ns("ineq_card_wrapper"),
          navset_card_pill(
            id = ns("ineq_navset_card_pill"),
            full_screen = TRUE,
            
            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("ineq_chart_tab"), #id for guided tour
                      uiOutput(ns("ineq_title")), # title
                      uiOutput(ns("ineq_caveats")), # caveats
                      highchartOutput(outputId = ns("ineq_chart")) %>% # chart
                        withSpinner() %>% 
                        bslib::as_fill_carrier()
            ),
            
            # data tab ------------------
            nav_panel("Data",
                      value = ns("ineq_data_tab"), #id for guided tour
                      reactableOutput(ns("ineq_table")) # table
            ),
            # Interpretation tab 
            nav_panel(
              title = "Help",
              uiOutput(ns("help_ineq_chart_tab"))
            ),
            
            # footer with download buttons
            footer = card_footer(class = "d-flex justify-content-left",
                                 div(id = ns("ineq_download_chart"), download_chart_mod_ui(ns("download_ineq_chart"))),
                                 div(id = ns("ineq_download_data"), download_data_btns_ui(ns("download_ineq_data"))))
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


ineq_mod_server <- function(id, simulated_ineq, root_session) {
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # Create reactive microsim data
    # microsim_data <- create_reactive_microsim_data()
    
    # create reactive data - filtering by selected indicator
    filtered_microsim_data <- reactive({
      req(simulated_ineq())
      
      
      temp_data <- simulated_ineq()
      
      # Apply filters based on user inputs
      if(!is.null(input$parameter_filter)) {
        temp_data <- temp_data %>% filter(Parameter == input$parameter_filter)
      }
      
      if(!is.null(input$areas_filter)) {
        temp_data <- temp_data %>% filter(Area == input$areas_filter)
      }
      
      
      temp_data
    })
    
    
    
    
    #######################################################.
    ## Dynamic text  ----
    #######################################################.
    
    output$ineq_title <- renderUI({
      req(filtered_microsim_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(filtered_microsim_data()$Parameter)
      
      # Create a clearer title based on the parameter
      parameter_title <- case_when(
        selected_parameter == "Gini index" ~ "Gini",
        selected_parameter == "Theil index" ~ "Theil",
        selected_parameter == "90/10 income ratio" ~ "90/10 income ratio",
        TRUE ~ selected_parameter
      )
      
      # display titles with improved clarity
      div(
        tags$h5(paste0("Inequality Indicator: ", parameter_title), class = "chart-header"), # selected Parameter with clearer label
        tags$h6(paste0("Area: ", first(filtered_microsim_data()$Area))) # selected Area
      )
      
    })
    
    output$selected_parameter_label <-  renderText({
      req(filtered_microsim_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(filtered_microsim_data()$Parameter)
      
      selected_parameter
    })
    
    
    #############################################.
    # Charts/tables ----
    #############################################.
    
    # inequality chart
    output$ineq_chart <- renderHighchart({
      req(filtered_microsim_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(filtered_microsim_data()$Parameter)
      
      # Create a clearer chart title based on the parameter
      chart_title <- case_when(
        selected_parameter == "Gini index" ~ "Gini",
        selected_parameter == "Theil index" ~ "Theil",
        selected_parameter == "90/10 income ratio" ~ "90/10 income ratio",
        TRUE ~ paste0(selected_parameter)
      )
      
      create_ineq_bar_chart(filtered_microsim_data(),
                           xaxis_col = "Income",
                           yaxis_col = "Current Policy") %>% 
        hc_exporting(
          filename = paste0("Inequality - ", first(filtered_microsim_data()$Parameter), " - ",
                            first(filtered_microsim_data()$Area)),
          chartOptions = list(
            title = list(text = chart_title),
            subtitle = list(
              text = paste0(
                "Area: ", input$areas_filter
              ),
              useHTML = TRUE  
            ))
        )
      
    })
    
    
    # data table
    output$ineq_table <- renderReactable({
      req(filtered_microsim_data())
      
      df_table <- filtered_microsim_data() %>% 
        select(Income, `Pre-reform`, `Current Policy`) %>% 
        mutate(diff = round(`Pre-reform` - `Current Policy`,3))
      
      # Get the selected parameter for better column labeling
      selected_parameter <- first(filtered_microsim_data()$Parameter)
      
      # Create clearer column names based on the parameter
      value_col_name <- case_when(
        selected_parameter == "Gini index" ~ "Gini",
        selected_parameter == "Theil index" ~ "Theil",
        selected_parameter == "90/10 income ratio" ~ "90/10 income ratio",
        TRUE ~ paste0(selected_parameter)
      )
      
      # Use the filtered data directly
      reactable(df_table,
                columns = list(
                  Income = colDef(name = "Income Concept"),
                  `Pre-reform` = colDef(name = paste0("Pre-reform ", value_col_name)),
                  `Current Policy` = colDef(name = paste0("Current Policy ", value_col_name)),
                  diff = colDef(name = "Difference")
                )
      )
      
    })
    
    
    ###################################.
    # Downloads ----
    ###################################.
    
    # server for chart and data downloads
    download_chart_mod_server(id = "download_ineq_chart", chart_id = ns("ineq_chart"))
    
    download_data_btns_server(id = "download_ineq_data",
                              data = simulated_ineq(),
                              file_name = "inequality_data_extract") # rename column
    
    # Render help content
    output$help_ineq_chart_tab <- renderUI({
      help_ineq_chart_tab
    })
    
    
    
  }) # close moduleServer
} # close server function
