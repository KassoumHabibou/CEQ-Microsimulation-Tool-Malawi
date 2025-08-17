###########################################################################.
# MODULE: pov_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.

pov_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(
        width = 500,
        open = list(mobile = "always-above"),
        accordion(
          open = c("pov_line_filter_panel"),
          multiple = TRUE,
          tags$h2(
            textOutput(outputId = ns("selected_parameter_label")),
            style = "color: #2c3e50; font-weight: bold;"
          ),
          accordion_panel(
            value = "pov_line_filter_panel",
            div(
              id = ns("pov_line_wrapper"),
              selectizeInput(
                ns("pov_line"),
                label = "Select the Poverty Line",
                choices = c(
                  "National poverty line (454 MWK per day in 2019/2020)" = "National poverty line (454 MWK per day)",
                  "Poverty line for lower income countries (2.15 USD or 656.7 MKW per day PPP)" = "Lower income class poverty line (656.7 MKW per day)",
                  "Poverty line for lower middle income countries (3.65 USD or 1115 MKW per day PPP)" = "Middle income class poverty line (1115 MKW per day)"
                ),
                selected = "National poverty line (454 MWK per day in 2019/2020)"
              ),
              layout_columns(
                radioButtons(
                  inputId = ns("parameter_filter"),
                  label = "Parameter to show:",
                  choices = setdiff(pov_parameter_list, "Welfare")
                ),
                radioButtons(
                  inputId = ns("areas_filter"),
                  label = "Areas:",
                  choices = pov_area_list
                )
              )
            )
          )
        )
      ),
      
      # main content with footer inside navset
      div(
        id = ns("pov_card_wrapper"),
        navset_card_pill(
          id = ns("pov_navset_card_pill"),
          full_screen = TRUE,
          
          nav_panel(
            "Charts",
            value = ns("pov_chart_tab"),
            uiOutput(ns("pov_title")),
            uiOutput(ns("pov_caveats")),
            highchartOutput(outputId = ns("pov_chart")) %>%
              withSpinner() %>%
              bslib::as_fill_carrier()
          ),
          nav_panel(
            "Data",
            value = ns("pov_data_tab"),
            reactableOutput(ns("pov_table"))
          ),
          nav_panel(
            title = "Help",
            uiOutput(ns("help_pov_chart_tab"))
          ),
          
          # footer with download buttons
          footer = card_footer(
            class = "d-flex justify-content-left",
            div(id = ns("pov_download_chart"), download_chart_mod_ui(ns("download_pov_chart"))),
            div(id = ns("pov_download_data"), download_data_btns_ui(ns("download_pov_data")))
          )
        )
      )
    ),
    
    # World Bank logo and About section ----
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
}



#######################################################.
## MODULE SERVER ----
#######################################################.


pov_mod_server <- function(id, simulated_pov, root_session) {
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
      req(simulated_pov())
      
      
      temp_data <- simulated_pov()
      
      # Apply filters based on user inputs
      if(!is.null(input$parameter_filter)) {
        temp_data <- temp_data %>% filter(Parameter == input$parameter_filter)
      }
      
      if(!is.null(input$areas_filter)) {
        temp_data <- temp_data %>% filter(Area == input$areas_filter)
      }

      if(!is.null(input$pov_line)) {
        temp_data <- temp_data %>% filter(`Poverty line` == input$pov_line)
      }
      
      temp_data
    })




    #######################################################.
    ## Dynamic text  ----
    #######################################################.

    output$pov_title <- renderUI({
      req(filtered_microsim_data())

      # Get the selected parameter for better labeling
      selected_parameter <- first(filtered_microsim_data()$Parameter)
      
      # Create a clearer title based on the parameter
      parameter_title <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor",
        selected_parameter == "Rate of poverty" ~ "Poverty Rate",
        selected_parameter == "Poverty gap" ~ "Poverty Gap",
        selected_parameter == "Poverty severity" ~ "Poverty Severity",
        TRUE ~ selected_parameter
      )

      # display titles with improved clarity
      div(
        tags$h5(paste0("Poverty Indicator: ", parameter_title), class = "chart-header"), # selected Parameter with clearer label
        tags$h6(paste0("Area: ", first(filtered_microsim_data()$Area))), # selected Area
        tags$h6(paste0("Poverty line: ", first(filtered_microsim_data()$`Poverty line`))) # selected Poverty line
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

    # Poverty chart
    output$pov_chart <- renderHighchart({
      req(filtered_microsim_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(filtered_microsim_data()$Parameter)
      
      # Create a clearer chart title based on the parameter
      chart_title <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor People by Income Concept",
        selected_parameter == "Rate of poverty" ~ "Poverty Rate by Income Concept",
        selected_parameter == "Poverty gap" ~ "Poverty Gap by Income Concept",
        selected_parameter == "Poverty severity" ~ "Poverty Severity by Income Concept",
        TRUE ~ paste0(selected_parameter, " by Income Concept")
      )
      
        create_pov_bar_chart(filtered_microsim_data(),
                         xaxis_col = "Income",
                         yaxis_col = "Post-reform") %>% 
          hc_exporting(
            filename = paste0("Poverty - ", first(filtered_microsim_data()$Parameter), " - ",
                              first(filtered_microsim_data()$Area), " - ",
                              first(filtered_microsim_data()$`Poverty line`)),
            chartOptions = list(
              title = list(text = chart_title),
              subtitle = list(
                text = paste0(
                  "Area: ", input$areas_filter,
                  "<br>",
                  "Poverty line: ",  input$pov_line
                ),
                useHTML = TRUE  
              ))
            )

    })


    # data table
    output$pov_table <- renderReactable({
      req(filtered_microsim_data())
      

      df_table <- filtered_microsim_data() %>% 
        select(Income, `Pre-reform`, `Post-reform`) %>% 
        mutate(diff = `Post-reform` - `Pre-reform`) %>% 
        mutate(impact = diff*100/`Pre-reform`) %>% 
        select(Income, `Pre-reform`, `Post-reform`, diff, impact) %>% 
        arrange(impact)
        
      # Get the selected parameter for better column labeling
      selected_parameter <- first(filtered_microsim_data()$Parameter)
      
      # Create clearer column names based on the parameter
      value_col_name <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor People",
        selected_parameter == "Rate of poverty" ~ "Poverty Rate (%)",
        selected_parameter == "Poverty gap" ~ "Poverty Gap (%)",
        selected_parameter == "Poverty severity" ~ "Poverty Severity",
        TRUE ~ selected_parameter
      )
      
      # Use the filtered data directly
      reactable(df_table,
                columns = list(
                  Income = colDef(name = "Income Concept"),
                  `Pre-reform` = colDef(name = paste0("Pre-reform ", value_col_name)),
                  `Post-reform` = colDef(name = paste0("Post-reform ", value_col_name)),
                  diff = colDef(name = "Diff (Post - Pre)"),
                  impact = colDef(name = paste0("Impact (%)", value_col_name))
                )
      )

    })


    ###################################.
    # Downloads ----
    ###################################.

    # server for chart and data downloads
    download_chart_mod_server(id = "download_pov_chart", chart_id = ns("pov_chart"))

    download_data_btns_server(id = "download_pov_data",
                              data = simulated_pov(),
                              file_name = "Poverty_data_extract") # rename column

    # Render help content
    output$help_pov_chart_tab <- renderUI({
      help_pov_chart_tab
    })

 

  }) # close moduleServer
} # close server function
