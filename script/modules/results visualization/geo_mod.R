# to do :
# sort time comparison dummbell chart colours?
# add mini app to show how module works

########################################################.
# MODULE: geo_charts_mod ---- 
# prepares the geo section 
########################################################.



#######################################################.
## MODULE UI ----
#######################################################.
# id = unique id 
geo_pov_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    layout_sidebar(
      full_screen = FALSE,
      height = "90%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 500,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        
                        accordion(
                          open = c("geo_pov_line_filter_panel"), # guided tour panel closed by default
                          multiple = TRUE, # allow multiple panels to be open at once
                          
                          tags$h2(textOutput(outputId = ns("selected_parameter_label")), style = "color: #2c3e50; font-weight: bold;"),
                          # accordion panel with indicator filter and definitions button
                          accordion_panel(
                            value = "geo_pov_line_filter_panel",
                            "Select a poverty line",
                            div(id = ns("geo_pov_line_wrapper"), 
                                #indicator filter (note this is a module)
                                selectizeInput(ns("geo_pov_line"), 
                                               label = "Poverty line",
                                               choices = c( "National poverty line (454 MWK per day 2019/2020)" =  "National poverty line (454 MWK per day)",
                                                            "Poverty line for lower income countries (2.15 USD or 656.7 MKW per day PPP)" = "Lower income class poverty line (656.7 MKW per day)",
                                                            "Poverty line for lower middle income countries (3.65 USD or 1115 MKW per day PPP)" = "Middle income class poverty line (1115 MKW per day)"),
                                               selected = "National poverty line (454 MWK per day)"),
                                
                                radioButtons(inputId = ns("geo_income"), label = "Income concept", choices = pov_geo_income_list),
                                
                                # all other geography filters
                                # note these filters are enabled/disabled in the server function based on selected indicator
                                  radioButtons(inputId = ns("geo_pov_parameter_filter"), label = "Parameter to show:", choices = setdiff(pov_parameter_list,"Welfare")),
                                  radioButtons(inputId = ns("geo_pov_areas_filter"), label = "Areas:", choices = pov_geo_area_list)
                                
                            )
                        ) # close all accordion
                )
      ), # close sidebar
      
      layout_column_wrap(
      #col_widths = c(8, 4), # Chart gets 8/12 width, map gets 4/12 width
        # bar chart card ----------------------
        navset_card_pill(
          id = ns("geo_pov_navset_card_pill"),
          full_screen = TRUE,
         
          
          # charts tab
          nav_panel("Charts",
                    value = ns("geo_pov_chart_tab"), #id for guided tour
                    uiOutput(ns("geo_pov_title_chart")), # title
                    uiOutput(ns("geo_pov_caveats")), # caveats
                    highchartOutput(outputId = ns("geo_pov_chart")) %>%  # chart
                      withSpinner() %>%  
                      bslib::as_fill_carrier() 
          ),
          # data tab
          nav_panel("Maps",
                    value = ns("geo_pov_map_wrapper"),
                    uiOutput(ns("geo_pov_maps_caveats")), # caveats,
                    card(
                    leafletOutput(ns("geo_pov_map")) %>%  # map
                            withSpinner() %>%
                            bslib::as_fill_carrier())
                        ),
          # 
          # data tab
          nav_panel("Data",
                    value = ns("geo_pov_data_tab"), #id for guided tour
                    reactableOutput(ns("geo_pov_table")) # table
          ),
          
          # Interpretation tab 
          nav_panel(
            title = "Help",
            uiOutput(ns("help_geospatial_pov_tab"))
          ),

          footer = card_footer(class = "d-flex justify-content-left",
                               div(id = ns("geo_pov_download_chart"), download_chart_mod_ui(ns("download_geo_pov_chart"))),
                               div(id = ns("geo_pov_download_data"), download_data_btns_ui(ns("download_geo_pov_data"))))
        )

      ) # close layout column wrap
    ) # close layout sidebar
  ) # close taglist
} # close ui function 


#######################################################.
## MODULE SERVER ----
#######################################################.

# id = unique id 
# profile_data = reactive dataframe created in main server script contains main data already filtered by profile
# geo_selections = reactive values in main server stores global geography selections
# selected_profile = name of reactive value stores selected profile from main server script

geo_pov_mod_server <- function(id, simulated_geo, root_session) {
  moduleServer(id, function(input, output, session) {
    
    
    # permits compatibility between shiny and cicerone tours
    # req(active_nav() == nav_id)
    
    ns <- session$ns
    
    #######################################################.
    # Dynamic filters ----
    #######################################################.

    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    
    
    # prepares data to be plotted --------------------------------------------
    geo_data <- reactive({
      req(simulated_geo())
      
      temp_data <- simulated_geo() 
      
      # Apply filters based on user inputs
      if(!is.null(input$geo_pov_parameter_filter)) {
        temp_data <- temp_data %>% filter(Parameter == input$geo_pov_parameter_filter)
      }
      
      if(!is.null(input$geo_pov_areas_filter)) {
        temp_data <- temp_data %>% filter(Area == input$geo_pov_areas_filter)
      }
      
      if(!is.null(input$geo_pov_line)) {
        temp_data <- temp_data %>% filter(`Poverty line` == input$geo_pov_line)
      }

      if(!is.null(input$geo_income)) {
        temp_data <- temp_data %>% filter(Income == input$geo_income)
      }      
      
      
      temp_data <- temp_data %>% 
        mutate(Impact = round((`Pre-reform` - `Current Policy`)*100/(`Pre-reform`),2))
      
      
      
      temp_data
    })
    
    
    
    
    # map data --------------------------------------
    # dynamically selects shapefile and joins with map data
    map_data <- reactive({
      req(geo_data())

      
      # get correct shapefile
      x <- switch(input$geo_pov_areas_filter,
                  "region" = mlw_bound_region,
                  "district" = mlw_bound_district
      )
      
      x <- x %>%  left_join(geo_data())

      x
      
    })
    
    
    #######################################################.
    ## Dynamic text  ----
    #######################################################.
    
    # title
    output$geo_pov_title <- renderUI({
      req(geo_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(geo_data()$Parameter)
      
      # Create a clearer title based on the parameter
      parameter_title <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor People",
        selected_parameter == "Rate of poverty" ~ "Poverty Rate",
        selected_parameter == "Poverty gap" ~ "Poverty Gap",
        selected_parameter == "Poverty severity" ~ "Poverty Severity",
        TRUE ~ selected_parameter
      )
      
      # prepare description of what map/chart show depending on
      # whether comparator included (and if so which comparator)
      # display titles with improved clarity
      div(
        tags$h5(paste0("Poverty Indicator: ", parameter_title)), # selected Parameter with clearer label
        tags$h6(paste0("Income: ", first(geo_data()$Income))), # selected 
        tags$h6(paste0("Area: ", first(geo_data()$Area))), # selected 
        tags$h6(paste0("Poverty line: ", first(geo_data()$`Poverty line`))) # selected 
      )
      
    })
    
    # title
    output$geo_pov_title_chart <- renderUI({
      req(geo_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(geo_data()$Parameter)
      
      # Create a clearer title based on the parameter
      parameter_title <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor People",
        selected_parameter == "Rate of poverty" ~ "Poverty Rate",
        selected_parameter == "Poverty gap" ~ "Poverty Gap",
        selected_parameter == "Poverty severity" ~ "Poverty Severity",
        TRUE ~ selected_parameter
      )
      
      # prepare description of what map/chart show depending on
      # whether comparator included (and if so which comparator)
      # display titles with improved clarity
      div(
        tags$h5(paste0("Poverty Indicator: ", parameter_title), class = "chart-header"), # selected Parameter with clearer label
        tags$h6(paste0("Income: ", first(geo_data()$Income))), # selected 
        tags$h6(paste0("Area: ", first(geo_data()$Area))), # selected 
        tags$h6(paste0("Poverty line: ", first(geo_data()$`Poverty line`))) # selected 
      )
      
    })
    
    #############################################.
    # Visualisations / data tables  ----
    #############################################.
    
    # chart (barchart/dumbell chart)
    output$geo_pov_chart <- renderHighchart({
      req(geo_data())
      
      
      # Reshape to long format for plotting
      plot_data_long <- geo_data() %>%
        pivot_longer(cols = c("Current Policy", "Pre-reform"),
                     names_to = "Policy",
                     values_to = "Value")
      
      hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Current Policy" = "#006D77")
      
      plot_data_long <- plot_data_long %>%
        mutate(Policy = factor(Policy, levels = c("Pre-reform", "Current Policy")),
               admin_name = factor(admin_name),
               color = hc_colors_vec[as.character(Policy)])
      
      #browser()
      # Create dumbbell chart
      # Plot using highcharter
      hc <- hchart(
        plot_data_long,
        type = "column",
        hcaes(x = admin_name, y = Value, group = Policy)) %>% 
        hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "")) %>% 
        #hc_add_theme(theme) %>% 
        hc_plotOptions(column = list(groupPadding = 0.1)) %>% 
        hc_tooltip(shared = TRUE)%>%
        hc_caption(
          text = "Data source: Malawi Fifth Integrated Household Survey 2019-2020",
          style = list(fontSize = "8px", color = "black")
        ) %>% 
        hc_exporting(
          filename = paste0("Geo Poverty - ", first(geo_data()$Parameter), " - ",
                            first(geo_data()$Area), " - ",
                            first(geo_data()$`Poverty line`)),
          chartOptions = list(
            title = list(text = input$geo_pov_parameter_filter),
            subtitle = list(
              text = paste0(
                "Income : ", input$geo_income,
                "<br>",
                "Area : ", input$geo_pov_areas_filter,
                "<br>",
                "Poverty line : ",  input$geo_pov_line
              ),
              useHTML = TRUE  
            ))
        )
      
      
      
      # Plot using highcharter
      # hchart(plot_data_long, 
      #              type = "bar", # 'bar' = horizontal bar chart
      #              hcaes(x = admin_name, y = Value, group = Policy)) %>%
      #   hc_colors(unname(hc_colors_vec[levels(factor(plot_data_long$Policy))])) %>%
      #   hc_xAxis(title = list(text = ""), categories = unique(plot_data_long$admin_name)) %>%
      #   hc_yAxis(title = list(text = "")) %>%
      #   hc_tooltip(
      #     headerFormat = "<table>",
      #     pointFormat = paste(
      #       '<tr><th colspan="2"><p>{point.areaname}</p></th></tr>',
      #       "<tr><th>{point.type_definition}</th><td>{point.measure}</td></tr>",
      #       "<tr><th>upper ci:</th><td>{point.upci}</td></tr>",
      #       "<tr><th>lower ci:</th><td>{point.lowci}</td></tr>"
      #     ),
      #     footerFormat = "</table>",
      #     useHTML = TRUE
      #   ) %>% 
      #   hc_legend(enabled = TRUE) %>%
      #   hc_chart(backgroundColor = 'white') %>%
      #   hc_caption(
      #     text = "Data source: Malawi Fifth Integrated Household Survey 2019-2020",
      #     style = list(fontSize = "8px", color = "black")
      #   )
      
      #browser()  
      # create_geo_pov_bar_chart(geo_data(),
      #                      xaxis_col = "admin_name",
      #                      yaxis_col = "Current Policy") %>% 
      #   hc_exporting(
      #     filename = paste0("Poverty - ", 
      #                       first(geo_data()$Parameter), " - ",
      #                       first(geo_data()$Income), " - ",
      #                       first(geo_data()$Area), " - ",
      #                       first(geo_data()$`Poverty line`))
      #   )
      
    })
    
    
    # leaflet map -------
    
    # Global definition of value_palette
    value_palette <- reactive({
      req(map_data())

        colorNumeric(palette = "RdYlGn", domain = map_data()$Impact)
    })
    
    
    
    output$geo_pov_map <- renderLeaflet({
      req(map_data())
      
      leaflet(map_data()) %>% 
        # Set initial view to Malawi coordinates
        setView(lng = 34.0, lat = -13.5, zoom = 6) %>%
        addProviderTiles(provider = providers[["OpenStreetMap"]]) %>% 
        addPolygons(data = map_data(), weight = 1, color = "black",
                    fillColor = ~value_palette()(Impact),
                    fillOpacity = 0.5, 
                    smoothFactor = 0.5, 
                    opacity = 1, 
                    label = ~paste0(map_data()$admin_name, ": ", map_data()$Impact),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
        addLegend(pal = value_palette(), values = ~Impact) %>% 
        # add option to save chart as png
        onRender(
          "function(el, x) {
            L.easyPrint({
              sizeModes: ['Current'],
              filename: 'impact-simulated policy-map',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
        )
      
    })
    
    
    
    # data table ----
    output$geo_pov_table <- renderReactable({
      req(geo_data())
      
      temp_data <- geo_data() %>% 
        select(admin_name, `Pre-reform`, `Current Policy`, Impact) %>% 
        rename("Admin name"=admin_name) %>% 
        arrange(Impact)
      
      reactable(temp_data,
                defaultExpanded = T,
                defaultPageSize = nrow(temp_data))
    })
    
    
    ######################################.
    # Downloads -------
    ######################################.
    
    # note these are both modules 
    
    # server for chart and data downloads
    download_chart_mod_server(id = "download_geo_pov_chart", chart_id = ns("geo_pov_chart"))
    
    download_data_btns_server(id = "download_geo_pov_data",
                              data = geo_data(),
                              file_name = "Geo_poverty_data_extract") # rename column
    
    # Render help content
    output$help_geospatial_pov_tab <- renderUI({
      help_geospatial_pov_tab
    })
    
    
  }) # close moduleServer
  
} # close server function



