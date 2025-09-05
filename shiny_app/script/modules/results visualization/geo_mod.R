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
geo_pov_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      full_screen = FALSE,
      height = "90%",
      # sidebar for filters ------------------
      sidebar = sidebar(
        width = 500,
        open = list(mobile = "always-above"),
        accordion(
          open = c("geo_pov_line_filter_panel"),
          multiple = TRUE,
          tags$h2(
            textOutput(outputId = ns("selected_parameter_label")),
            style = "color: #2c3e50; font-weight: bold;"
          ),
          accordion_panel(
            value = "geo_pov_line_filter_panel",
            div(
              id = ns("geo_pov_line_wrapper"),
              selectizeInput(
                ns("geo_pov_line"),
                label = "Poverty line",
                choices = c(
                  "National poverty line (454 MWK per day 2019/2020)" = "National poverty line (454 MWK per day)",
                  "Poverty line for lower income countries (2.15 USD or 656.7 MKW per day PPP)" = "Lower income class poverty line (656.7 MKW per day)",
                  "Poverty line for lower middle income countries (3.65 USD or 1115 MKW per day PPP)" = "Middle income class poverty line (1115 MKW per day)"
                ),
                selected = "National poverty line (454 MWK per day)"
              ),
              radioButtons(inputId = ns("geo_income"), label = "Income concept", choices = pov_geo_income_list, selected = "Disposable Income"),
              radioButtons(inputId = ns("geo_pov_parameter_filter"), label = "Parameter to show:", choices = setdiff(pov_parameter_list, "Welfare"), selected = "Rate of poverty"),
              radioButtons(inputId = ns("geo_pov_areas_filter"), label = "Areas:", choices = pov_geo_area_list),
              #  chart vs trend selector
              radioButtons(
                inputId = ns("chart_view_mode"),
                label   = "View as:",
                choices = c("Bar chart" = "bar", "Trend (line)" = "trend"),
                selected = "bar",
                inline  = TRUE
              )
            )
          )
        )
      ),
      
      layout_column_wrap(
        navset_card_pill(
          id = ns("geo_pov_navset_card_pill"),
          full_screen = TRUE,
          
          # charts tab
          nav_panel(
            "Charts",
            value = ns("geo_pov_chart_tab"),
            uiOutput(ns("geo_pov_title_chart")),
            uiOutput(ns("geo_pov_caveats")),
            highchartOutput(outputId = ns("geo_pov_chart")) %>%
              withSpinner() %>%
              bslib::as_fill_carrier()
          ),
          
          # maps tab
          nav_panel(
            "Maps",
            value = ns("geo_pov_map_wrapper"),
            uiOutput(ns("geo_pov_maps_caveats")),
            card(
              leafletOutput(ns("geo_pov_map")) %>%
                withSpinner() %>%
                bslib::as_fill_carrier()
            )
          ),
          
          # data tab
          nav_panel(
            "Data",
            value = ns("geo_pov_data_tab"),
            reactableOutput(ns("geo_pov_table"))
          ),
          
          # help tab
          nav_panel(
            title = "Help",
            uiOutput(ns("help_geospatial_pov_tab"))
          ),
          
          # footer
          footer = card_footer(
            class = "d-flex justify-content-left",
            div(id = ns("geo_pov_download_chart"), download_chart_mod_ui(ns("download_geo_pov_chart"))),
            div(id = ns("geo_pov_download_data"), download_data_btns_ui(ns("download_geo_pov_data")))
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
      
      if(input$geo_pov_parameter_filter=="Number of poor") {
        temp_data <- temp_data %>%
          mutate(across(where(is.numeric), ~ round(.x, 0)))
      }
      
      temp_data <- temp_data %>% 
        mutate(
          diff = (`Post-reform` - `Pre-reform`),
          impact = round((`Post-reform` - `Pre-reform`)*100/(`Pre-reform`),2))
      
      
      temp_data <- temp_data %>% drop_na()
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
        selected_parameter == "Number of poor" ~ "Number of Poor",
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
    
    output$selected_parameter_label <-  renderText({
      req(geo_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(geo_data()$Parameter)
      
      # Create a clearer title based on the parameter
      parameter_title <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor",
        selected_parameter == "Rate of poverty" ~ "Poverty Rate",
        selected_parameter == "Poverty gap" ~ "Poverty Gap",
        selected_parameter == "Poverty severity" ~ "Poverty Severity",
        TRUE ~ selected_parameter
      )
      
      # Get the selected parameter for better labeling
      selected_parameter <- parameter_title
      
      selected_parameter
    })
    
    
    # title
    output$geo_pov_title_chart <- renderUI({
      req(geo_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(geo_data()$Parameter)
      
      # Create a clearer title based on the parameter
      parameter_title <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor",
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
    
    
    geo_pov_title_map <- reactive({
      req(geo_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- first(geo_data()$Parameter)
      
      # Create a clearer title based on the parameter
      parameter_title <- case_when(
        selected_parameter == "Number of poor" ~ "Number of Poor",
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
      
      # Build an ordering key per admin
      ord_df <- geo_data() %>% 
        dplyr::transmute(
          admin_name,
          pre  = `Pre-reform`,
          post = `Post-reform`
        ) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(
          abs_change = post - pre,
          pct_change = ifelse(!is.na(pre) & pre != 0, 100 * (post - pre) / pre, NA_real_)
        )
      
      admin_order <- ord_df %>% 
        dplyr::arrange(dplyr::desc(pct_change)) %>% 
        dplyr::pull(admin_name)
      
      # factor order for x-axis
      #plot_data$admin_name <- factor(plot_data$admin_name)
      
      
      # Reshape to long format for plotting
      plot_data_long <- geo_data() %>%
        pivot_longer(cols = c("Post-reform", "Pre-reform"),
                     names_to = "Policy",
                     values_to = "Value")
      
      hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Post-reform" = "#006D77")
      
      plot_data_long <- plot_data_long %>%
        mutate(Policy = factor(Policy, levels = c("Pre-reform", "Post-reform")),
               admin_name = factor(admin_name, levels = admin_order),
               color = hc_colors_vec[as.character(Policy)]) %>% 
        dplyr::arrange(.data$Policy, .data$admin_name)
      

      # Decide which chart to draw
      if (identical(input$chart_view_mode, "trend")) {
        
        hc <-   highcharter::hchart(
          plot_data_long,
          type = "line",
          highcharter::hcaes(x = admin_name, y = Value, group = Policy)
        ) %>% 
          highcharter::hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>% 
          highcharter::hc_xAxis(
            title = list(text = "Administrative area"),
            categories = levels(plot_data_long$admin_name),
            labels = list(rotation = -35)
          ) %>% 
          highcharter::hc_yAxis(title = list(text = "")) %>% 
          highcharter::hc_plotOptions(series = list(marker = list(enabled = TRUE, radius = 3),
                                                    lineWidth = 3)) %>% 
          highcharter::hc_tooltip(shared = TRUE, valueDecimals = 2) 

        
      } else {
      # Plot using highcharter
      hc <- highcharter::hchart(
        plot_data_long,
        type = "column",
        hcaes(x = admin_name, y = Value, group = Policy)) %>% 
        hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>%
        hc_xAxis(title = list(text = "Administrative area")) %>% 
        hc_yAxis(title = list(text = "")) %>% 
        hc_plotOptions(column = list(groupPadding = 0.1)) %>% 
        hc_tooltip(shared = TRUE)%>%
        hc_caption(
          text = "Data source: Malawi Fifth Integrated Household Survey 2019-2020",
          style = list(fontSize = "8px", color = "black")
        ) }
      
      
      hc %>% 
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
    })
    
    
    # leaflet map -------
    
    # Global definition of value_palette
    value_palette <- reactive({
      req(map_data())

        colorNumeric(palette = "RdYlGn", domain = map_data()$impact)
    })
    
    
    
    output$geo_pov_map <- renderLeaflet({
      req(map_data(),geo_pov_title_map())
      
      leaflet(map_data(),
              options = leafletOptions(zoomControl = FALSE)) %>%# disable default zoom buttons
        # Set initial view to Malawi coordinates
        setView(lng = 34.0, lat = -13.5, zoom = 6) %>%
        # Change base map provider — e.g., Carto Light
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Title control FIRST
        addControl(
          html =  geo_pov_title_map(),
          position = "topleft"
        ) %>%
        addPolygons(data = map_data(), weight = 1, color = "black",
                    fillColor = ~value_palette()(impact),
                    fillOpacity = 0.5, 
                    smoothFactor = 0.5, 
                    opacity = 1, 
                    label = ~paste0(map_data()$admin_name, ": ", map_data()$impact),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
        addLegend(
          pal      = value_palette(),
          values   = ~impact,
          position = "topright",
          title    = HTML("Impact<br/>(% change)"),
          labFormat = labelFormat(
            digits = 1,
            suffix = "%"
          ),
          opacity  = 0.8
        ) %>%
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
        select(admin_name, `Pre-reform`, `Post-reform`, diff, impact) %>% 
        rename("Admin name"=admin_name,
               "Diff (Post - Pre) "= diff,
               "Impact (%)"  = impact,
               ) %>% 
        arrange(`Impact (%)`)
      
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



