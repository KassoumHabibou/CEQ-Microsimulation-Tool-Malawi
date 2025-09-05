###########################################################################.
# MODULE: incid_sub_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.


incid_sub_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 417,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        accordion(
                          open = c("incid_sub_line_filter_panel"), # guided tour panel closed by default
                          multiple = TRUE, # allow multiple panels to be open at once
                          tags$h2(
                            textOutput(outputId = ns("selected_parameter_label")),
                            style = "color: #2c3e50; font-weight: bold;"
                          ),
                          # accordion panel with indicator filter and definitions button
                          accordion_panel(
                            value = "incid_sub_line_filter_panel",
                            
                            div(id = ns("incid_sub_line_wrapper"), 
                                
                                #indicator filter (note this is a module)
                                selectizeInput(ns("incid_sub_nbr_dec"), 
                                               label = "Select the number of percentiles",
                                               choices = 2:10,
                                               selected = 5),
                                
                                radioButtons(inputId = ns("parameter_filter"), label = "Percentiles by: ", choices = pov_geo_income_list, selected =  "Market Income plus pensions"),
                                radioButtons(inputId = ns("incid_sub_type"), label = "Subsidy: ", choices = c("All Indirect Subsidy", "Electricity Subsidy", "Fuel Subsidy"), selected = "All Indirect Subsidy"),
                                radioButtons(inputId = ns("incid_sub_parameter"), label = "Parameter: ", choices = c("Share of total Subsidy (%)", "Subsidy as % of income","Average subsidy per household (MWK)","Total Subsidy (MWK)"), selected = "Share of total Subsidy (%)"),
                                #  chart vs trend selector
                                radioButtons(
                                  inputId = ns("chart_view_mode"),
                                  label   = "View as:",
                                  choices = c("Bar chart" = "bar", "Trend (line)" = "trend"),
                                  selected = "bar",
                                  inline  = TRUE
                                )
                            )
                          )) # close all accordion
      ), # close sidebar
      
      
      # create a multi-tab card
      div(id = ns("incid_sub_card_wrapper"),
          navset_card_pill(
            id = ns("incid_sub_navset_card_pill"),
            full_screen = TRUE,
            
            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("incid_sub_chart_tab"), #id for guided tour
                      uiOutput(ns("incid_sub_title")), # title
                      highchartOutput(outputId = ns("incid_sub_chart")) %>% # chart
                        withSpinner() %>% 
                        bslib::as_fill_carrier()
            ),
            
            # data tab ------------------
            nav_panel("Data",
                      value = ns("incid_sub_data_tab"), #id for guided tour
                      reactableOutput(ns("incid_sub_table")) # table
            ),
            # Interpretation tab 
            nav_panel(
              title = "Help",
              uiOutput(ns("help_incid_sub_chart_tab"))
            ),
            
            # footer with download buttons
            footer = card_footer(class = "d-flex justify-content-left",
                                 div(id = ns("incid_sub_download_chart"), download_chart_mod_ui(ns("download_incid_sub_chart"))),
                                 div(id = ns("incid_sub_download_data"), download_data_btns_ui(ns("download_incid_sub_data"))))
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


incid_sub_mod_server <- function(id, simulated_df, root_session) {
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # create reactive data - filtering by selected indicator
    incid_data <- reactive({
      req(simulated_df())
      
      temp_data <- simulated_df()
      # 1) Map income filter -> baseline income variable
      inc_map <- c(
        "Consumable Income"            = "yc_pc",
        "Disposable Income"            = "yd_pc",
        "Final Income"                 = "yf_pc",
        "Gross Income"                 = "yg_pc",
        "Market Income plus pensions"  = "yp_pc",
        "Net Market Income"            = "yn_pc"   # fallback/default
      )
      inc_var <- inc_map[[ input$parameter_filter ]] %||% "yc_pc"

      # 2) Map sub incidence type -> sub variable 
      sub_map <- c(
        "All Indirect Subsidy"    = "sub_all_hh",
        "Electricity Subsidy"      = "sub_electri_hh",
        "Fuel Subsidy" = "sub_fuel_hh"
      )
      sub_var <- sub_map[[ input$incid_sub_type ]] %||% "sub_all_hh"
      
      # 3) Attach chosen variables
      temp_data <- temp_data %>%
        left_join(
          bl_df %>%
            dplyr::select(hhid, pid, input_income = dplyr::all_of(inc_var),
                          sub_selected_pre = dplyr::all_of(sub_var))
          ,
          by = c("hhid","pid")
        ) %>%
        dplyr::mutate(
          sub_selected = .data[[sub_var]]
        )
      
      # 4) Weighted percentiles (creates a 'percentile' column using input_income and weight)
      temp_data <- make_weighted_percentiles(temp_data, "input_income", as.numeric(input$incid_sub_nbr_dec))
      
      # 5) Keep the essentials
      temp_data <- temp_data %>% 
        dplyr::select(hhid, weight, percentile, input_income, sub_selected, sub_selected_pre)
      
      
      # 6) Compute incidence by percentile
      incidence_by_percentile <- temp_data %>%
        dplyr::group_by(percentile) %>%
        dplyr::summarise(
          # Amount
          sub_wt    = sum(weight * sub_selected, na.rm = TRUE),
          sub_wt_pre    = sum(weight * sub_selected_pre, na.rm = TRUE),
          
          # Average
          sub_mean_wt    = weighted.mean(sub_selected,weight, na.rm = TRUE),
          sub_mean_wt_pre    = weighted.mean(sub_selected_pre,weight, na.rm = TRUE),
          
          income_wt = sum(input_income * weight, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        ungroup() %>% 
        dplyr::mutate(
          # Subsidy as % of income: weighted average sub rate within the percentile (in %)
          relative_incidence_pct = ifelse(income_wt > 0, 100 * sub_wt / income_wt, NA_real_),
          relative_incidence_pct_pre = ifelse(income_wt > 0, 100 * sub_wt_pre / income_wt, NA_real_),
          
          # Share of total Subsidy (%): share of total subes paid by this percentile (in %)
          absolute_incidence_pct = 100 * sub_wt / sum(sub_wt, na.rm = TRUE),
          absolute_incidence_pct_pre = 100 * sub_wt_pre / sum(sub_wt_pre, na.rm = TRUE),
          
          # Level (currency units): weighted sub amount in the percentile
          level_sub = sub_wt,
          level_sub_pre = sub_wt_pre
        ) %>%
        dplyr::arrange(percentile)
      
      # Round for display
      incidence_by_percentile <- incidence_by_percentile %>%
        dplyr::mutate(
          # Post-reform
          relative_incidence_pct = round(relative_incidence_pct, 2),
          absolute_incidence_pct = round(absolute_incidence_pct, 2),
          level_sub              = round(level_sub),
          sub_mean_wt = round(sub_mean_wt),
          # Pre-reform
          relative_incidence_pct_pre = round(relative_incidence_pct_pre, 2),
          absolute_incidence_pct_pre = round(absolute_incidence_pct_pre, 2),
          level_sub_pre              = round(level_sub_pre),
          sub_mean_wt_pre = round(sub_mean_wt_pre)
        )
      
      
      incidence_by_percentile
    })
    
    
    
    
    #######################################################.
    ## Dynamic text  ----
    #######################################################.
    
    output$incid_sub_title <- renderUI({
      req(incid_data())
      
      
      # display titles with improved clarity
      div(
        tags$h5(paste0("Incidence Indicator: ", input$incid_sub_parameter), class = "chart-header"), # selected Parameter with clearer label
        tags$h6(paste0("Subsidy: ", input$incid_sub_type)), # selected Area
        tags$h6(paste0("Percentile Parameter: ", input$parameter_filter)) # percentile parameter
      )
      
    })
    
    output$selected_parameter_label <-  renderText({
      req(incid_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- input$incid_sub_parameter
      
      selected_parameter
    })
    
    #############################################.
    # Charts/tables ----
    #############################################.
    
    # Poverty chart
    output$incid_sub_chart <- renderHighchart({
      req(incid_data())
      
      # --- pick which column to show based on the radioButtons selection ----
      param_col <- dplyr::case_when(
        input$incid_sub_parameter == "Subsidy as % of income" ~ "relative_incidence_pct",
        input$incid_sub_parameter == "Total Subsidy (MWK)"        ~ "level_sub",
        input$incid_sub_parameter == "Average subsidy per household (MWK)" ~ "sub_mean_wt",
        TRUE  ~ "absolute_incidence_pct"   # "Share of total Subsidy (%)"
      )
      
      # Create a clearer chart title based on the parameter
      chart_title <- input$incid_sub_parameter
      
      plot_data <- incid_data()  
      
    
      plot_data <- plot_data %>%
        dplyr::mutate(
          Value  = .data[[param_col]],
          Value_pre = .data[[paste0(param_col,'_pre')]]
        ) %>% 
        select(percentile, Value, Value_pre)
      
      # Decide which chart to draw
      if (identical(input$chart_view_mode, "trend")) {
        
        hc <- create_incid_trend_chart(plot_data)
        
      } else {
        hc <- create_incid_bar_chart(plot_data)
      }
      
      
      hc %>% 
        hc_exporting(
          filename = paste0("Incidence - ", input$incid_sub_parameter, " - ",
                            input$incid_sub_type, " - ",
                            input$parameter_filter),
          chartOptions = list(
            title = list(text = chart_title),
            subtitle = list(
              text = paste0(
                "Subsidy: ", input$incid_sub_type,
                "<br>",
                "Percentile Parameter: ",  input$parameter_filter
              ),
              useHTML = TRUE  
            ))
        )
      
    })
    
    
    # data table
    output$incid_sub_table <- renderReactable({
      req(incid_data())
      
      # --- pick which column to show based on the radioButtons selection ----
      param_col <- dplyr::case_when(
        input$incid_sub_parameter == "Subsidy as % of income" ~ "relative_incidence_pct",
        input$incid_sub_parameter == "Share of total Subsidy (%)" ~ "absolute_incidence_pct",
        input$incid_sub_parameter == "Total Subsidy (MWK)" ~ "level_sub",
        input$incid_sub_parameter == "Average subsidy per household (MWK)"  ~ "sub_mean_wt",
        TRUE ~ NA    # "Share of total Subsidy (%)"
      )
      
      # Get the selected parameter for better column labeling
      selected_parameter <- input$parameter_filter
      
      # Create clearer column names based on the parameter
      value_col_name <- case_when(
        input$incid_sub_parameter == "Share of total Subsidy (%)" ~ "Share of total Subsidy (%)",
        input$incid_sub_parameter == "Subsidy as % of income" ~ "Subsidy as % of income",
        input$incid_sub_parameter == "Average subsidy per household (MWK)"~ "Average subsidy per household (MWK)",
        input$incid_sub_parameter == "Total Subsidy (MWK)" ~ "Total Subsidy (MWK)",
        TRUE  ~ NA
      )
      
      
      df_table <- incid_data() %>%
        dplyr::mutate(
          Value  = .data[[param_col]],
          Value_pre = .data[[paste0(param_col,'_pre')]]
        ) %>% 
        select(percentile, Value, Value_pre)
      
      
      df_table <- df_table %>% 
        mutate(diff = round(Value - Value_pre,2)) %>% 
        mutate(Prop = round(diff*100/Value_pre,2))
      
      
      # Use the filtered data directly
      reactable(df_table,
                columns = list(
                  percentile = colDef(name = "Percentile"),
                  Value_pre  = colDef(name = paste0("Pre-reform ", value_col_name)),
                  Value = colDef(name = paste0("Post-reform ", value_col_name)),
                  diff = colDef(name = "Diff (Post - Pre)"),
                  Prop = colDef(name = "Impact (%)")
                )
      )
      
    })
    
    
    ###################################.
    # Downloads ----
    ###################################.
    
    # server for chart and data downloads
    download_chart_mod_server(id = "download_incid_sub_chart", chart_id = ns("incid_sub_chart"))
    
    download_data_btns_server(id = "download_incid_sub_data",
                              data = incid_data(),
                              file_name = paste0(input$incid_sub_parameter,"_incidence_data_extract")) # rename column
    
    # Render help content
    output$help_incid_sub_chart_tab <- renderUI({
      help_incid_sub_chart_tab
    })
    
    
    
  }) # close moduleServer
} # close server function


