###########################################################################.
# MODULE: incid_itx_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.


incid_itx_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 350,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        accordion(
                          open = c("incid_itx_line_filter_panel"), # guided tour panel closed by default
                          multiple = TRUE, # allow multiple panels to be open at once
                          tags$h2(
                            textOutput(outputId = ns("selected_parameter_label")),
                            style = "color: #2c3e50; font-weight: bold;"
                          ),
                          # accordion panel with indicator filter and definitions button
                          accordion_panel(
                            value = "incid_itx_line_filter_panel",
                            
                            div(id = ns("incid_itx_line_wrapper"), 
                                
                                #indicator filter (note this is a module)
                                selectizeInput(ns("incid_itx_nbr_dec"), 
                                               label = "Select the number of deciles",
                                               choices = 2:10,
                                               selected = 5),

                                radioButtons(inputId = ns("parameter_filter"), label = "Percentiles by: ", choices = pov_geo_income_list, selected = "Consumable Income"),
                                radioButtons(inputId = ns("incid_itx_type"), label = "Taxes: ", choices = c("All Indirect Taxes", "VAT Tax","Excise Tax"), selected = "All Indirect Taxes"),
                                radioButtons(inputId = ns("incid_itx_parameter"), label = "Parameter: ", choices = c("Absolute incidence", "Relative incidence","Total level"), selected = "Absolute incidence")
                                
                            )
                          )) # close all accordion
      ), # close sidebar
      
      
      # create a multi-tab card
      div(id = ns("incid_itx_card_wrapper"),
          navset_card_pill(
            id = ns("incid_itx_navset_card_pill"),
            full_screen = TRUE,
            
            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("incid_itx_chart_tab"), #id for guided tour
                      uiOutput(ns("incid_itx_title")), # title
                      highchartOutput(outputId = ns("incid_itx_chart")) %>% # chart
                        withSpinner() %>% 
                        bslib::as_fill_carrier()
            ),
            
            # data tab ------------------
            nav_panel("Data",
                      value = ns("incid_itx_data_tab"), #id for guided tour
                      reactableOutput(ns("incid_itx_table")) # table
            ),
            # Interpretation tab 
            nav_panel(
              title = "Help",
              uiOutput(ns("help_incid_itx_chart_tab"))
            ),
            
            # footer with download buttons
            footer = card_footer(class = "d-flex justify-content-left",
                                 div(id = ns("incid_itx_download_chart"), download_chart_mod_ui(ns("download_incid_itx_chart"))),
                                 div(id = ns("incid_itx_download_data"), download_data_btns_ui(ns("download_incid_itx_data"))))
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


incid_itx_mod_server <- function(id, simulated_df, root_session) {
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
      
      # 2) Map tax incidence type -> tax variable
      tax_map <- c(
        "All Indirect Taxes"    = "itx_all_hh",
        "VAT Tax"      = "itx_vatx_hh",
        "Excise Tax" = "itx_excx_hh"
      )
      tax_var <- tax_map[[ input$incid_itx_type ]] %||% "itx_all_hh"
      
      # 3) Attach chosen variables
      temp_data <- temp_data %>%
        left_join(
          bl_df %>%
            dplyr::select(hhid, pid, input_income = dplyr::all_of(inc_var),
                          tax_selected_pre = dplyr::all_of(tax_var))
          ,
          by = c("hhid","pid")
        ) %>%
        dplyr::mutate(
          tax_selected = .data[[tax_var]]
        )
      
      # 4) Weighted deciles (creates a 'decile' column using input_income and weight)
      temp_data <- make_weighted_deciles(temp_data, "input_income", as.numeric(input$incid_itx_nbr_dec))
      
      # 5) Keep the essentials
      temp_data <- temp_data %>% 
        dplyr::select(hhid, weight, decile, input_income, tax_selected, tax_selected_pre)
      
      
      # 6) Compute incidence by decile
      incidence_by_decile <- temp_data %>%
        dplyr::group_by(decile) %>%
        dplyr::summarise(
          tax_wt    = sum(weight * tax_selected, na.rm = TRUE),
          tax_wt_pre    = sum(weight * tax_selected_pre, na.rm = TRUE),
          income_wt = sum(weight * input_income,  na.rm = TRUE),
          .groups = "drop"
        ) %>%
        ungroup() %>% 
        dplyr::mutate(
          # Relative incidence: weighted average tax rate within the decile (in %)
          relative_incidence_pct = ifelse(income_wt > 0, 100 * tax_wt / income_wt, NA_real_),
          relative_incidence_pct_pre = ifelse(income_wt > 0, 100 * tax_wt_pre / income_wt, NA_real_),
          
          # Absolute incidence: share of total taxes paid by this decile (in %)
          absolute_incidence_pct = 100 * tax_wt / sum(tax_wt, na.rm = TRUE),
          absolute_incidence_pct_pre = 100 * tax_wt_pre / sum(tax_wt_pre, na.rm = TRUE),
          
          # Level (currency units): weighted tax amount in the decile
          level_tax = tax_wt,
          level_tax_pre = tax_wt_pre
        ) %>%
        dplyr::arrange(decile)
      
      # Round for display
      incidence_by_decile <- incidence_by_decile %>%
        dplyr::mutate(
          # Post-reform
          relative_incidence_pct = round(relative_incidence_pct, 2),
          absolute_incidence_pct = round(absolute_incidence_pct, 2),
          level_tax              = round(level_tax),
          # Pre-reform
          relative_incidence_pct_pre = round(relative_incidence_pct_pre, 2),
          absolute_incidence_pct_pre = round(absolute_incidence_pct_pre, 2),
          level_tax_pre              = round(level_tax_pre)
        )
      
      
      
      
      incidence_by_decile
    })
    
    
    
    
    #######################################################.
    ## Dynamic text  ----
    #######################################################.
    
    output$incid_itx_title <- renderUI({
      req(incid_data())
      
      
      # display titles with improved clarity
      div(
        tags$h5(paste0("Incidence Indicator: ", input$incid_itx_parameter), class = "chart-header"), # selected Parameter with clearer label
        tags$h6(paste0("Taxes: ", input$incid_itx_type)), # selected Area
        tags$h6(paste0("Decile parameter: ", input$parameter_filter)) # selected Poverty line
      )
      
    })
    
    output$selected_parameter_label <-  renderText({
      req(incid_data())
      
      # Get the selected parameter for better labeling
      selected_parameter <- input$incid_itx_parameter
      
      selected_parameter
    })
    #############################################.
    # Charts/tables ----
    #############################################.
    
    # Poverty chart
    output$incid_itx_chart <- renderHighchart({
      req(incid_data())
      
      # --- pick which column to show based on the radioButtons selection ----
      param_col <- dplyr::case_when(
        input$incid_itx_parameter == "Relative incidence" ~ "relative_incidence_pct",
        input$incid_itx_parameter == "Total level"        ~ "level_tax",
        TRUE                                              ~ "absolute_incidence_pct"   # "Absolute incidence"
      )
      
      # Create a clearer chart title based on the parameter
      chart_title <- input$incid_itx_parameter
      
      plot_data <- incid_data()  
      
      
      plot_data <- plot_data %>%
        dplyr::mutate(
          Value  = .data[[param_col]],
          Value_pre = .data[[paste0(param_col,'_pre')]]
        ) %>% 
        select(decile, Value, Value_pre)
      
      create_incid_bar_chart(plot_data) %>% 
        hc_exporting(
          filename = paste0("Incidence - ", input$incid_itx_parameter, " - ",
                            input$incid_itx_type, " - ",
                            input$parameter_filter),
          chartOptions = list(
            title = list(text = chart_title),
            subtitle = list(
              text = paste0(
                "Taxes: ", input$incid_itx_type,
                "<br>",
                "Decile parameter: ",  input$parameter_filter
              ),
              useHTML = TRUE  
            ))
        )
      
    })
    
    
    # data table
    output$incid_itx_table <- renderReactable({
      req(incid_data())
      
      # --- pick which column to show based on the radioButtons selection ----
      param_col <- dplyr::case_when(
        input$incid_itx_parameter == "Relative incidence" ~ "relative_incidence_pct",
        input$incid_itx_parameter == "Total level"        ~ "level_tax",
        TRUE                                              ~ "absolute_incidence_pct"   # "Absolute incidence"
      )
      
      # Get the selected parameter for better column labeling
      selected_parameter <- input$parameter_filter
      
      # Create clearer column names based on the parameter
      value_col_name <- case_when(
        input$incid_itx_parameter == "Absolute incidence" ~ "Absolute incidence (%)",
        input$incid_itx_parameter == "Relative incidence" ~ "Relative incidence (%)",
        input$incid_itx_parameter == "Total level" ~ "Total level (in MWK)"
      )
      
      
      df_table <- incid_data() %>%
        dplyr::mutate(
          Value  = .data[[param_col]],
          Value_pre = .data[[paste0(param_col,'_pre')]]
        ) %>% 
        select(decile, Value, Value_pre)
      
      
      df_table <- df_table %>% 
        mutate(diff = Value - Value_pre) %>% 
        mutate(Prop = diff*100/Value_pre)
      
      
      # Use the filtered data directly
      reactable(df_table,
                columns = list(
                  decile = colDef(name = "Decile"),
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
    download_chart_mod_server(id = "download_incid_itx_chart", chart_id = ns("incid_itx_chart"))
    
    download_data_btns_server(id = "download_incid_itx_data",
                              data = incid_data(),
                              file_name = paste0(input$incid_itx_parameter,"_incidence_data_extract")) # rename column
    
    # Render help content
    output$help_incid_itx_chart_tab <- renderUI({
      help_incid_itx_chart_tab
    })
    
    
    
  }) # close moduleServer
} # close server function


