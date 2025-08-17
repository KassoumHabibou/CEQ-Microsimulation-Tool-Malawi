
###############################################################################.
# MODULE: summary_table_mod ---- 
###############################################################################.

# prepares summary data for each profile and creates a table containing the latest data for each indicator in a profile, for the chosen geography
# within this table, there is a spine chart rendered one each row
# note for the charts to work you need to add a link to the highchart js library in the UI script (highcharts is not free - ScotPHO have a licence for this)
# module also includes download options:
# option to download as pdf (requires a separate rmarkdown file to re-create the summary table)
# option to download data in various formats using another module which is nested in this module (see download_data_mod.R)

###############################################################################.
# UI function ----
###############################################################################.

sum_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Main summary card -------------------------------------------------------
    card(
      full_screen = FALSE,
      fill = TRUE,
      card_header(
        bs_icon("zoom-in", size = "3em"),
        span("Main Results of the Policy Change",
             style = "font-size: 2em; font-weight: bold; vertical-align: middle; margin-left: 10px;"),
        class = "info-box-header"
      ),
      
      # Key Poverty & Inequality Metrics ----
      h2("Poverty and Inequality", class = "fw-bold text-primary"),
      
      # Layout: LEFT = cards, RIGHT = map
      fluidRow(
        style = "display: flex; align-items: stretch;",  # makes both columns same height
        # LEFT: poverty indicators (2 per row)
        column(
          width = 6,
          fluidRow(
            column(6, uiOutput(ns("card_pov_rate"))),
            column(6, uiOutput(ns("card_pov_gap")))
          ),
          fluidRow(
            column(6, uiOutput(ns("card_pov_nbr"))),
            column(6, uiOutput(ns("card_gini")))
          )
        ),
        # RIGHT: poverty map in a card
        column(
          width = 6,
          bslib::card(
            full_screen = TRUE,
            leafletOutput(ns("geo_pov_map_sum"), height = "400") %>%
              withSpinner() %>%
              bslib::as_fill_carrier()
          )
        )
      ),
      
      h2("Governement Revenues", class = "fw-bold text-primary"),
      fluidRow(
        title = "Government revenue",
        column(3, uiOutput(ns("card_direct_tax"))),
        column(3, uiOutput(ns("card_indirect_tax"))),
        column(3, uiOutput(ns("card_direct_transfert"))),
        column(3, uiOutput(ns("card_indirect_subsidies")))
      ),
      
      tags$hr(),
      
      h2("Policy Parameters Changes", class = "fw-bold text-primary"),

      ## Section 1: Direct Tax
      h3("Direct Taxes", class = "fw-bold"),
      bslib::card(
        bslib::card_header("Pay As You Earn (PAYE) – Personal Income Tax"),
        card_body(reactableOutput(ns("paye_table")))
      ),
      bslib::card(
        bslib::card_header("Corporate Income Tax – By Industry"),
        card_body(reactableOutput(ns("cor_table")))
      ),
      
      tags$hr(style = "margin-top: 2em; margin-bottom: 1em;"),
      
      ## Section 2: Indirect Tax
      h3("Indirect Taxes", class = "fw-bold"),
      bslib::card(
        bslib::card_header("Value Added Tax (VAT) – By Product"),
        card_body(reactableOutput(ns("vat_table")))
      ),
      bslib::card(
        bslib::card_header("Excise Duties – By Product"),
        card_body(reactableOutput(ns("excise_table")))
      )
    ),
    
    # World Bank logo and About section --------------------------------------
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



sum_mod_server <- function(id, simulated_pov, simulated_geo, simulated_ineq, simulated_revmob, 
                           paye_value, corp_value,  vat_value, excise_value, session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # PAYE Logic
    paye_names <- c("Less than 1.8", "More than 1.8 and less than 6", "More than 6 and less than 30.6", "More than 30.6")
    paye_pre <- c(0, 25, 30, 35)

    
    # Corporate Logic
    # Ordered by industry ID (0-18)
    corp_names <- c(
                    "Agriculture",                                # ID 1  
                    "Mining",                                     # ID 2
                    "Manufacture",                                # ID 3
                    "Electricity",                                # ID 4
                    "Construction",                               # ID 5
                    "Retail",                                     # ID 6
                    "Transport",                                  # ID 7
                    "Hospitality",                                # ID 8
                    "Communication",                              # ID 9
                    "Financial",                                  # ID 10
                    "Real Estate",                                # ID 11
                    "Professional",                               # ID 12
                    "Administrative and Support",                 # ID 13
                    "Public Admin",                               # ID 14
                    "Education",                                  # ID 15
                    "Health",                                     # ID 16
                    "Arts",                                       # ID 17
                    "Other Services")                             # ID 18
    
    corp_pre <- c(
                  0,    # Agriculture  
                  30,   # Mining
                  15,   # Manufacture
                  0,    # Electricity
                  15,   # Construction
                  15,   # Retail
                  15,   # Transport
                  15,   # Hospitality
                  15,   # Communication
                  15,   # Financial
                  15,   # Real Estate
                  15,   # Professional
                  15,   # Administrative and Support
                  15,   # Public Admin
                  15,   # Education
                  15,   # Health
                  15,   # Arts
                  15)   # Other Services
    

    # VAT logic
    vat_name <- c("VAT Rate")
    vat_pre <- c(16.5)
    
    # Excise
    excise_names <- c(
      "Wine or commercial liquor",                               # 914 (250%)
      "Bottled / canned beer (Carlsberg, etc)",                 # 911 (250%)
      "Traditional beer (masese)",                               # 913 (40%)
      "Cooking utensils (cookpots, stirring spoons and whisks, etc.)", # 330 (20%)
      "Radio with flash drive/micro CD",                         # 5801 (20%)
      "Diesel",                                                  # 211 (10%)
      "Tomato sauce (bottle)",                                   # 813 (10%)
      "Umbrella",                                                # 333 (10%)
      "Bottled water",                                           # 909 (5%)
      "Mini-bus",                                                # 519 (5%)
      "Motorcycle/scooter",                                      # 517 (5%)
      "Tractor",                                                 # 611 (5%)
      
      # Items with zero excise rates
      "Pork",                                                    # 506 (0%)
      "Mutton",                                                  # 507 (0%)
      "Rice",                                                    # 106 (0%)
      "Bread",                                                   # 111 (0%)
      "Maize ufa mgaiwa (normal flour)",                         # 101 (0%)
      "Sugar",                                                   # 801 (0%)
      "Cooking oil",                                             # 803 (0%)
      "Salt"                                                     # 810 (0%)
    )
    
    # Excise pre-set values vector (rates as percentages, matching CSV data)
    excise_pre <- c(
      250,  # 914 - Wine or commercial liquor
      250,  # 911 - Bottled / canned beer
      40,   # 913 - Traditional beer
      20,   # 330 - Cooking utensils
      20,   # 5801 - Radio with flash drive/micro CD
      10,   # 211 - Diesel
      10,   # 813 - Tomato sauce (bottle)
      10,   # 333 - Umbrella
      5,    # 909 - Bottled water
      5,    # 519 - Mini-bus
      5,    # 517 - Motorcycle/scooter
      5,    # 611 - Tractor
      
      # Items with zero excise rates
      0,    # 506 - Pork
      0,    # 507 - Mutton
      0,    # 106 - Rice
      0,    # 111 - Bread
      0,    # 101 - Maize ufa mgaiwa (normal flour)
      0,    # 801 - Sugar
      0,    # 803 - Cooking oil
      0     # 810 - Salt
    )
    
    
    
    # create reactive data - filtering by selected indicator
    filtered_pov_data <- reactive({
      req(simulated_pov())
      
      
      temp_data <- simulated_pov() %>%
        filter(Area == "Country") %>% 
        filter(`Poverty line` == "National poverty line (454 MWK per day)") %>% 
        filter(Income == "Disposable Income")
      
      
      temp_data
    })
    
    filtered_ineq_data <- reactive({
      req(simulated_ineq())
      
      temp_data <- simulated_ineq() %>%
        filter(Area == "Country") %>% 
        filter(Income == "Disposable Income") %>% 
        filter(Parameter == "Gini index")
      
    })
    
    
    # prepares data to be plotted --------------------------------------------
    map_data_sum <- reactive({
      req(simulated_geo())
      
      temp_data <- simulated_geo() %>% 
        filter(Parameter == "Rate of poverty")  %>% 
        filter(Area == "district") %>% 
        filter(`Poverty line` == "National poverty line (454 MWK per day)") %>% 
        filter(Income == "Disposable Income") %>% 
        select(Code_area, admin_name, `Post-reform`)
      

      temp_data <- mlw_bound_district %>%  left_join(temp_data)
      
      temp_data
    })
    
    # Global definition of value_palette
    value_palette <- reactive({
      req(map_data_sum())
      colorNumeric(palette = rev(brewer.pal(n = 11, name = "RdYlGn")),
                   domain = map_data_sum()$`Post-reform`)
    })
    

    output$geo_pov_map_sum <- renderLeaflet({
      req(map_data_sum())

      leaflet(map_data_sum(),
              options = leafletOptions(zoomControl = FALSE) ) %>%# disable default zoom buttons) %>% 
        setView(lng = 34.0, lat = -13.5, zoom = 6) %>%
        
        # Change base map provider — e.g., Carto Light
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        # Title control FIRST
        addControl(
          html =  div(
            tags$h5(paste0("Poverty Indicator: ", "headcount ratio in %"), class = "chart-header"), # selected Parameter with clearer label
            tags$h6(paste0("Area: ", "district")), # selected 
            tags$h6(paste0("Poverty line: ","national poverty line (454 MWK)")) # selected 
          ),
          position = "topleft"
        ) %>%
        addPolygons(
          data = map_data_sum(),
          weight = 1, color = "black",
          fillColor = ~value_palette()(`Post-reform`),
          fillOpacity = 0.5,
          smoothFactor = 0.5,
          opacity = 1,
          label = ~paste0(admin_name, ": ", `Post-reform`),
          highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
        ) %>%
        addLegend(pal = value_palette(), values = ~`Post-reform`) %>%
        onRender(
          "function(el, x) {
        L.easyPrint({
          sizeModes: ['Current'],
          filename: 'Poverty rates for post reform',
          exportOnly: true,
          hideControlContainer: false
        }).addTo(this);
      }"
        )
    })
    
    output$card_pov_rate <- renderUI({
      req(filtered_pov_data())
      
      
      pov_rate_data <- filtered_pov_data() %>%
        filter(Parameter == "Rate of poverty")
      
      renderCard(pov_rate_data, "Poverty Rate", "person-fill-exclamation", is_inverse = FALSE)
    })
    
    output$card_pov_gap <- renderUI({
      req(filtered_pov_data())
      
      pov_gap_data <- filtered_pov_data() %>%
        filter(Parameter == "Poverty gap")
      
      
      renderCard(pov_gap_data, "Poverty Gap", "exclamation-triangle-fill", is_inverse = FALSE)
    })
    
    output$card_pov_nbr <- renderUI({
      req(filtered_pov_data())
      
      pov_nbr_data <- filtered_pov_data() %>%
        filter(Parameter == "Number of poor")
      
      
      renderCard(pov_nbr_data, "Number of Poor", "people-fill", is_inverse = FALSE)
    })
    
    
    output$card_gini <- renderUI({
      req(filtered_ineq_data())
      ineq_data <- filtered_ineq_data() %>%
        filter(Parameter == "Gini index")
      
      renderCard(ineq_data, "Gini Index", "bar-chart-fill", is_inverse = FALSE)
    })
    
    
    
    ### Revenue 
    output$card_direct_tax <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Direct taxes") %>% 
        mutate(`Post-reform` = `Post-reform`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Direct taxes", "bank2", is_inverse = TRUE)
    })
    
    output$card_indirect_tax <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Indirect taxes") %>% 
        mutate(`Post-reform` = `Post-reform`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Indirect taxes", "cart-check", is_inverse = TRUE)
    })
    
    output$card_direct_transfert <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Direct transfers") %>% 
        mutate(`Post-reform` = `Post-reform`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Direct transfers", "cash-stack", is_inverse = TRUE)
    })
    
    output$card_indirect_subsidies <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Indirect subsidies") %>% 
        mutate(`Post-reform` = `Post-reform`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Indirect subsidies", "droplet-half", is_inverse = TRUE)
    })
    
    # Summary cards for each indicator
    # ---- Bright traffic-light colors + bold/dark arrows ----
    renderCard <- function(data, title, icon, is_inverse = FALSE) {
      
      if (nrow(data) == 0) {
        return(div(class = "alert alert-warning", paste("No data available for", title)))
      }
      if (!all(c("Post-reform","Pre-reform") %in% colnames(data))) {
        return(div(class = "alert alert-danger",
                   paste("Data structure error for", title, ". Available columns:",
                         paste(colnames(data), collapse = ", "))))
      }
      
      val  <- round(data$`Post-reform`, 2)
      base <- round(data$`Pre-reform`, 2)
      diff <- ifelse(ceiling(val - base) == 0, 0, round(val - base, 0))
      prop <- round(ifelse(base != 0, (diff / base) * 100, NA), 1)
      
      # Arrow icon (bold/dark)
      arrow_icon <- if (diff > 0) "arrow-up" else if (diff < 0) "arrow-down" else "dash-circle"
      arrow_tag  <- tags$span(bs_icon(arrow_icon, size = "1.6em"), class = "fw-bold text-dark")
      
      # Status logic (improve vs worse vs no change), then map to bright colors
      worse     <- (diff > 0 & !is_inverse) | (diff < 0 & is_inverse)
      improve   <- (diff < 0 & !is_inverse) | (diff > 0 & is_inverse)
      nochange  <- diff == 0
      
      bg_col <- if (worse) "#ff3b30" else if (improve) "#34c759" else "#ffd60a"  # red / green / yellow
      txtcol <- "#111"
      
      div(
        class = "card",
        style = paste0("border:1px solid #ddd;border-radius:8px;padding:15px;margin:10px 0;",
                       "background-color:", bg_col, ";color:", txtcol, ";"),
        div(
          style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
          span(
            tagList(
              bs_icon(icon, size = "1.5em"),
              span(title, style = "font-size:1.2em;margin-left:10px;font-weight:bold;")
            )
          ),
          arrow_tag
        ),
        div(
          style = "text-align:center;",
          h3(
            paste0(val, " (", ifelse(is.na(prop), "—", paste0(ifelse(diff >= 0, "+", ""), prop, "%")), ")"),
            style = "margin:10px 0;font-size:1.5em;"
          ),
          p(
            if (nochange) paste0("No change (Pre-reform ", base, ")")
            else if (worse) paste0("Decrease (Pre-reform ", base, ")")
            else paste0("Increase (Pre-reform ", base, ")"),
            style = "margin:5px 0;font-size:0.9em;"
          )
        )
      )
    }
    
    # Summary cards for each indicator
    renderCard_rev <- function(data, title, icon, is_inverse = FALSE) {
      
      if (nrow(data) == 0) {
        return(div(class = "alert alert-warning", paste("No data available for", title)))
      }
      if (!all(c("Post-reform","Pre-reform") %in% colnames(data))) {
        return(div(class = "alert alert-danger",
                   paste("Data structure error for", title, ". Available columns:",
                         paste(colnames(data), collapse = ", "))))
      }
      
      val  <- round(data$`Post-reform`, 2)
      base <- round(data$`Pre-reform`, 2)
      diff <- ifelse(ceiling(val - base) == 0, 0, round(val - base, 0))
      prop <- round(ifelse(base != 0, (diff / base) * 100, NA), 1)
      
      arrow_icon <- if (diff > 0) "arrow-up" else if (diff < 0) "arrow-down" else "dash-circle"
      arrow_tag  <- tags$span(bs_icon(arrow_icon, size = "1.6em"), class = "fw-bold text-dark")
      
      worse     <- (diff > 0 & !is_inverse) | (diff < 0 & is_inverse)
      improve   <- (diff < 0 & !is_inverse) | (diff > 0 & is_inverse)
      nochange  <- diff == 0
      
      bg_col <- if (worse) "#ff3b30" else if (improve) "#34c759" else "#ffd60a"
      txtcol <- "#111"
      
      div(
        class = "card",
        style = paste0("border:1px solid #ddd;border-radius:8px;padding:15px;margin:10px 0;",
                       "background-color:", bg_col, ";color:", txtcol, ";"),
        div(
          style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
          span(
            tagList(
              bs_icon(icon, size = "1.5em"),
              span(title, style = "font-size:1.2em;margin-left:10px;font-weight:bold;")
            )
          ),
          arrow_tag
        ),
        div(
          style = "text-align:center;",
          h3(
            paste0(round(val, 0), " Billion MWK (",
                   ifelse(is.na(prop), "—", paste0(ifelse(diff >= 0, "+", ""), round(prop, 2), "%")), ")"),
            style = "margin:10px 0;font-size:1.5em;"
          ),
          p(
            if (nochange) paste0("No change (Pre-reform ", round(base, 0), " Billion MWK)")
            else if (worse) paste0("Decrease (Pre-reform ", round(base, 0), " Billion MWK)")
            else paste0("Increase (Pre-reform ", round(base, 0), " Billion MWK)"),
            style = "margin:5px 0;font-size:0.9em;"
          )
        )
      )
    }
    
    # PAYE summary table
    output$paye_table <- renderReactable({
      req(paye_value())
      
      
      df <- data.frame(
        name_paye = paye_names,
        pre = paye_pre,
        current = paye_value(),
        stringsAsFactors = FALSE
      ) %>% 
        mutate(Diff = round(pre - current),
               Prop = round(ifelse(pre!=0, (pre - current)*100/pre, NA))) 
      
      reactable(df, 
                defaultPageSize = 15,
                highlight = TRUE,
                bordered = TRUE,
                columns = list(
                  name_paye = colDef(name = "Industry"),
                  pre = colDef(name = "Pre-Reform"),
                  current = colDef(name = "Post-reform"),
                  Diff = colDef(name = "Difference"),
                  Prop = colDef(name = "Difference in (%) from Pre-Reform")
                ))
      
    })
      
    # Corporate summary table
    output$cor_table <- renderReactable({
      req(corp_value())
      
      
      df <- data.frame(
        name_corp = corp_names,
        pre = corp_pre,
        current = corp_value(),
        stringsAsFactors = FALSE
      ) %>% 
        mutate(Diff = round(pre - current),
               Prop = round(ifelse(pre!=0, (pre - current)*100/pre, NA))) 
      
      reactable(df, 
                defaultPageSize = 15,
                highlight = TRUE,
                bordered = TRUE,
                columns = list(
                  name_corp = colDef(name = "Industry"),
                  pre = colDef(name = "Pre-Reform"),
                  current = colDef(name = "Post-reform"),
                  Diff = colDef(name = "Difference"),
                  Prop = colDef(name = "Difference in (%) from Pre-Reform")
                ))
      
    })
    
        
      output$excise_table <- renderReactable({
          req(excise_value())
          
        
          df <- data.frame(
            name_excise = excise_names,
            pre = excise_pre,
            current = excise_value(),
            stringsAsFactors = FALSE
          ) %>% 
            mutate(Diff = round(pre - current),
                   Prop = round(ifelse(pre!=0, (pre - current)*100/pre, NA))) 
          
          reactable(df, 
                    defaultPageSize = 15,
                    highlight = TRUE,
                    bordered = TRUE,
                    columns = list(
                      name_excise = colDef(name = "Item"),
                      pre = colDef(name = "Pre-Reform"),
                      current = colDef(name = "Post-reform"),
                      Diff = colDef(name = "Difference"),
                      Prop = colDef(name = "Difference in (%) from Pre-Reform")
                    ))

      })
    
   
      
      # Corporate summary table
      output$vat_table <- renderReactable({
        req(vat_value())
        
        
        df <- data.frame(
          name_vat = vat_name,
          pre = vat_pre,
          current = vat_value(),
          stringsAsFactors = FALSE
        ) %>% 
          mutate(Diff = round(pre - current),
                 Prop = round(ifelse(pre!=0, (pre - current)*100/pre, NA))) 
        
        reactable(df, 
                  defaultPageSize = 15,
                  highlight = TRUE,
                  bordered = TRUE,
                  columns = list(
                    name_vat = colDef(name = "Item"),
                    pre = colDef(name = "Pre-Reform"),
                    current = colDef(name = "Post-reform"),
                    Diff = colDef(name = "Difference"),
                    Prop = colDef(name = "Difference in (%) from Pre-Reform")
                  ))
        
      })
      
    })
  }
 
