
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

# id = unique id 

sum_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      full_screen = FALSE,
      fill = TRUE,
      tags$hr(),
      card_header(
        bs_icon("bar-chart-line", size = "3em"),
        span("Main Results of the Policy Change", style = "font-size: 2em; font-weight: bold; vertical-align: middle; margin-left: 10px;"),
        class = "info-box-header"
      ),
      # Key Poverty & Inequality Metrics ----
      h2("Key Poverty and Inequality Metrics"),
      fluidRow(
        column(3, uiOutput(ns("card_pov_rate"))),
        column(3, uiOutput(ns("card_pov_gap"))),
        column(3, uiOutput(ns("card_pov_nbr"))),
        column(3, uiOutput(ns("card_gini")))
      ),
      
      h2("Governement Revenues"),
      fluidRow(
        title = "Government revenue",
        column(3, uiOutput(ns("card_direct_tax"))),
        column(3, uiOutput(ns("card_indirect_tax"))),
        column(3, uiOutput(ns("card_direct_transfert"))),
        column(3, uiOutput(ns("card_indirect_subsidies")))
      ),
      
      tags$hr(),
      
      # Policy Parameters Comparison ----
      # fluidRow(
      #   column(6, h4("PAYE"), tableOutput(ns("summary_paye"))),
      #   column(6, h4("Corporate"), tableOutput(ns("summary_corp")))
      # ),
      # h3("Indirect tax"),
      # fluidRow(
      #   column(6, h4("VAT"), tableOutput(ns("summary_vat"))),
      #   column(6, h4("Excise"), tableOutput(ns("summary_excise")))
      # ),
      
      h2("Policy Parameters Changes"),
      
      ## Section 1: Direct Tax
      h3("Direct Taxes (on Income and Profits)"),
      bslib::card(
        bslib::card_header("Pay As You Earn (PAYE) – Personal Income Tax"),
        card_body(
          reactableOutput(ns("paye_table"))
        )
      ),
      bslib::card(
        bslib::card_header("Corporate Income Tax – By Industry"),
        card_body(
          reactableOutput(ns("cor_table"))
        )
      ),
      
      tags$hr(style = "margin-top: 2em; margin-bottom: 1em;"),
      
      ## Section 2: Indirect Tax
      h3("Indirect Taxes (on Consumption)"),
      bslib::card(
        bslib::card_header("Value Added Tax (VAT) – By Category"),
        card_body(
          reactableOutput(ns("vat_table"))
        )
      ),
      bslib::card(
        bslib::card_header("Excise Duties – By Product"),
        card_body(
          reactableOutput(ns("excise_table"))
        )
      )
    )
  )
}


sum_mod_server <- function(id, simulated_pov, simulated_ineq, simulated_revmob, 
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
    
    
    output$card_pov_rate <- renderUI({
      req(filtered_pov_data())
      
      
      pov_rate_data <- filtered_pov_data() %>%
        filter(Parameter == "Rate of poverty")
      
      renderCard(pov_rate_data, "Poverty Rate", "bar-chart", is_inverse = FALSE)
    })
    
    output$card_pov_gap <- renderUI({
      req(filtered_pov_data())
      
      pov_gap_data <- filtered_pov_data() %>%
        filter(Parameter == "Poverty gap")
      
      
      renderCard(pov_gap_data, "Poverty Gap", "exclamation-triangle", is_inverse = FALSE)
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
      
      renderCard(ineq_data, "Gini Index", "cash", is_inverse = FALSE)
    })
    
    
    
    # output$card_wel_nbr <- renderUI({
    #   req(filtered_pov_data())
    #   
    #   welf_data <- filtered_pov_data() %>%
    #     filter(Parameter == "Welfare")
    #   
    #   renderCard(welf_data, "Welfare", "currency-dollar", is_inverse = TRUE)
    # })
    ### Revenue mobilisation
    output$card_direct_tax <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Direct taxes") %>% 
        mutate(`Current Policy` = `Current Policy`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Direct taxes", "bank", is_inverse = TRUE)
    })
    
    output$card_indirect_tax <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Indirect taxes") %>% 
        mutate(`Current Policy` = `Current Policy`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Indirect taxes", "receipt", is_inverse = TRUE)
    })
    
    output$card_direct_transfert <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Direct transfers") %>% 
        mutate(`Current Policy` = `Current Policy`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Direct transfers", "cash-stack", is_inverse = TRUE)
    })
    
    output$card_indirect_subsidies <- renderUI({
      req(simulated_revmob())
      
      revmob_data <- simulated_revmob() %>%
        filter(Area == "Country") %>% 
        filter(Parameter == "Indirect subsidies") %>% 
        mutate(`Current Policy` = `Current Policy`/1000000000,
               `Pre-reform` = `Pre-reform`/1000000000
        )
      
      renderCard_rev(revmob_data, "Indirect subsidies", "droplet-half", is_inverse = TRUE)
    })
    
    # Summary cards for each indicator
    renderCard <- function(data, title, icon, is_inverse = FALSE) {
      
      # Check if data is empty or has wrong structure
      if (nrow(data) == 0) {
        return(div(
          class = "alert alert-warning",
          paste("No data available for", title)
        ))
      }
      
      # Check if required columns exist
      if (!"Current Policy" %in% colnames(data) || !"Pre-reform" %in% colnames(data)) {
        return(div(
          class = "alert alert-danger",
          paste("Data structure error for", title, ". Available columns:", 
                paste(colnames(data), collapse = ", "))
        ))
      }
      
      val <- round(data$`Current Policy`, 0)
      base <- round(data$`Pre-reform`, 0)
      diff <- round(val - base, 0)
      prop <- round((diff / base) * 100, 2)
      
      if(data$Parameter=="Gini index"){
        val <- round(data$`Current Policy`, 2)
        base <- round(data$`Pre-reform`, 2)
        diff <- round(val - base, 2)
        prop <- round((diff / base) * 100, 2)
      }
      
      
      arrow_icon <- arrow_icon <- if (diff > 0) {
        "arrow-up"
      } else if (diff < 0) {
        "arrow-down"
      } else {
        "dash-circle"
      }
      
      arrow_html <- if (arrow_icon != "") as.character(bs_icon(arrow_icon)) else ""
      
      # Use inverse logic if higher value means improvement (e.g. reduction in poverty is positive)
      color <- ifelse(diff == 0, "warning", 
                      ifelse((diff > 0 & !is_inverse) | (diff < 0 & is_inverse), "danger", "info"))
      comment <- if (diff == 0) {
        paste0("No change (Pre-reform value ", base,")")
      } else if ((diff > 0 & !is_inverse) | (diff < 0 & is_inverse)) {
        paste0("Unfavorable (Pre-reform value ", base,")")
      } else {
        paste0("Improvement (Pre-reform value ", base,")")
      }
      
      # Use simple div structure for better compatibility
      div(
        class = "card",
        style = paste0("border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin: 10px 0; background-color: ", 
                       ifelse(color == "danger", "#f8d7da" , 
                              ifelse(color == "info", "#66B9BD", "#fff3cd")), ";"),
        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
          span(
            tagList(bs_icon(icon, size = "1.5em"),
                    span(title, style = "font-size: 1.2em; margin-left: 10px; font-weight: bold;"))
          ),
          span(HTML(arrow_html))
        ),
        div(
          style = "text-align: center;",
          h3(paste0(round(val, 2), " (", ifelse(diff >= 0, "+", ""), round(prop, 2), "%)"), 
             style = "margin: 10px 0; font-size: 1.5em;"),
          p(paste0(comment), style = "margin: 5px 0; font-size: 0.9em;")
        )
      )
    }
    
    
    # Summary cards for each indicator
    renderCard_rev <- function(data, title, icon, is_inverse = FALSE) {
      
      # Check if data is empty or has wrong structure
      if (nrow(data) == 0) {
        return(div(
          class = "alert alert-warning",
          paste("No data available for", title)
        ))
      }
      
      # Check if required columns exist
      if (!"Current Policy" %in% colnames(data) || !"Pre-reform" %in% colnames(data)) {
        return(div(
          class = "alert alert-danger",
          paste("Data structure error for", title, ". Available columns:", 
                paste(colnames(data), collapse = ", "))
        ))
      }
      
      val <- round(data$`Current Policy`, 0)
      base <- round(data$`Pre-reform`, 0)
      diff <- round(val - base, 0)
      prop <- round((diff / base) * 100, 2)
      
      
      arrow_icon <- if (diff > 0) {
        "arrow-up"
      } else if (diff < 0) {
        "arrow-down"
      } else {
        "dash-circle"
      }
      
      arrow_html <- if (arrow_icon != "") as.character(bs_icon(arrow_icon)) else ""
      
      # Use inverse logic if higher value means improvement (e.g. reduction in poverty is positive)
      color <- ifelse(diff == 0, "warning", 
                      ifelse((diff > 0 & !is_inverse) | (diff < 0 & is_inverse), "danger", "info"))
      comment <- if (diff == 0) {
        paste0("No change (Pre-reform value ", base," Billion MWK)")
      } else if ((diff > 0 & !is_inverse) | (diff < 0 & is_inverse)) {
        paste0("Unfavorable(Pre-reform value ", base," Billion MWK)")
      } else {
        paste0("Improvement (Pre-reform value ", base," Billion MWK)")
      }
      
      # Use simple div structure for better compatibility
      div(
        class = "card",
        style = paste0("border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin: 10px 0; background-color: ", 
                       ifelse(color == "danger", "#f8d7da" , 
                              ifelse(color == "info", "#66B9BD", "#fff3cd")), ";"),
        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
          span(
            tagList(bs_icon(icon, size = "1.5em"),
                    span(title, style = "font-size: 1.2em; margin-left: 10px; font-weight: bold;"))
          ),
          span(HTML(arrow_html))
        ),
        div(
          style = "text-align: center;",
          h3(paste0(round(val, 0), " Billion MWK (", ifelse(diff >= 0, "+", ""), round(prop, 2), "%)"), 
             style = "margin: 10px 0; font-size: 1.5em;"),
          p(paste0(comment), style = "margin: 5px 0; font-size: 0.9em;")
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
                  current = colDef(name = "Current Policy"),
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
                  current = colDef(name = "Current Policy"),
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
                      current = colDef(name = "Current Policy"),
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
                    current = colDef(name = "Current Policy"),
                    Diff = colDef(name = "Difference"),
                    Prop = colDef(name = "Difference in (%) from Pre-Reform")
                  ))
        
      })
      
    
 
    
    # Corporate summary table
    # output$summary_corp <- renderTable({
    #   
    # 
    #   df <- data.frame(
    #     Sector = corp_names,
    #     `Pre-Reform` = corp_baseline,
    #     `Current Policy` = corp_value(),
    #     stringsAsFactors = FALSE
    #   )
    #   
    #   df <- df %>% 
    #     mutate(Difference = Current.Policy - Pre.Reform) %>% 
    #     mutate(change = ifelse(Pre.Reform!=0,round(Difference/Pre.Reform),0)) %>% 
    #     mutate(Change_percent = change*100) 
    #   
    #   df <- df %>% 
    #     select(Sector, Pre.Reform, Current.Policy, Change_percent) %>% 
    #     rename("Pre-Reform" = Pre.Reform,
    #            "Current Policy" = Current.Policy,
    #            "Change in %" = Change_percent
    #     )
    #   df
    # }, digits = 2)
    
 

    
    # PAYE summary table
    # output$summary_paye <- renderTable({
    #   
    #   df <- data.frame(
    #     Bracket = paye_names,
    #     "Pre-Reform" = paye_baseline,
    #     "Current Policy" = paye_val(),
    #     stringsAsFactors = FALSE
    #   )
    #   df <- df %>% 
    #     mutate(Difference = Current.Policy - Pre.Reform) %>% 
    #     mutate(change = ifelse(Pre.Reform!=0,round(Difference/Pre.Reform),0)) %>% 
    #     mutate(Change_percent = round(change*100)) 
    #   
    #   df <- df %>% 
    #     select(Bracket, Pre.Reform, Current.Policy, Change_percent) %>% 
    #     rename("Pre-Reform" = Pre.Reform,
    #            "Current Policy" = Current.Policy,
    #            "Change in %" = Change_percent
    #     )
    #   df
    # }, digits = 2)

     
   }) # close moduleServer
 } # close sum_mod_server function
# 
# sum_mod_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     
#     # enable guided tour
#     # use_cicerone(),
#     
#     br(),
#     
#     bslib::card(
#       # bslib::card_header(
#       #   class = "d-flex flex-row-reverse",
#       #   layout_columns(
#       #     downloadButton(ns("download_summary_pdf"), "Download PDF report", class = "btn-sm btn-download", icon = icon("file-pdf")),
#       #     download_data_btns_ui(ns("download_summary_data")),
#       #   )
#       # ),
#       # Key Poverty & Inequality Metrics ----
#       fluidRow(
#         column(3, uiOutput(ns("card_pov_rate"))),
#         column(3, uiOutput(ns("card_pov_gap"))),
#         column(3, uiOutput(ns("card_pov_nbr"))),
#         column(3, uiOutput(ns("card_gini")))
#       )
#       
#       # card_body(
#       #   withSpinner(reactableOutput(ns("summary_table"))) # summary table
#       # )
#     )
#   )
# }
# 
# 
# ###############################################################################.
# # SERVER function 
# ###############################################################################.
# 
# # id = unique id
# # selected_geo = name of the reactive value storing selected areaname and selected areatype
# # selected_profile = name of reactive value storing selected profile
# # filtered_data = name of reactive dataframe where data has already been filtered by profile 
# 
# sum_mod_server <- function(id, simulated_pov, simulated_ineq, session) {
#   
#   moduleServer(id, function(input, output, session) {
#     
#     # permits compatibility between shiny and cicerone tours
#     ns <- session$ns
#     
#     pov_rate_data <- simulated_pov() %>%
#         filter(`Poverty line` == "pline_mod") %>%
#         filter(Area == "Country") %>%
#         filter(Parameter == "Rate of poverty") %>%
#         mutate(diff = `Pre-reform` - `Current Policy`) %>%
#         mutate(prop = diff*100/ `Pre-reform`)
# 
#     pov_gap_data <- simulated_pov() %>%
#       filter(`Poverty line` == "pline_mod") %>%
#       filter(Area == "Country") %>%
#       filter(Parameter == "Poverty gap") %>%
#       mutate(diff = `Pre-reform` - `Current Policy`) %>%
#       mutate(prop = diff*100/ `Pre-reform`)
# 
#     pov_nbr_data <- simulated_pov() %>%
#       filter(`Poverty line` == "pline_mod") %>%
#       filter(Area == "Country") %>%
#       filter(Parameter == "Number of poor") %>%
#       mutate(diff = `Pre-reform` - `Current Policy`) %>%
#       mutate(prop = diff*100/ `Pre-reform`)
#     
#     ineq_data <- simulated_ineq() %>%
#       filter(Income=="Disposable Income") %>%
#       filter(Area == "Country") %>%
#       filter(Parameter=="Gini index") %>%
#       mutate(diff = `Pre-reform` - `Current Policy`) %>%
#       mutate(prop = diff*100/ `Pre-reform`)
#     
#     
#     output$card_pov_rate <- renderUI({
#       pov_rate_data <- simulated_pov() %>%
#         filter(`Poverty line` == "pline_mod", Area == "Country", Parameter == "Rate of poverty")
#       renderCard(pov_rate_data, "Poverty Rate", "bar-chart", is_inverse = TRUE)
#     })
#     
#     output$card_pov_gap <- renderUI({
#       pov_gap_data <- simulated_pov() %>%
#         filter(`Poverty line` == "pline_mod", Area == "Country", Parameter == "Poverty gap")
#       renderCard(pov_gap_data, "Poverty Gap", "exclamation-triangle", is_inverse = TRUE)
#     })
#     
#     output$card_pov_nbr <- renderUI({
#       pov_nbr_data <- simulated_pov() %>%
#         filter(`Poverty line` == "pline_mod", Area == "Country", Parameter == "Number of poor")
#       renderCard(pov_nbr_data, "Number of Poor", "people-fill", is_inverse = TRUE)
#     })
#     
#     output$card_gini <- renderUI({
#       ineq_data <- simulated_ineq() %>%
#         filter(Income == "Disposable Income", Area == "Country", Parameter == "Gini index")
#       renderCard(ineq_data, "Gini Index", "graph-up-arrow", is_inverse = TRUE)
#     })
#     
#     # Corporate summary table
#     # output$summary_corp <- renderTable({
#     #   df <- data.frame(
#     #     Sector = corp_names,
#     #     `Pre-Reform` = corp_baseline,
#     #     `Current Policy` = unlist(lapply(corp_ids, function(id) input[[id]])),
#     #     stringsAsFactors = FALSE
#     #   )
#     #   df$Difference = df$`Current Policy` - df$`Pre-Reform`
#     #   df$`% Change` = round(100 * df$Difference / df$`Pre-Reform`, 2)
#     #   df <- df[df$Difference != 0, ]
#     #   df
#     # }, digits = 2)
#     # Summary cards for each indicator
#     renderCard <- function(data, title, icon, is_inverse = FALSE) {
#       val <- data$`Current Policy`
#       base <- data$`Pre-reform`
#       diff <- val - base
#       prop <- round((diff / base) * 100, 1)
#       
#       # Use inverse logic if higher value means improvement (e.g. reduction in poverty is positive)
#       color <- ifelse(diff == 0, "warning", 
#                       ifelse((diff > 0 & !is_inverse) | (diff < 0 & is_inverse), "danger", "info"))
#       comment <- if (diff == 0) {
#         "No change"
#       } else if ((diff > 0 & !is_inverse) | (diff < 0 & is_inverse)) {
#         "Unfavorable increase"
#       } else {
#         "Improvement"
#       }
#       
#       valueBox(
#         value = paste0(round(val, 2), " (", ifelse(diff >= 0, "+", ""), round(diff, 2), ")"),
#         subtitle = paste0(title, ": ", comment),
#         icon = bs_icon(icon),
#         color = color
#       )
#     }
#     
#     # Summary Cards
#     output$card_poor <- renderUI({
#       valueBox(
#         value = paste0(format(sim_data$new_poor, big.mark = ","),
#                        " (", round(sim_data$new_poor / sim_data$baseline_poor * 100, 1), "%)"),
#         subtitle = "New Poor Compared to Baseline",
#         icon = bs_icon("people-fill"),
#         color = "danger"
#       )
#     })
#     
#     output$card_gap <- renderUI({
#       valueBox(
#         value = paste0("+", round(sim_data$poverty_gap_change * 100, 2), "%"),
#         subtitle = "Change in Poverty Gap",
#         icon = bs_icon("exclamation-triangle-fill"),
#         color = "warning"
#       )
#     })
#     
#     output$card_gini <- renderUI({
#       valueBox(
#         value = paste0(round(sim_data$gini_new, 3),
#                        " (", ifelse(sim_data$gini_new - sim_data$gini_base >= 0, "+", "-"),
#                        round(abs(sim_data$gini_new - sim_data$gini_base), 3), ")"),
#         subtitle = "Gini Index (Post Reform)",
#         icon = bs_icon("graph-up-arrow"),
#         color = "info"
#       )
#     })
#     
#     #   # get latest data for each indicator
#     #   chosen_area <- dt[type_definition != "Number",
#     #                     .SD[!is.na(measure) & year == max(year)], by = indicator]
#     #   
#     #   # include scotland figures for comparison
#     #   scotland <- main_dataset[areaname == "Scotland"]
#     #   chosen_area <- chosen_area[scotland, on = c("ind_id", "year"), scotland_value := scotland$measure]
#     #   
#     #   # calculate quantiles for each indicator within chosen geography level for spine chart
#     #   chosen_areatype <- unique(chosen_area$areatype)
#     #   other_areas <- main_dataset[areatype == chosen_areatype][chosen_area, on = .(ind_id, year), nomatch = 0][,
#     #                                                                                                            .(Q0 = quantile(measure, probs = 0, na.rm = TRUE),
#     #                                                                                                              Q100 = quantile(measure, probs = 1, na.rm = TRUE),
#     #                                                                                                              Q25 = quantile(measure, probs = 0.25, na.rm = TRUE),
#     #                                                                                                              Q75 = quantile(measure, probs = 0.75, na.rm = TRUE)),
#     #                                                                                                            by = .(ind_id, year)]
#     #   
#     #   # add quantile values for each indicator to table
#     #   chosen_area <- chosen_area[other_areas, on = c("ind_id", "year"),
#     #                              c("Q0", "Q100", "Q25", "Q75") := .(other_areas$Q0, other_areas$Q100 ,other_areas$Q25, other_areas$Q75)]
#     #   
#     #   
#     #   # if the selected profile has a particular order the domains should appear in the table
#     #   # (i.e. the selected profile's domain order isn't NULL in the 'profiles_list' from the global script)
#     #   # then covert the domain column to factor and set levels to ensure the data is ordered accordingly
#     #   if(!is.null(selected_profile()$domain_order)){
#     #     chosen_area <- chosen_area[, domain := factor(domain, levels = selected_profile()$domain_order)]
#     #   }
#     #   
#     #   chosen_area <- setorder(chosen_area, domain)
#     #   
#     #   # assign colours to values depending on statistical significance
#     #   final <- chosen_area %>%
#     #     mutate(marker_colour = case_when(lowci <= scotland_value & upci >= scotland_value & interpret %in% c("H", "L") ~'#6A6C6D',
#     #                                      lowci > scotland_value & interpret == "H" ~ '#1B7CED',
#     #                                      lowci > scotland_value & interpret == "L" ~ '#FFA500',
#     #                                      upci < scotland_value & interpret == "L" ~ '#1B7CED',
#     #                                      upci < scotland_value & interpret == "H" ~ '#FFA500',
#     #                                      interpret == "O" ~ '#FFFFFF', TRUE ~ '#FFFFFF'))
#     #   
#     #   
#     #   # creating spine chart data ----
#     #   final <- final %>%
#     #     # duplicate chosen area value in another column so one can be used in the table and one can be used for spine chart
#     #     mutate(chosen_value = measure) %>%
#     #     
#     #     mutate(scale_min = case_when(scotland_value - Q0 > Q100 - scotland_value ~ Q0,
#     #                                  TRUE ~ scotland_value - (Q100 - scotland_value))) %>%
#     #     mutate(scale_max = case_when(scale_min == Q0 ~ scotland_value + (scotland_value - Q0), TRUE ~ Q100)) %>%
#     #     mutate(across(c(chosen_value, Q0, Q25, Q75, Q100), ~ (. - scale_min) / (scale_max - scale_min)))
#     #   
#     #   
#     #   final <- final %>%
#     #     mutate(across(c("Q0", "Q25", "Q75", "Q100", "chosen_value"), ~ case_when(interpret == "L" ~ 1 - ., TRUE ~ .)))
#     #   
#     #   
#     #   # conditionally calculating worst to best, depending on whether a lower is value is better or a higher value is better
#     #   # this ensures that 'worst' is always to the left of the spine, and 'best' is always to the right
#     #   final <- final %>%
#     #     mutate(worst = case_when(interpret == "L" ~ Q0 - Q25, TRUE ~ Q100 - Q75), # worst
#     #            p25 = case_when(interpret == "L" ~ Q25 - Q75, TRUE ~ Q75 - Q25), # 25th percentile
#     #            p75 = case_when(interpret == "L" ~ Q75 - Q100, TRUE ~ Q25 - Q0), # 75th percentile
#     #            best = case_when(interpret == "L" ~ Q100, TRUE ~ Q0) # best
#     #     )
#     #   
#     #   final$chart <- NA # create empty column to populate with in-line highcharts
#     #   
#     #   # create id to use for the charts (each chart needs a unique id otherwise chart won't display)
#     #   final <- final |> 
#     #     mutate(unique_id = paste0("highchart-", indicator, "-", chosen_value, Sys.time()))
#     #   
#     #   
#     #   # selecting columns required for table ----
#     #   final <- final %>%
#     #     select(code,
#     #            areaname,
#     #            areatype,
#     #            domain, # required for domain column 
#     #            indicator, # required for indicator column 
#     #            measure, # required for for selected area column 
#     #            scotland_value, # required for scotland column 
#     #            type_definition, # required for indicator column 
#     #            def_period, # required for indicator column 
#     #            chart, # required for spine chart 
#     #            worst, # required for building spine chart 
#     #            p25, # required for building spine chart 
#     #            p75, # required for building spine chart 
#     #            best, # required for building spine chart 
#     #            chosen_value, # required for building spine chart 
#     #            marker_colour, # required for building spine chart 
#     #            unique_id) # required for building spine chart )
#     #   
#     #   final
#     #   
#     # })
#     # 
#     # 
#     # # prepare scotland summary data ----
#     # scotland_summary <- reactive({
#     #   req(selected_geo()$areatype == "Scotland")
#     #   
#     #   # set the profile data to data.table format
#     #   dt <- setDT(filtered_data())
#     #   
#     #   # filter on scotland 
#     #   dt <- dt[areaname == "Scotland" & type_definition != "number"]
#     #   
#     #   # remove archived indicators
#     #   dt <- dt[!(ind_id %in% archived_indicators)]
#     #   
#     #   # order the data before grouping
#     #   setorder(dt, indicator, year)
#     #   
#     #   # aggregate to 1 row per indicator (this step is equivalent using group_by() and summarise() from dplyr)
#     #   dt <- dt[,.(measures = list(measure), # for the trend chart 
#     #               years = list(year), # for the trend chart 
#     #               domain = first(domain), # for the table 
#     #               trend_min = first(trend_axis), # for the trend chart label
#     #               trend_max = last(trend_axis), # for the trend
#     #               def_period = last(def_period), # for the table
#     #               type_definition = first(type_definition), # for the table
#     #               measure = last(measure), # for the table
#     #               code = "S00000001", # for data download
#     #               areatype = "Scotland", # for data download
#     #               areaname = "Scotland" # for data download
#     #   ),
#     #   by = indicator]
#     #   
#     #   # create some additional cols
#     #   dt <- dt[, trend := NA] # empty column to place chart in 
#     #   dt <- dt[, unique_id := paste0(indicator, Sys.time())] # unique id for each chart 
#     #   
#     #   # set domain column as the first in the table
#     #   setcolorder(dt, "domain")
#     #   
#     #   
#     #   # if the selected profile has a particular order the domains should appear in the table
#     #   # (i.e. the selected profile's domain order isn't NULL in the 'profiles_list' from the global script)
#     #   # then covert the domain column to factor and set levels to ensure the data is ordered accordingly
#     #   if(!is.null(selected_profile()$domain_order)){
#     #     dt <- dt[, domain := factor(domain, levels = selected_profile()$domain_order)]
#     #   }
#     #   
#     #   dt <- setorder(dt, domain)
#     #   
#     #   dt
#     #   
#     #   
#     # })
#     # 
#     # 
#     # prepare data for download extract ----
#     # data_download <- reactive({
#     #   if(selected_geo()$areatype == "Scotland"){
#     #     df <- scotland_summary() |>
#     #       select("code",
#     #              "areaname",
#     #              "areatype",
#     #              "domain", 
#     #              "indicator", 
#     #              "definition_period" = "def_period", 
#     #              "type_definition", 
#     #              "measure")
#     #   } else {
#     #     df <- local_summary() |>
#     #       select("code",
#     #              "areaname",
#     #              "areatype",
#     #              "domain",
#     #              "indicator",
#     #              "type_definition",
#     #              "definition_period" = "def_period",
#     #              "measure",
#     #              "scotland_value")
#     #   }
#     #   
#     #   df
#     # })
#     # 
#     # 
#     # 
#     # 
#     # output$summary_table <- renderReactable({
#     #   
#     #   # determine which dataset to use
#     #   data <- if(selected_geo()$areatype == "Scotland"){
#     #     scotland_summary()
#     #   } else {
#     #     local_summary()
#     #   }
#     #   
#     #   shiny::validate(
#     #     need( nrow(data) > 0, "No indicators available")
#     #   )
#     #   
#     #   # domain column ----
#     #   domain =  colDef(
#     #     name = "Domain",
#     #     maxWidth = 120,
#     #     # this JS function hides domain name from appearing on every row
#     #     # i.e. gives appearance of 'merged' cells
#     #     style = JS("function(rowInfo, column, state) {
#     #                                      const prevRow = state.pageRows[rowInfo.viewIndex - 1]
#     #                                      if (prevRow && rowInfo.values['domain'] === prevRow['domain']) {
#     #                                        return {visibility: 'hidden'}
#     #                                      }
#     #                                    }
#     #                                  "))
#     #   
#     #   
#     #   # indicator column ----
#     #   indicator = colDef(
#     #     name = "Indicator",
#     #     minWidth = 320,
#     #     html = TRUE,
#     #     
#     #     # this JS function creates clickable links to view trend data
#     #     # when user clicks an indicator, it navigates to 'trends' tab
#     #     # and updates filters on that tab to that particular indicator and users chosen geography area
#     #     cell = JS(" function(rowInfo){
#     #                                           return `<div>
#     #                                                   <div style =  'font-weight: bold;'>${rowInfo.values['indicator']}</div>
#     #                                                   <div style = 'margin-top: 3px;'>
#     #                                                   <span style = 'margin-right: 0.25rem; padding: 2px; background-color:#F5F5F5;
#     #                                                                   border: 1px solid hsl(0, 0%, 75%);
#     #                                                                   border-radius: 2px;'>${rowInfo.values['type_definition']}</span>
#     #                                                               <span> • </span><span>${rowInfo.values['def_period']}
#     #                                                               </span>
#     #                                                                  </div>
#     #                                                                </div>`;}"))
#     #   
#     #   # Scotland column ----
#     #   scotland_value = colDef(
#     #     maxWidth = 80,
#     #     name = "Scotland",
#     #     cell = function(value){
#     #       div(style = "margin-top: 19px;", value)
#     #     }
#     #   )
#     #   
#     #   
#     #   
#     #   # Chosen area column ----
#     #   measure = colDef(
#     #     maxWidth = 80,
#     #     name = as.character(selected_geo()$areaname),
#     #     cell = function(value){
#     #       div(style = "margin-top: 19px;", value)
#     #     }
#     #   )
#     #   
#     #   # spine chart column ----
#     #   chart = colDef(name = "Chart",
#     #                  html = T,
#     #                  minWidth = 200,
#     #                  cell = JS("
#     #           function(rowInfo) {
#     #             var containerId = rowInfo.row.unique_id;
#     #             var rowData = rowInfo.row;
#     # 
#     #             var chartHTML = '<div id=\"' + containerId + '\" style=\"height: 70px; width: 100%\"></div>';
#     # 
#     #             setTimeout(function() {
#     #               Highcharts.chart(containerId, {
#     #                 chart: {
#     #                   type: 'bar',
#     #                   backgroundColor:'transparent',
#     #                   animation: false
#     #                 },
#     #                 title: {
#     #                   text: ''
#     #                 },
#     #                 xAxis: {
#     #                   categories: [''],
#     #                   title: { text: null },
#     #                   lineWidth: 0,
#     #                   lineColor: 'transparent',
#     #                   gridLineColor: 'transparent'
#     #                 },
#     #                 yAxis: {
#     #                   min: 0,
#     #                   max: 1,
#     #                   title: { text: null },
#     #                   labels: { enabled: false },
#     #                   startOnTick: false,
#     #                   endOnTick: false,
#     #                   gridLineColor: 'transparent',
#     #                   plotLines:[{
#     #                   color: 'red',
#     #                   width: 3,
#     #                   value: 0.5,
#     #                   zIndex: 1000
#     #                   }],
#     #                   lineColor: 'transparent'
#     #                   },
#     #                 legend: { enabled: false },
#     #                 plotOptions: {
#     #                   series: {
#     #                     animation: false,
#     #                     stacking: 'normal',
#     #                     dataLabels: {
#     #                       enabled: false
#     #                     },
#     #                     enableMouseTracking: false
#     #                   }
#     #                 },
#     #                 series: [{
#     #                   name: 'worst',
#     #                   data: [{y: rowData.worst, color: '#D3D3D3'}]
#     #                 }, {
#     #                   name: '25th Percentile',
#     #                 data: [{y: rowData.p25, color: '#A4A4A4'}]
#     #                 }, {
#     #                   name: '75th Percentile',
#     #                   data: [{y: rowData.p75, color: '#D3D3D3'}]
#     #                 }, {
#     #                   name: 'best',
#     #                   data: [{y: rowData.best, color: 'white'}]
#     #                 }, {
#     #                   type: 'scatter',
#     #                   data: [{
#     #                     x: 0,
#     #                     y: rowData.chosen_value,
#     #                     marker: {
#     #                       radius: 8,
#     #                       lineWidth: 1,
#     #                       lineColor: 'black',
#     #                       fillColor: rowData.marker_colour
#     #                     }
#     #                   }],
#     #                   marker: {symbol: 'circle'},
#     #                   zIndex: 5
#     #                 }],
#     #                 credits: {
#     #                   enabled: false
#     #                 },
#     #                 exporting: {
#     #                   enabled: false
#     #                 },
#     #                 tooltip: {
#     #                   enabled: false
#     #                 }
#     #               });
#     #             }, 20); // add brief delay before rendering charts 
#     # 
#     #             return chartHTML;
#     #           }
#     #         "))
#     #   
#     #   
#     #   
#     #   trend = colDef(
#     #     name = "Trend",
#     #     html = TRUE,
#     #     cell = JS("
#     #         function(rowInfo) {
#     #           var containerId = rowInfo.row.unique_id;
#     #           var chartHTML = '<div id=\"' + containerId + '\" style=\"height: 100px; width: 100%\"></div>';
#     #           var time_period = rowInfo.row.trend_min + ' to ' + rowInfo.row.trend_max;
#     #           
#     #           setTimeout(function() {
#     #             Highcharts.chart(containerId, {
#     #               chart: {
#     #                 type: 'area',
#     #                 backgroundColor:'transparent',
#     #                 animation: false
#     #               },
#     #               title: {
#     #                 text: time_period,
#     #                  style: {
#     #                   fontSize: '12px'
#     #                 }
#     #               },
#     #               xAxis: {
#     #                 categories: rowInfo.row.years,
#     #                 labels: {
#     #                   enabled: false
#     #                 },
#     #                 tickLength: 0,
#     #                 lineWidth: 0
#     #               },
#     #               yAxis: {
#     #                 title: {
#     #                   text: null,
#     #                   
#     #                 },
#     #                 labels: {
#     #                   enabled: false
#     #                 },
#     #                 gridLineWidth: 0
#     #               },
#     #               series: [{
#     #                 name: '',
#     #                 data: rowInfo.row.measures,
#     #                 color: '#0078D4',
#     #                 fillColor: {
#     #                 linearGradient: {
#     #                 x1: 0,
#     #                 y1: 0,
#     #                 x2: 0,
#     #                 y2: 1
#     #                 },
#     #                 stops: [
#     #                 [0, '#B3D7F2'],
#     #                 [1, '#E6F2FB'] 
#     #                 ]
#     #                 },
#     #                 marker: {
#     #                 enabled: true,
#     #                 radius: 3 
#     #                 }
#     #                 
#     #               }],
#     #               credits: {
#     #                   enabled: false
#     #                 },
#     #                 exporting: {
#     #                   enabled: false
#     #                 },
#     #                 tooltip: {
#     #                   enabled: false
#     #                 },
#     #                 legend: { 
#     #                 enabled: false 
#     #                 },
#     #                 plotOptions: {
#     #                   series: {
#     #                     animation: false,
#     #                     connectNulls: true,
#     #                     dataLabels: {
#     #                       enabled: false
#     #                     },
#     #                     enableMouseTracking: false
#     #                   }
#     #                 }
#     #             });
#     #           }, 20); // add brief delay before rendering charts
#     #           
#     #           return chartHTML;
#     #         }
#     #       ")
#     #   )
#     #   
#     #   
#     #   
#     #   if(selected_geo()$areatype == "Scotland"){
#     #     cols <- list(domain = domain, 
#     #                  indicator = indicator,
#     #                  measure = measure, 
#     #                  trend = trend,
#     #                  def_period = colDef(show = FALSE),
#     #                  years = colDef(show = FALSE),
#     #                  measures = colDef(show = FALSE),
#     #                  type_definition = colDef(show = FALSE),
#     #                  unique_id = colDef(show = FALSE),
#     #                  trend_min = colDef(show = FALSE),
#     #                  trend_max = colDef(show = FALSE),
#     #                  code = colDef(show = FALSE),
#     #                  areatype = colDef(show = FALSE),
#     #                  areaname = colDef(show = FALSE)
#     #                  
#     #     )
#     #   } else {
#     #     cols <- list(domain = domain, 
#     #                  indicator = indicator, 
#     #                  scotland_value = scotland_value, 
#     #                  measure = measure, 
#     #                  chart = chart,
#     #                  code = colDef(show = FALSE),
#     #                  areaname = colDef(show = FALSE),
#     #                  areatype = colDef(show = FALSE),
#     #                  worst = colDef(show = FALSE),
#     #                  p25 = colDef(show = FALSE),
#     #                  p75 = colDef(show = FALSE),
#     #                  best = colDef(show = FALSE),
#     #                  chosen_value = colDef(show = FALSE),
#     #                  def_period = colDef(show = FALSE),
#     #                  marker_colour = colDef(show = FALSE),
#     #                  type_definition = colDef(show = FALSE),
#     #                  unique_id = colDef(show = FALSE))
#     #   }
#     #   
#     #   
#     #   reactable(data,
#     #             compact = TRUE,
#     #             defaultPageSize = nrow(data),
#     #             defaultExpanded = T,
#     #             sortable = F,
#     #             highlight = FALSE,
#     #             theme = reactableTheme(
#     #               headerStyle = list(backgroundColor = "#ECECEC")
#     #             ),
#     #             columns = cols)
#     #   
#     #   
#     #   
#     #   
#     # })
#     # 
#     # 
#     # # download data module ----
#     # download_data_btns_server(id = "download_summary_data", 
#     #                           file_name = "ScotPHO_summary_data_extract",
#     #                           data = data_download
#     # )
#     # 
#     # 
#     # # download PDF logic ----
#     # output$download_summary_pdf <- downloadHandler(
#     #   
#     #   # name of file when downloaded
#     #   filename = function() {
#     #     paste(selected_profile()$full_name, "-summary-", selected_geo()$areaname, ".pdf", sep="")
#     #   },
#     #   
#     #   content = function(file) {
#     #     
#     #     
#     #     td <- tempdir() # create a temporary directory
#     #     
#     #     markdown_doc <- ifelse(selected_geo()$areatype == "Scotland", "scotland_summary_table_pdf.Rmd", "spinecharts.Rmd")
#     #     
#     #     tempReport <- file.path(td, markdown_doc) # rmarkdown file 
#     #     tempLogo <- file.path(td, "scotpho_reduced.png") # scotpho logo to add to pdf
#     #     
#     #     file.copy(markdown_doc, tempReport, overwrite = TRUE) 
#     #     file.copy("scotpho_reduced.png", tempLogo, overwrite = TRUE)
#     #     
#     #     # Set up parameters to pass to Rmd document
#     #     params <- list(
#     #       reactive_df = if (selected_geo()$areatype == "Scotland") {
#     #         scotland_summary()
#     #       } else {
#     #         local_summary()
#     #       },
#     #       chosen_area = selected_geo()$areaname,
#     #       chosen_profile = selected_profile()$full_name,
#     #       chosen_geography_level = selected_geo()$areatype
#     #     )
#     #     
#     #     # create the pdf report 
#     #     rmarkdown::render(tempReport, output_file = file,
#     #                       params = params,
#     #                       envir = new.env(parent = globalenv())
#     #     )
#     #     
#     #     # unload package after each time report generated
#     #     # otherwise users can only download 1 pdf, and any other download attempts will fail
#     #     # details of issue here: https://stackoverflow.com/questions/46080853/why-does-rendering-a-pdf-from-rmarkdown-require-closing-rstudio-between-renders
#     #     detach("package:kableExtra", unload=TRUE)
#     #   }
#     # )
#     # 
#     # 
#     # 
#     # 
#     # ###########################################.
#     # # Guided tour ----
#     # ###########################################.
#     # 
#     # guide_summary <- Cicerone$
#     #   new()$
#     #   step(
#     #     ns("summary_download_pdf_wrapper"),
#     #     "Download PDF",
#     #     "Click here to download a PDF containing all available indicators for the selected area.",
#     #     position = "below"
#     #   )$
#     #   step(
#     #     ns("summary_download_data_wrapper"),
#     #     "Download Data Button",
#     #     "Click here to download the selected data as a CSV, RDS or JSON file.",
#     #     position = "below"
#     #   )
#     # 
#     # #initiate the guide
#     # guide_summary$init()
#     # 
#     # #when guided tour button is clicked, start the guide
#     # observeEvent(input$summary_tour_button, {
#     #   guide_summary$start()
#     # })
#     # 
#     
#     
#     
#   }) # close module server
# } # close module
# 
# 
# 
