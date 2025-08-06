###############################################.
#
# App main server script
#
##############################################.



server <- function(input, output, session) {

    # Keeps the shiny app from timing out quickly on Posit 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })

  # Initial state: Disable the "Results" tab when the app starts
  # This observe runs once on startup.
  observe({
    shinyjs::disable(selector = '#nav li a[data-value="results"]')
  })
  
  observeEvent(input$tax_rate_lowest, {
    if (is.na(input$tax_rate_lowest) || input$tax_rate_lowest < 0 || input$tax_rate_lowest > 100) {
      feedbackWarning("tax_rate_lowest", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_lowest))
      updateNumericInput(session, "tax_rate_lowest", value = corrected_value)
    } else {
      hideFeedback("tax_rate_lowest")
    }
  })
  
  observeEvent(input$tax_rate_second, {
    if (is.na(input$tax_rate_second) || input$tax_rate_second < 0 || input$tax_rate_second > 100) {
      feedbackWarning("tax_rate_second", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_second))
      updateNumericInput(session, "tax_rate_second", value = corrected_value)
    } else {
      hideFeedback("tax_rate_second")
    }
  })
  
  observeEvent(input$tax_rate_middle, {
    if (is.na(input$tax_rate_middle) || input$tax_rate_middle < 0 || input$tax_rate_middle > 100) {
      feedbackWarning("tax_rate_middle", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_middle))
      updateNumericInput(session, "tax_rate_middle", value = corrected_value)
    } else {
      hideFeedback("tax_rate_middle")
    }
  })
  
  observeEvent(input$tax_rate_top, {
    if (is.na(input$tax_rate_top) || input$tax_rate_top < 0 || input$tax_rate_top > 100) {
      feedbackWarning("tax_rate_top", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_top))
      updateNumericInput(session, "tax_rate_top", value = corrected_value)
    } else {
      hideFeedback("tax_rate_top")
    }
  })
  
  
  # Create a reactiveVal to hold simulation output
  simulated_pov <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold inequality simulation output
  simulated_ineq <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold geo simulation output
  simulated_geo <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold revùob simulation output
  simulated_revmob  <- reactiveVal(NULL)
  # Run simulation when button is clicked
  observeEvent(input$simulate_button, {
    # Use user inputs for tax rates
    # tax_rate_lowest <- input$tax_rate_lowest / 100
    # tax_rate_second <- input$tax_rate_second / 100
    # tax_rate_middle <- input$tax_rate_middle / 100
    # tax_rate_top  <- input$tax_rate_top / 100
    
    
    # Agregate poverty estimates
    sim_result <- simulate_pov_est(
                                      # Direct taxes
                                        ## Income taxes
                                      input$tax_rate_lowest,
                                      input$tax_rate_second,
                                      input$tax_rate_middle,
                                      input$tax_rate_top,
                                      
                                      ## Income taxes
                                      #input$corp_tax_0,
                                      input$corp_tax_1,
                                      input$corp_tax_2,
                                      input$corp_tax_3,
                                      input$corp_tax_4,
                                      input$corp_tax_5,
                                      input$corp_tax_6,
                                      input$corp_tax_7,
                                      input$corp_tax_8,   
                                      input$corp_tax_9,
                                      input$corp_tax_10,
                                      input$corp_tax_11,
                                      input$corp_tax_12,   
                                      input$corp_tax_13,
                                      input$corp_tax_14,
                                      input$corp_tax_15,
                                      input$corp_tax_16,   
                                      input$corp_tax_17,
                                      input$corp_tax_18,
                                      
                                      input$remove_agriculture_exemption,
                                      input$remove_electricity_exemption,
                                      
                                      ## Indirect taxes
                                      
                                      input$vat_rate,
                                      input$excise_item_914,   # Wine or commercial liquor (250%)
                                      input$excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
                                      input$excise_item_913,   # Traditional beer (masese) (40%)
                                      input$excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
                                      input$excise_item_5801,  # Radio with flash drive/micro CD (20%)
                                      input$excise_item_211,   # Diesel (10%)
                                      input$excise_item_813,   # Tomato sauce (bottle) (10%)
                                      input$excise_item_333,   # Umbrella (10%)
                                      input$excise_item_909,   # Bottled water (5%)
                                      input$excise_item_519,   # Mini-bus (5%)
                                      input$excise_item_517,   # Motorcycle/scooter (5%)
                                      input$excise_item_611,   # Tractor (5%)
                                      
                                      # Items with zero excise rates
                                      input$excise_item_506,   # Pork (0%)
                                      input$excise_item_507,   # Mutton (0%)
                                      input$excise_item_106,   # Rice (0%)
                                      input$excise_item_111,   # Bread (0%)
                                      input$excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
                                      input$excise_item_801,   # Sugar (0%)
                                      input$excise_item_803,   # Cooking oil (0%)
                                      input$excise_item_810    # Salt (0%)
                                      
                                      
                                      )
    simulated_pov(sim_result)
    
    
    # Agregate geo poverty estimates
    sim_geo_result <- simulate_geo_est(
      input$tax_rate_lowest,
      input$tax_rate_second,
      input$tax_rate_middle,
      input$tax_rate_top,
      
      ## Income taxes
      #input$corp_tax_0,
      input$corp_tax_1,
      input$corp_tax_2,
      input$corp_tax_3,
      input$corp_tax_4,
      input$corp_tax_5,
      input$corp_tax_6,
      input$corp_tax_7,
      input$corp_tax_8,   
      input$corp_tax_9,
      input$corp_tax_10,
      input$corp_tax_11,
      input$corp_tax_12,   
      input$corp_tax_13,
      input$corp_tax_14,
      input$corp_tax_15,
      input$corp_tax_16,   
      input$corp_tax_17,
      input$corp_tax_18,
      
      input$remove_agriculture_exemption,
      input$remove_electricity_exemption,
      
      ## Indirect taxes
      
      input$vat_rate,
      input$excise_item_914,   # Wine or commercial liquor (250%)
      input$excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
      input$excise_item_913,   # Traditional beer (masese) (40%)
      input$excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
      input$excise_item_5801,  # Radio with flash drive/micro CD (20%)
      input$excise_item_211,   # Diesel (10%)
      input$excise_item_813,   # Tomato sauce (bottle) (10%)
      input$excise_item_333,   # Umbrella (10%)
      input$excise_item_909,   # Bottled water (5%)
      input$excise_item_519,   # Mini-bus (5%)
      input$excise_item_517,   # Motorcycle/scooter (5%)
      input$excise_item_611,   # Tractor (5%)
      
      # Items with zero excise rates
      input$excise_item_506,   # Pork (0%)
      input$excise_item_507,   # Mutton (0%)
      input$excise_item_106,   # Rice (0%)
      input$excise_item_111,   # Bread (0%)
      input$excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
      input$excise_item_801,   # Sugar (0%)
      input$excise_item_803,   # Cooking oil (0%)
      input$excise_item_810    # Salt (0%)
      
      )
    
    simulated_geo(sim_geo_result)
    
    sim_ineq_result <- simulate_ineq_est(
      
      input$tax_rate_lowest,
      input$tax_rate_second,
      input$tax_rate_middle,
      input$tax_rate_top,
      
      ## Income taxes
      #input$corp_tax_0,
      input$corp_tax_1,
      input$corp_tax_2,
      input$corp_tax_3,
      input$corp_tax_4,
      input$corp_tax_5,
      input$corp_tax_6,
      input$corp_tax_7,
      input$corp_tax_8,   
      input$corp_tax_9,
      input$corp_tax_10,
      input$corp_tax_11,
      input$corp_tax_12,   
      input$corp_tax_13,
      input$corp_tax_14,
      input$corp_tax_15,
      input$corp_tax_16,   
      input$corp_tax_17,
      input$corp_tax_18,
      
      input$remove_agriculture_exemption,
      input$remove_electricity_exemption,
      
      ## Indirect taxes
      
      input$vat_rate,
      input$excise_item_914,   # Wine or commercial liquor (250%)
      input$excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
      input$excise_item_913,   # Traditional beer (masese) (40%)
      input$excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
      input$excise_item_5801,  # Radio with flash drive/micro CD (20%)
      input$excise_item_211,   # Diesel (10%)
      input$excise_item_813,   # Tomato sauce (bottle) (10%)
      input$excise_item_333,   # Umbrella (10%)
      input$excise_item_909,   # Bottled water (5%)
      input$excise_item_519,   # Mini-bus (5%)
      input$excise_item_517,   # Motorcycle/scooter (5%)
      input$excise_item_611,   # Tractor (5%)
      
      # Items with zero excise rates
      input$excise_item_506,   # Pork (0%)
      input$excise_item_507,   # Mutton (0%)
      input$excise_item_106,   # Rice (0%)
      input$excise_item_111,   # Bread (0%)
      input$excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
      input$excise_item_801,   # Sugar (0%)
      input$excise_item_803,   # Cooking oil (0%)
      input$excise_item_810    # Salt (0%)
      
      )
    
    simulated_ineq(sim_ineq_result)
    
    
    # Simulate taxes and transfers
    # Agregate geo poverty estimates
    sim_geo_result <- simulate_geo_est(
      input$tax_rate_lowest,
      input$tax_rate_second,
      input$tax_rate_middle,
      input$tax_rate_top,
      
      ## Income taxes
      #input$corp_tax_0,
      input$corp_tax_1,
      input$corp_tax_2,
      input$corp_tax_3,
      input$corp_tax_4,
      input$corp_tax_5,
      input$corp_tax_6,
      input$corp_tax_7,
      input$corp_tax_8,   
      input$corp_tax_9,
      input$corp_tax_10,
      input$corp_tax_11,
      input$corp_tax_12,   
      input$corp_tax_13,
      input$corp_tax_14,
      input$corp_tax_15,
      input$corp_tax_16,   
      input$corp_tax_17,
      input$corp_tax_18,
      
      input$remove_agriculture_exemption,
      input$remove_electricity_exemption,
      
      ## Indirect taxes
      input$vat_rate,
      input$excise_item_914,   # Wine or commercial liquor (250%)
      input$excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
      input$excise_item_913,   # Traditional beer (masese) (40%)
      input$excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
      input$excise_item_5801,  # Radio with flash drive/micro CD (20%)
      input$excise_item_211,   # Diesel (10%)
      input$excise_item_813,   # Tomato sauce (bottle) (10%)
      input$excise_item_333,   # Umbrella (10%)
      input$excise_item_909,   # Bottled water (5%)
      input$excise_item_519,   # Mini-bus (5%)
      input$excise_item_517,   # Motorcycle/scooter (5%)
      input$excise_item_611,   # Tractor (5%)
      
      # Items with zero excise rates
      input$excise_item_506,   # Pork (0%)
      input$excise_item_507,   # Mutton (0%)
      input$excise_item_106,   # Rice (0%)
      input$excise_item_111,   # Bread (0%)
      input$excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
      input$excise_item_801,   # Sugar (0%)
      input$excise_item_803,   # Cooking oil (0%)
      input$excise_item_810    # Salt (0%)
      
      
    )
    
    simulated_geo(sim_geo_result)
    
    sim_revmob_result <- simulate_revmob_est(
      
      input$tax_rate_lowest,
      input$tax_rate_second,
      input$tax_rate_middle,
      input$tax_rate_top,
      
      ## Income taxes
      #input$corp_tax_0,
      input$corp_tax_1,
      input$corp_tax_2,
      input$corp_tax_3,
      input$corp_tax_4,
      input$corp_tax_5,
      input$corp_tax_6,
      input$corp_tax_7,
      input$corp_tax_8,   
      input$corp_tax_9,
      input$corp_tax_10,
      input$corp_tax_11,
      input$corp_tax_12,   
      input$corp_tax_13,
      input$corp_tax_14,
      input$corp_tax_15,
      input$corp_tax_16,   
      input$corp_tax_17,
      input$corp_tax_18,
      
      input$remove_agriculture_exemption,
      input$remove_electricity_exemption,
      
      ## Indirect taxes
      
      input$vat_rate,
      input$excise_item_914,   # Wine or commercial liquor (250%)
      input$excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
      input$excise_item_913,   # Traditional beer (masese) (40%)
      input$excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
      input$excise_item_5801,  # Radio with flash drive/micro CD (20%)
      input$excise_item_211,   # Diesel (10%)
      input$excise_item_813,   # Tomato sauce (bottle) (10%)
      input$excise_item_333,   # Umbrella (10%)
      input$excise_item_909,   # Bottled water (5%)
      input$excise_item_519,   # Mini-bus (5%)
      input$excise_item_517,   # Motorcycle/scooter (5%)
      input$excise_item_611,   # Tractor (5%)
      
      # Items with zero excise rates
      input$excise_item_506,   # Pork (0%)
      input$excise_item_507,   # Mutton (0%)
      input$excise_item_106,   # Rice (0%)
      input$excise_item_111,   # Bread (0%)
      input$excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
      input$excise_item_801,   # Sugar (0%)
      input$excise_item_803,   # Cooking oil (0%)
      input$excise_item_810    # Salt (0%)
      
    )
    
    simulated_revmob(sim_revmob_result)
    
    Sys.sleep(2)
    
    # After simulation, enable the "Results" tab
    shinyjs::enable(selector = '#nav li a[data-value="results"]')
    
    resetLoadingButton("simulate_button")
    
    # Show a toast notification that simulation is complete
    showToast(
      "info", 
      "Simulation complete! You can now click on the Results tab to see the results.", 
      .options = list(
        positionClass = "toast-bottom-full-width",  # Full width across top of screen
        progressBar = TRUE,  # Enable progress bar
        timeOut = 5000,  # Longer display time for important message
        closeButton = TRUE,
        newestOnTop = TRUE,
        preventDuplicates = FALSE,
        showDuration = 200,  # Slightly longer fade in
        hideDuration = 500,
        extendedTimeOut = 2000,  # Extended time on hover
        showEasing = "swing",  # More noticeable animation
        hideEasing = "linear",
        showMethod = "slideDown",  # Slide down from top
        hideMethod = "slideUp"  # Slide up when hiding
      )
      
    )

  })
  # Reactive for all policy input values (PAYE and Corporate)
  paye_value <- reactive({
    c(input$tax_rate_lowest, input$tax_rate_second, input$tax_rate_middle, input$tax_rate_top)
  })
  
  
  # Reactive for all policy input values (PAYE and Corporate) input$corp_tax_0,
  corp_value <- reactive({
    c(input$corp_tax_1,input$corp_tax_2,input$corp_tax_3,
      input$corp_tax_4,input$corp_tax_5,input$corp_tax_6,input$corp_tax_7,
      input$corp_tax_8, input$corp_tax_9,input$corp_tax_10,input$corp_tax_11,
      input$corp_tax_12, input$corp_tax_13,input$corp_tax_14,input$corp_tax_15,
      input$corp_tax_16,input$corp_tax_17,input$corp_tax_18)
  })
  
  # Indirect taxes
  vat_value <- reactive({
    c(input$vat_rate)
  })
  
  excise_value <- reactive({
  c(input$excise_item_914,   # Wine or commercial liquor (250%)
    input$excise_item_911,   # Bottled / canned beer (Carlsberg, etc) (250%)
    input$excise_item_913,   # Traditional beer (masese) (40%)
    input$excise_item_330,   # Cooking utensils (cookpots, stirring spoons and whisks, etc.) (20%)
    input$excise_item_5801,  # Radio with flash drive/micro CD (20%)
    input$excise_item_211,   # Diesel (10%)
    input$excise_item_813,   # Tomato sauce (bottle) (10%)
    input$excise_item_333,   # Umbrella (10%)
    input$excise_item_909,   # Bottled water (5%)
    input$excise_item_519,   # Mini-bus (5%)
    input$excise_item_517,   # Motorcycle/scooter (5%)
    input$excise_item_611,   # Tractor (5%)
    
    # Items with zero excise rates
    input$excise_item_506,   # Pork (0%)
    input$excise_item_507,   # Mutton (0%)
    input$excise_item_106,   # Rice (0%)
    input$excise_item_111,   # Bread (0%)
    input$excise_item_101,   # Maize ufa mgaiwa (normal flour) (0%)
    input$excise_item_801,   # Sugar (0%)
    input$excise_item_803,   # Cooking oil (0%)
    input$excise_item_810)    # Salt (0%)
  })
  
  sum_mod_server("sim_summary", simulated_pov, simulated_ineq, simulated_revmob, 
                 paye_value, corp_value,  vat_value, excise_value, session)
  
  # Load the poverty module, passing simulation result
  pov_mod_server("sim_poverty", simulated_pov, session)
  
  # Load the poverty module, passing simulation result
  geo_pov_mod_server("sim_geo_poverty", simulated_geo, session)
  
  # Load the poverty module, passing simulation result
  ineq_mod_server("sim_inequality", simulated_ineq, session)
  
  
}

##END

