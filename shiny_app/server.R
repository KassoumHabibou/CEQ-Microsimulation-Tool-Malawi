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

############################################## Input control ############################################## 
  # Generic helper: clamp to [0,100] with feedback + auto-correct
  observe_clamped_numeric <- function(id) {
    observeEvent(input[[id]], {
      val <- suppressWarnings(as.numeric(input[[id]]))
      if (is.na(val) | val < 0 | val > 100) {
        feedbackWarning(id, "Value must be between 0 and 100")
        updateNumericInput(session, id, value = max(0, min(100, val)))
      } else {
        hideFeedback(id)
      }
    }, ignoreInit = TRUE)
  }
  
  # 1) PAYE (your four existing fields)
  lapply(c("tax_rate_lowest","tax_rate_second","tax_rate_middle","tax_rate_top"), observe_clamped_numeric)
  
  # 2) Corporate (IDs corp_tax_1 ... corp_tax_18)
  lapply(sprintf("corp_tax_%d", 1:18), observe_clamped_numeric)
  
  # 3) VAT by item (IDs like vat_post_<code>)
  #    Use the same catalog you used to build the UI table; ensure it has a 'code' column
  #    If your CSV had 'Code', normalize once at load: names(vat_catalog) <- tolower(names(vat_catalog))
  lapply(as.character(vat_catalog$code), function(cd) observe_clamped_numeric(paste0("vat_post_", cd)))
  
  # 4) Excise by item (IDs like excise_post_<code>)
  #lapply(as.character(excise_catalog$code), function(cd) observe_clamped_numeric_excise(paste0("excise_post_", cd)))
  
  
################################################################################################   
  
  # Helper to clamp 0–100 and fallback to pre*100 when input is missing
  collect_post_vector_percent <- function(df, input, prefix) {

    v <- sapply(df$code, function(cd) {
      x <- input[[paste0(prefix, cd)]]
      if (is.null(x) || is.na(x)) df$pre_rate[df$code == cd] * 100 else as.numeric(x)
    }, USE.NAMES = FALSE)
    names(v) <- df$code         # keep codes for debugging
    v
  }
  
  collect_post_vector_value <- function(df, input, prefix) {
    
    v <- sapply(df$decile, function(cd) {
      x <- input[[paste0(prefix, cd)]]
      if (is.null(x) | is.na(x)) 0 else as.numeric(x)
    }, USE.NAMES = FALSE)
    names(v) <- df$decile         # keep codes for debugging
    v
  }

  ################################################################################################  
  # Create a reactiveVal to hold simulation output
  simulated_pov <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold inequality simulation output
  simulated_ineq <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold geo simulation output
  simulated_geo <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold revùob simulation output
  simulated_revmob  <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold main policy changes 
  simulated_df <- reactiveVal(NULL)
  
  # Run simulation when button is clicked
  observeEvent(input$simulate_button, {

    # read inputs INSIDE the observer
    vat_post_vec    <- collect_post_vector_percent(vat_catalog,    input, "vat_post_")
    excise_post_vec <- collect_post_vector_percent(excise_catalog, input, "excise_post_")
    
    dct_gov_hh_post_vec    <- collect_post_vector_value(dct_catalog,    input, "dct_gov_hh_post_")
    dct_fips_hh_post_vec <- collect_post_vector_value(dct_fips_catalog, input, "dct_fips_hh_post_")
    
    # --- Individual post-reform vectors (by pre-reform decile) -------------------
    dtr_frmz_hh_post_vec  <- collect_post_vector_value(dtr_frmz_hh_catalog,  input, "dtr_frmz_hh_post_")
    dtr_nfra_hh_post_vec  <- collect_post_vector_value(dtr_nfra_hh_catalog,  input, "dtr_nfra_hh_post_")
    dtr_masaf_hh_post_vec <- collect_post_vector_value(dtr_masaf_hh_catalog, input, "dtr_masaf_hh_post_")
    dtr_ffwk_hh_post_vec  <- collect_post_vector_value(dtr_ffwk_hh_catalog,  input, "dtr_ffwk_hh_post_")
    dtr_ifwp_hh_post_vec  <- collect_post_vector_value(dtr_ifwp_hh_catalog,  input, "dtr_ifwp_hh_post_")
    dtr_ses_hh_post_vec   <- collect_post_vector_value(dtr_ses_hh_catalog,   input, "dtr_ses_hh_post_")
    dtr_tes_hh_post_vec   <- collect_post_vector_value(dtr_tes_hh_catalog,   input, "dtr_tes_hh_post_")
    dtr_onc_hh_post_vec   <- collect_post_vector_value(dtr_onc_hh_catalog,   input, "dtr_onc_hh_post_")
    
    # -------------- Subsidies ------------------------------

    # clamp/collect already done — now just round to 2 dp
    vat_post_vec    <- round(vat_post_vec, 2)
    excise_post_vec <- round(excise_post_vec, 2)
    
    dct_gov_hh_post_vec    <- round(dct_gov_hh_post_vec, 2)
    dct_fips_hh_post_vec <- round(dct_fips_hh_post_vec, 2)
    dtr_frmz_hh_post_vec  <- round(dtr_frmz_hh_post_vec,  2)
    dtr_nfra_hh_post_vec  <- round(dtr_nfra_hh_post_vec,  2)
    dtr_masaf_hh_post_vec <- round(dtr_masaf_hh_post_vec, 2)
    dtr_ffwk_hh_post_vec  <- round(dtr_ffwk_hh_post_vec,  2)
    dtr_ifwp_hh_post_vec  <- round(dtr_ifwp_hh_post_vec,  2)
    dtr_ses_hh_post_vec   <- round(dtr_ses_hh_post_vec,   2)
    dtr_tes_hh_post_vec   <- round(dtr_tes_hh_post_vec,   2)
    dtr_onc_hh_post_vec   <- round(dtr_onc_hh_post_vec,   2)
    
    # Aggregate poverty estimates
    sim_df <- simulate_main_df(
      # --- Direct taxes (PAYE) ---
      input$tax_rate_lowest,
      input$tax_rate_second,
      input$tax_rate_middle,
      input$tax_rate_top,
      
      # --- Corporate taxes ---
      input$corp_tax_1,  input$corp_tax_2,  input$corp_tax_3,  input$corp_tax_4,  input$corp_tax_5,
      input$corp_tax_6,  input$corp_tax_7,  input$corp_tax_8,  input$corp_tax_9,  input$corp_tax_10,
      input$corp_tax_11, input$corp_tax_12, input$corp_tax_13, input$corp_tax_14, input$corp_tax_15,
      input$corp_tax_16, input$corp_tax_17, input$corp_tax_18,
      
      input$remove_agriculture_exemption,
      input$remove_electricity_exemption,
      
      # --- Indirect taxes (vectors by item, in order) ---
      vat_codes     = vat_catalog$code,
      vat_post      = vat_post_vec,
      excise_codes  = excise_catalog$code,
      excise_post   = excise_post_vec,
      
      # --- Subsidies (vectors by decile, in order) ---
      dct_gov_hh_decile  = dct_catalog$decile,
      dct_gov_hh         = dct_gov_hh_post_vec,
      dct_fips_hh_decile = dct_fips_catalog$decile,
      dct_fips_hh        = dct_fips_hh_post_vec,
      
      # --- Near-cash transfers (vectors by decile, in order) ---
      dtr_frmz_hh_decile  = dtr_frmz_hh_catalog$decile,
      dtr_frmz_hh         = dtr_frmz_hh_post_vec,
      
      dtr_nfra_hh_decile  = dtr_nfra_hh_catalog$decile,
      dtr_nfra_hh         = dtr_nfra_hh_post_vec,
      
      dtr_masaf_hh_decile = dtr_masaf_hh_catalog$decile,
      dtr_masaf_hh        = dtr_masaf_hh_post_vec,
      
      dtr_ffwk_hh_decile  = dtr_ffwk_hh_catalog$decile,
      dtr_ffwk_hh         = dtr_ffwk_hh_post_vec,
      
      dtr_ifwp_hh_decile  = dtr_ifwp_hh_catalog$decile,
      dtr_ifwp_hh         = dtr_ifwp_hh_post_vec,
      
      dtr_ses_hh_decile   = dtr_ses_hh_catalog$decile,
      dtr_ses_hh          = dtr_ses_hh_post_vec,
      
      dtr_tes_hh_decile   = dtr_tes_hh_catalog$decile,
      dtr_tes_hh          = dtr_tes_hh_post_vec,
      
      dtr_onc_hh_decile   = dtr_onc_hh_catalog$decile,
      dtr_onc_hh          = dtr_onc_hh_post_vec
      
      
      # # Electricity knobs 
      # elec_rate_subsidized = input$elec_rate_subsidized,  # MWK/kWh
      # elec_block1_kwh      = input$elec_block1_kwh,       # kWh/month
      # elec_rate_block2     = input$elec_rate_block2,      # MWK/kWh
      # 
      # # Fuel — national envelope share (% of GDP)
      # fuel_subsidy_pct_gdp = input$fuel_subsidy_pct_gdp   # percent, e.g., 0.243
      
    )
    
    
    simulated_df(sim_df)
    
    
    # Agregate poverty estimates
    sim_result <- simulate_pov_est(simulated_df)
    simulated_pov(sim_result)
    
    
    # Agregate geo poverty estimates
    sim_geo_result <- simulate_geo_est(simulated_df)
    simulated_geo(sim_geo_result)
    
    # Inequality
    sim_ineq_result <- simulate_ineq_est(simulated_df)
    simulated_ineq(sim_ineq_result)
    
    
    # Simulate taxes and transfers
    # Agregate geo poverty estimates
    sim_geo_result <- simulate_geo_est(simulated_df)
    simulated_geo(sim_geo_result)
    
    # Revenue mobilisation
    sim_revmob_result <- simulate_revmob_est(simulated_df)
    simulated_revmob(sim_revmob_result)
    
    Sys.sleep(2)
    
    # After simulation, enable the "Results" tab
    shinyjs::enable(selector = '#nav li a[data-value="results"]')
    
    resetLoadingButton("simulate_button")
    
    # navigate to Results and Summary sub-tab
    bslib::nav_select("nav", "results", session = session)
    bslib::nav_select("sub_tabs_results", "summary", session = session)
    
    # Show a toast notification that simulation is complete
    showToast(
      "info", 
      "Simulation complete! You can now naviguate the Results tab to see the results.", 
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
  
  sum_mod_server("sim_summary", simulated_pov, simulated_geo, simulated_ineq, simulated_revmob, 
                 paye_value, corp_value,  vat_value, excise_value, session)
  
  # Load the poverty module, passing simulation result
  pov_mod_server("sim_poverty", simulated_pov, session)
  
  # Load the poverty module, passing simulation result
  geo_pov_mod_server("sim_geo_poverty", simulated_geo, session)
  
  # Load the poverty module, passing simulation result
  ineq_mod_server("sim_inequality", simulated_ineq, session)
  
  # Load the Incidence module, passing simulation result
  incid_mod_server("sim_incidence", simulated_df, session)
  
}

##END

