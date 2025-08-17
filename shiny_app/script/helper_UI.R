
# Helper to build the full table purely in UI
build_vat_table_ui <- function(df, id_prefix = "vat_post_") {
  # Build body rows with numeric inputs 0–100 (step 0.1)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    code <- df$code[i]
    nm   <- df$itemcode[i]
    pre  <- df$pre_rate[i]
    
    tags$tr(
      tags$td(code),
      tags$td(nm),
      tags$td(sprintf("%.1f", pre*100)),
      tags$td(
        numericInput(
          inputId = paste0(id_prefix, code),
          label   = NULL,
          value   = round(pre*100,2),    # default to pre-reform rate
          min     = 0,
          max     = 100,
          step    = 0.1,
          width   = "110px"
        )
      )
    )
  })
  
  tags$table(
    class = "table table-borderless",
    tags$thead(
      tags$tr(
        tags$th("Code"),
        tags$th("Item"),
        tags$th("Pre-reform VAT (%)"),
        tags$th("Post-reform VAT (%)")
      )
    ),
    tags$tbody(do.call(tagList, rows))
  )
}


# Helper to build the full table purely in UI
build_excise_table_ui <- function(df, id_prefix = "excise_post_") {
  # Build body rows with numeric inputs 0–100 (step 0.1)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    code <- df$code[i]
    nm   <- df$itemcode[i]
    pre  <- df$pre_rate[i]
    
    tags$tr(
      tags$td(code),
      tags$td(nm),
      tags$td(sprintf("%.1f", pre*100)),
      tags$td(
        numericInput(
          inputId = paste0(id_prefix, code),
          label   = NULL,
          value   = round(pre*100,2),   # default to pre-reform rate
          min     = 0,
          max     = 1000,
          step    = 0.1,
          width   = "110px"
        )
      )
    )
  })
  
  tags$table(
    class = "table table-borderless",
    tags$thead(
      tags$tr(
        tags$th("Code"),
        tags$th("Item"),
        tags$th("Pre-reform excise rate (%)"),
        tags$th("Post-reform excise rate (%)")
      )
    ),
    tags$tbody(do.call(tagList, rows))
  )
}


build_dct_gov_table_ui <- function(df, id_prefix = "dct_gov_hh_post_") {
  dc_rows <- lapply(seq_len(nrow(df)), function(i) {
    d  <- df$decile[i]
    dc_mean <- df$dct_gov_hh_pre_mean[i]
    dc_median <- df$dct_gov_hh_pre_median[i]
    
    tags$tr(
      tags$td(d),
      tags$td(format(round(dc_mean, 0), big.mark = ",")),
      tags$td(format(round(dc_median, 0), big.mark = ",")),
      tags$td(
        numericInput(
          inputId = paste0(id_prefix, d),
          label   = NULL,
          value   = 0,   # default to pre-reform mean
          min     = -1000000,
          step    = 1,
          width   = "110px"
        )
      )
    )
  })
  
  tags$table(
    class = "table table-borderless table-sm align-middle",
    tags$thead(
      tags$tr(
        tags$th("Decile (Pre-reform)"),
        tags$th("Direct cash — mean (Pre-reform)"),
        tags$th("Direct cash — median (Pre-reform)"),
        tags$th("Add to Pre-reform in MWK (Post-reform)")
      )
    ),
    tags$tbody(do.call(tagList, dc_rows))
  )
}

build_fips_gov_table_ui <- function(df, id_prefix = "dct_fips_hh_post_") {
  fips_rows <- lapply(seq_len(nrow(df)), function(i) {
    d  <- df$decile[i]
    fp_mean <- df$dct_fips_hh_pre_mean[i]
    fp_median <- df$dct_fips_hh_pre_median[i]
    
    tags$tr(
      tags$td(d),
      tags$td(format(round(fp_mean, 0), big.mark = ",")),
      tags$td(format(round(fp_median, 0), big.mark = ",")),
      tags$td(
        numericInput(
          inputId = paste0(id_prefix, d),
          label   = NULL,
          value   = 0,   # default to pre-reform mean
          min     = -1000000,
          step    = 1,
          width   = "110px"
        )
      )
    )
  })
  
  tags$table(
    class = "table table-borderless table-sm align-middle",
    tags$thead(
      tags$tr(
        tags$th("Decile (Pre-reform)"),
        tags$th("FIPS — mean (Pre-reform)"),
        tags$th("FIPS — mean (Post-reform)"),
        tags$th("Add to Pre-reform in MWK (Post-reform)")
      )
    ),
    tags$tbody(do.call(tagList, fips_rows))
  )
}


# Generic builder for Near-cash programs (UI-only, no server render)
# df must have: decile, and either:
#   (a) pre_mean_value & pre_median_value  OR
#   (b) <var_key>_pre_mean & <var_key>_pre_median
build_nearcash_table_ui <- function(df, var_key, program_label, id_prefix) {
  
  # Resolve columns (generic first, then var-specific)
  mean_col   <- if ("pre_mean_value"   %in% names(df)) "pre_mean_value"   else paste0(var_key, "_pre_mean")
  median_col <- if ("pre_median_value" %in% names(df)) "pre_median_value" else paste0(var_key, "_pre_median")
  
  stopifnot(all(c("decile", mean_col, median_col) %in% names(df)))
  
  rows <- lapply(seq_len(nrow(df)), function(i) {
    d   <- df$decile[i]
    m   <- df[[mean_col]][i]
    md  <- df[[median_col]][i]
    
    tags$tr(
      tags$td(d),
      tags$td(format(round(m, 0),  big.mark = ",")),
      tags$td(format(round(md, 0), big.mark = ",")),
      tags$td(
        numericInput(
          inputId = paste0(id_prefix, d),
          label   = NULL,
          value   = 0,    # user adds to pre-reform mean (absolute MWK)
          min     = -1000000,
          step    = 1,
          width   = "110px"
        )
      )
    )
  })
  
  tags$table(
    class = "table table-borderless table-sm align-middle",
    tags$thead(
      tags$tr(
        tags$th("Decile (Pre-reform)"),
        tags$th(paste0(program_label, " — mean (Pre-reform)")),
        tags$th(paste0(program_label, " — median (Pre-reform)")),
        tags$th("Add to Pre-reform in MWK (Post-reform)")
      )
    ),
    tags$tbody(do.call(tagList, rows))
  )
}
