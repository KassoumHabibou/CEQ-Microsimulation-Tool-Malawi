

###################.
# chart theme ----
##################.
theme <- hc_theme(
  chart = list(
    backgroundColor = "white"
  ),
  plotOptions = list(
    series = list(
      animation = TRUE,
      connectNulls=TRUE
    ),
    column = list(
      groupPadding = 0
    )
  )
)

###################.
# Make weighted percentiles ----
##################.

make_weighted_percentiles <- function(curr_df, welfare, n) {
  
  welfare <- enquo(welfare)

  # Calculate quantile breakpoints using  Hmisc::wtd.quantile
  probs <- seq(0, 1, length.out = n + 1)
  percentile_breaks <- Hmisc::wtd.quantile(
    x = pull(curr_df, !!welfare),
    weights = pull(curr_df, weight),
    probs = probs,
    type="quantile"
  )
  
  # Assign quantile groups to all original data
  
  curr_df$percentile <- cut(pull(curr_df, !!welfare), 
                        breaks = as.numeric(percentile_breaks), 
                        labels = 1:n, 
                        include.lowest = TRUE)
  
  return(curr_df)

}



############################.
# bar chart chart ----
############################.
create_pov_bar_chart <- function(plot_data,
                             xaxis_col,
                             yaxis_col,
                             chart_theme = theme){
  
  
  # Reshape to long format for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c("Post-reform", "Pre-reform"),
                 names_to = "Policy",
                 values_to = "Value")
  
  
  plot_data_long <- plot_data_long %>%
    mutate(Policy = factor(Policy, levels = c("Pre-reform", "Post-reform")))
  
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Post-reform" = "#006D77")
  
  # Plot using highcharter
  hc <- hchart(
    plot_data_long,
    type = "column",
    hcaes(x = Income, y = Value, group = Policy)) %>% 
    hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>%
    hc_xAxis(title = list(text = "Income Concept")) %>% 
    hc_yAxis(title = list(text = "")) %>% 
    hc_add_theme(theme) %>% 
    hc_plotOptions(column = list(groupPadding = 0.1)) %>% 
    hc_tooltip(shared = TRUE)%>%
    hc_caption(
      text = "Data source: Malawi Fifth Integrated Household Survey 2019-2020",
      style = list(fontSize = "8px", color = "black")
    )
  
}

# Trend across income concepts (Highcharter)
create_pov_trend_chart <- function(plot_data,
                                   policies_to_plot = c("Pre-reform", "Post-reform"),
                                   chart_theme = theme) {
  
  income_order = c("Market Income plus pensions","Net Market Income",
                   "Gross Income", "Disposable Income","Consumable Income",
                   "Final Income")
  
  # Order 
  plot_data$Income <- factor(plot_data$Income, levels = income_order)
  
  # wide -> long 
  plot_data_long <- plot_data %>% 
    tidyr::pivot_longer(cols = c("Pre-reform", "Post-reform"),
                        names_to = "Policy",
                        values_to = "Value") %>% 
    dplyr::mutate(
      Policy = factor(Policy, levels = c("Pre-reform", "Post-reform"))
    ) %>% 
    dplyr::arrange(Policy, Income)
  
  # colors (same palette idea as your bars)
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Post-reform" = "#006D77")
  
  # plot
  hc <- highcharter::hchart(
    plot_data_long,
    type = "line",  # or "spline" if you prefer
    highcharter::hcaes(x = Income, y = Value, group = Policy)) %>% 
    highcharter::hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>% 
    highcharter::hc_xAxis(title = list(text = "Income concept"),
                          categories = levels(plot_data_long$Income)) %>% 
    highcharter::hc_plotOptions(series = list(
      marker = list(enabled = TRUE, radius = 3),
      lineWidth = 3
    )) %>% 
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 2) %>% 
    highcharter::hc_add_theme(theme) %>% 
    highcharter::hc_caption(
      text = "Data source: Malawi Fifth Integrated Household Survey 2019–2020",
      style = list(fontSize = "8px", color = "black")
    ) 
  
  return(hc)
}


create_ineq_bar_chart  <- function(plot_data,
                               xaxis_col,
                               yaxis_col,
                               chart_theme = theme){
  
  
  # Reshape to long format for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c("Post-reform", "Pre-reform"),
                 names_to = "Policy",
                 values_to = "Value")
  
  
  plot_data_long <- plot_data_long %>%
    mutate(Policy = factor(Policy, levels = c("Pre-reform", "Post-reform")))
  
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Post-reform" = "#006D77")
  
  # Plot using highcharter
  hc <- hchart(
    plot_data_long,
    type = "column",
    hcaes(x = Income, y = Value, group = Policy)) %>% 
    hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>%
    hc_xAxis(title = list(text = "Income Concept")) %>% 
    hc_yAxis(title = list(text = "")) %>% 
    hc_add_theme(theme) %>% 
    hc_plotOptions(column = list(groupPadding = 0.1)) %>% 
    hc_tooltip(shared = TRUE)%>%
    hc_caption(
      text = "Data source: Malawi Fifth Integrated Household Survey 2019-2020",
      style = list(fontSize = "8px", color = "black")
    )
  
  
}


# Trend across income concepts (Highcharter)
create_ineq_trend_chart <- function(plot_data,
                                   policies_to_plot = c("Pre-reform", "Post-reform"),
                                   chart_theme = theme) {
  
  income_order = c("Market Income plus pensions","Net Market Income",
                   "Gross Income", "Disposable Income","Consumable Income",
                   "Final Income")
  
  # Order 
  plot_data$Income <- factor(plot_data$Income, levels = income_order)
  
  # wide -> long 
  plot_data_long <- plot_data %>% 
    tidyr::pivot_longer(cols = c("Pre-reform", "Post-reform"),
                        names_to = "Policy",
                        values_to = "Value") %>% 
    dplyr::mutate(
      Policy = factor(Policy, levels = c("Pre-reform", "Post-reform"))
    ) %>% 
    dplyr::arrange(Policy, Income)
  
  # colors (same palette idea as your bars)
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Post-reform" = "#006D77")
  
  # plot
  hc <- highcharter::hchart(
    plot_data_long,
    type = "line",  # or "spline" if you prefer
    highcharter::hcaes(x = Income, y = Value, group = Policy)) %>% 
    highcharter::hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>% 
    highcharter::hc_xAxis(title = list(text = "Income concept"),
                          categories = levels(plot_data_long$Income)) %>% 
    highcharter::hc_plotOptions(series = list(
      marker = list(enabled = TRUE, radius = 3),
      lineWidth = 3
    )) %>% 
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 2) %>% 
    highcharter::hc_add_theme(theme) %>% 
    highcharter::hc_caption(
      text = "Data source: Malawi Fifth Integrated Household Survey 2019–2020",
      style = list(fontSize = "8px", color = "black")
    ) 
  
  return(hc)
}

create_incid_bar_chart <- function(plot_data,
                                 chart_theme = theme){
  
  
  # Reshape to long format for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c("Value", "Value_pre"),
                 names_to = "Policy",
                 values_to = "Value") %>% 
    mutate(Policy = recode(Policy,
      Value = "Post-reform",
      Value_pre = "Pre-reform"
    ))
  
  
  plot_data_long <- plot_data_long %>%
    mutate(Policy = factor(Policy, levels = c("Pre-reform", "Post-reform")))
  
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Post-reform" = "#006D77")
  
  # Plot using highcharter
  hc <- hchart(
    plot_data_long,
    type = "column",
    hcaes(x = percentile, y = Value, group = Policy)) %>% 
    hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>%
    hc_xAxis(title = list(text = "Percentiles")) %>% 
    hc_yAxis(title = list(text = "")) %>% 
    hc_add_theme(theme) %>% 
    hc_plotOptions(column = list(groupPadding = 0.1)) %>% 
    hc_tooltip(shared = TRUE)%>%
    hc_caption(
      text = "Data source: Malawi Fifth Integrated Household Survey 2019-2020",
      style = list(fontSize = "8px", color = "black")
    )
  
}

create_incid_trend_chart <- function(plot_data,
                                   chart_theme = theme){
  
  
  # Reshape to long format for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c("Value", "Value_pre"),
                 names_to = "Policy",
                 values_to = "Value") %>% 
    mutate(Policy = recode(Policy,
                           Value = "Post-reform",
                           Value_pre = "Pre-reform"
    ))
  
  
  plot_data_long <- plot_data_long %>%
    mutate(Policy = factor(Policy, levels = c("Pre-reform", "Post-reform")))
  
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Post-reform" = "#006D77")
  
  # Plot using highcharter
  hc <- hchart(
    plot_data_long,
    type = "line",  # or "spline" if you prefer
    hcaes(x = percentile, y = Value, group = Policy)) %>% 
    hc_colors(unname(hc_colors_vec[levels(plot_data_long$Policy)])) %>%
    highcharter::hc_xAxis(title = list(text = "Percentiles"),
                          categories = levels(plot_data_long$percentile)) %>% 
    highcharter::hc_plotOptions(series = list(
      marker = list(enabled = TRUE, radius = 3),
      lineWidth = 3
    )) %>% 
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 2) %>% 
    highcharter::hc_add_theme(theme) %>% 
    highcharter::hc_caption(
      text = "Data source: Malawi Fifth Integrated Household Survey 2019–2020",
      style = list(fontSize = "8px", color = "black")
    ) 
  


  
}