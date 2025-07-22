

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


############################.
# bar chart chart ----
############################.
create_pov_bar_chart <- function(plot_data,
                             xaxis_col,
                             yaxis_col,
                             chart_theme = theme){
  
  
  # Reshape to long format for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c("Current Policy", "Pre-reform"),
                 names_to = "Policy",
                 values_to = "Value")
  
  
  plot_data_long <- plot_data_long %>%
    mutate(Policy = factor(Policy, levels = c("Pre-reform", "Current Policy")))
  
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Current Policy" = "#006D77")
  
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



############################.
# bar chart chart ----
############################.
create_geo_pov_bar_chart <- function(plot_data,
                                 xaxis_col,
                                 yaxis_col){
  
  
  # Reshape to long format for plotting
  plot_data_long <- plot_data %>%
    pivot_longer(cols = c("Current Policy", "Pre-reform"),
                 names_to = "Policy",
                 values_to = "Value")
  
  hc_colors_vec <- c("Pre-reform" = "#A1AEB1", "Current Policy" = "#006D77")
  
  plot_data_long <- plot_data_long %>%
    mutate(Policy = factor(Policy, levels = c("Pre-reform", "Current Policy")),  
           color = hc_colors_vec[as.character(Policy)])
  
  # Plot using highcharter
  hc <- hchart(plot_data_long, 
         type = "bar", # 'bar' = horizontal bar chart
         hcaes(x = admin_name, y = Value, group = Policy)) %>%
    hc_colors(unname(hc_colors_vec[levels(factor(plot_data_long$Policy))])) %>%
    hc_xAxis(title = list(text = ""), categories = unique(plot_data_long$admin_name)) %>%
    hc_yAxis(title = list(text = "")) %>%
    hc_plotOptions(
      bar = list(
        grouping = TRUE,
        groupPadding = 0.1,
        pointPadding = 0.05
      )
    ) %>%
    hc_legend(enabled = TRUE) %>%
    hc_chart(backgroundColor = 'white') %>%
    hc_caption(
      text = "Data source: Malawi Fifth Integrated Household Survey 2019-2020",
      style = list(fontSize = "8px", color = "black")
    )
  return(hc)
}