load("R/FIPS_df.RData")
load("R/covid.RData")

pull_peers <- function(df, add_info = F, subset_to_peers = T, geog = "", additional_geogs = ""){

  # If no geography provided, use MSA column. If no MSA column, use FIPS column.
  if (geog == ""){
    if ("MSA" %in% names(df)) geog <- "MSA"
    else if ("FIPS" %in% names(df)) geog <- "FIPS"
  }
  if(geog == "") stop("MSA and FIPS columns are missing from the data frame.")

  # Ensure the Brimingham FIPS code is five digits and that the MSA column is of type character
  if ("MSA" %in% names(df))  df %<>% mutate(MSA = MSA %>% as.character())
  if ("FIPS" %in% names(df)) df %<>% mutate(FIPS = FIPS %>% as.character %>% replace(. == "1073", "01073"))

  # Add information columns
  if (add_info) {
    if      ("MSA" %in% names(df))  df %<>% left_join(MSA_df,  by = "MSA")
    else if ("FIPS" %in% names(df)) df %<>% left_join(FIPS_df, by = "FIPS")
  }

  # subset to peers based on geog
  if(subset_to_peers) {
    if (geog == "FIPS") df %<>% filter(FIPS %in% c(FIPS_df$FIPS, additional_geogs))
    if (geog == "MSA") {
      if ("MSA" %in% names(df)) {
        df %<>% filter(MSA %in% c(MSA_FIPS$MSA, additional_geogs))
      } else {
        df %<>% filter(FIPS %in% c(MSA_FIPS$FIPS, additional_geogs))
      }
    }
    if (geog == "MSA_2012") {
      if ("MSA" %in% names(df)) {
        df %<>% filter(MSA %in% c(MSA_FIPS_2012$MSA, additional_geogs))
      } else {
        df %<>% filter(FIPS %in% c(MSA_FIPS_2012$FIPS, additional_geogs))
      }
    }
  }

  df
}

rollmeanr <- function(x, r){
  n <- length(x)
  y <- rep(NA, n)

  dif <- floor(r / 2)

  for(i in (1 + dif):(n - dif)){
    y[i] <- mean(x[(i - dif):(i + dif)])
  }

  y
}

reshape_daily_data <- function(df, var, other_grouping_vars = ""){

  df %<>%
    pull_peers(add_info = TRUE) %>%
    filter(current == 1)

  # Create a data frame with Louisville and
  #   a data frame without Louisville.
  df_wol <- df %>% filter(FIPS != 21111)
  lville <- df %>% filter(FIPS == 21111)

  #  Group the data set by year, category, if avilable.
  #  Calculate the 25th percentile, 75th percentile,
  #   and mean of the peer data set.
  df_wol %<>%
    group_by(across(c("year", other_grouping_vars))) %>%
    summarise(q1 = quantile(.data[[var]], prob = 0.25, na.rm = T, names = F),
              mean = mean(.data[[var]], na.rm = TRUE),
              q3 = quantile(.data[[var]], prob = 0.75, na.rm = T, names = F), .groups = "drop")

  # Lville data frame contains three columns
  lville %<>%
    select(c("year", other_grouping_vars, var)) %>%
    rename(lou = !!var)

  #join 25th percentile, 75th percentile, and Louisville values
  df <- full_join(lville, df_wol, by = c("year", other_grouping_vars))

  #Reshape the data to long format
  df %<>% pivot_longer(lou:q3, names_to = "variable", values_to = "value")

  df

}

data_fxn <- function(input) {

  variable <- recode(input$variable,
                     "Consumer Spending" = "consumer_spending",
                     "Small Business Revenue" = "small_business_revenue",
                     "Small Businesses Open" = "small_business_open",
                     "Time Spent Away from Home" = "time_away_from_home")

  rolling_mean <- if_else(input$rolling_mean, 1, 7)

  if(!input$peer_option) {
    covid_summary <- covid %>%
      pull_peers(add_info = TRUE) %>%
      filter(
        current == 1,
        !is.na(!!variable)) %>%
      pivot_wider(id_cols = date, names_from = city, values_from = variable) %>%
      mutate(across(Birmingham:`St. Louis`, ~ rollmeanr(., rolling_mean)))
  } else {
    covid_summary <- covid %>%
      reshape_daily_data(variable, other_grouping_vars = "date") %>%
      group_by(variable) %>%
      mutate(value = rollmeanr(value, rolling_mean)) %>%
      ungroup()

    covid_summary %<>%
      filter(!is.na(value)) %>%
      pivot_wider(id_cols = date, names_from = variable, values_from = value) %>%
      mutate(
        q1_copy = q1,
        q3_copy = q3)
  }

  #y_range = c(min(covid_summary$value, na.rm = TRUE), max(covid_summary$value, na.rm = TRUE))

  ts <- xts(x = select(covid_summary, -date), order.by = covid_summary$date)

  ts
}

graph_fxn <- function(ts, input) {

  variable <- recode(input$variable,
                     "Consumer Spending" = "consumer_spending",
                     "Small Business Revenue" = "small_business_revenue",
                     "Small Businesses Open" = "small_business_open",
                     "Time Spent Away from Home" = "time_away_from_home")

  output_graph <- dygraph(ts)

  if(!input$peer_option) {
    output_graph %<>%
      dySeries("Louisville", color = "#00a9b7", strokeWidth = 3) %>%
      dyGroup(setdiff(FIPS_df$city[FIPS_df$current == 1], "Louisville"),
              color = rep("#7f7f7f", 16))
  } else {
    output_graph %<>%
      dySeries("lou", label = "Louisville", color = "#00a9b7", strokeWidth = 3) %>%
      dySeries("q3", label = "75th Percentile", color = "#7f7f7f", strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries(c("q1_copy", "mean", "q3_copy"), label = "Peer Mean", color = "black", strokeWidth = 2) %>%
      dySeries("q1", label = "25th Percentile", color = "#7f7f7f", strokeWidth = 2, strokePattern = "dashed")

  }

  output_graph %<>%
    dyAxis("y",
           label = "Percent",
           valueFormatter = "function(v){return (v).toFixed(1) + '%'}",
           axisLabelFormatter = "function(v){return (v).toFixed(0) + '%'}",
           rangePad = 25) %>%
    dyHighlight(highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.3,
                highlightSeriesOpts = list(strokeWidth = 2),
                hideOnMouseOut = TRUE) %>%
    dyLimit(0, strokePattern = "solid") %>%
    dyCrosshair(direction = "vertical")

  if (input$peer_option) {
    output_graph %<>%
      dyLegend(showZeroValues = F, labelsSeparateLines = T)#, labelsDiv = "legendDivID")
  } else {
    output_graph %<>%
      dyLegend(showZeroValues = F)#, labelsDiv = "legendDivID")
  }


  label_positions <- case_when(
    variable %in% c("consumer_spending", "small_business_revenue", "small_business_open") ~
      c("bottom", "bottom", "top", "top", "top", "bottom"),
    variable == "time_away_from_home" ~ c("bottom", "bottom", "top", "top", "top", "bottom"))

  output_graph %<>%
    dyEvent(lubridate::ymd("2020-01-20"), color = "#7f7f7f",
            "First U.S. COVID-19 Case", label_positions[1]) %>%
    dyEvent(lubridate::ymd("2020-03-16"), color = "#7f7f7f",
            "KY Public Schools Close", label_positions[2]) %>%
    dyEvent(lubridate::ymd("2020-03-26"), color = "#7f7f7f",
            "KY Stay at Home Advisory", label_positions[3]) %>%
    dyEvent(lubridate::ymd("2020-04-15"), color = "#7f7f7f",
            "Stimulus Payments Start", label_positions[4]) %>%
    dyEvent(lubridate::ymd("2020-05-11"), color = "#7f7f7f",
            "KY Stay at Home Order Ends", label_positions[5]) %>%
    dyEvent(lubridate::ymd("2020-07-28"), color = "#7f7f7f",
            "KY Select Bus. Re-close", label_positions[6])

  output_graph %<>%
    dyCallbacks(highlightCallback = "function() { Shiny.setInputValue('hover_values', arguments); }")

  output_graph
}
