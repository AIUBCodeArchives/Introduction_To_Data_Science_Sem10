# ============================================================
# Air Pollution Intelligence: Global AQI Analysis and Prediction
# Structured and Refactored Version
# ============================================================

# Part 1: Required Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ranger)
library(xgboost)
library(caret)
library(forecast)
library(lubridate)
library(scales)

# ============================================================
# Function Definitions
# ============================================================

#' Fetch data from OpenAQ API for a specific country
#' @param api_key OpenAQ API Key
#' @param country_id ID of the country (e.g., 128 for Bangladesh)
#' @return A data.frame containing raw air quality measurements
fetch_openaq_data <- function(api_key, country_id = 128) {
  # Get locations
  url_loc <- paste0("https://api.openaq.org/v3/locations?limit=100&countries_id=", country_id)
  res_loc <- GET(url_loc, add_headers("X-API-Key" = api_key))
  loc_data <- fromJSON(content(res_loc, "text", encoding = "UTF-8"), flatten = TRUE)
  locations <- loc_data$results
  cat("\nTotal locations found:", nrow(locations), "\n")
  
  # Get all sensor IDs from all locations
  all_sensors <- list()
  for (i in 1:nrow(locations)) {
    loc_id   <- locations$id[i]
    loc_name <- locations$name[i]
    
    url_loc_detail <- paste0("https://api.openaq.org/v3/locations/", loc_id)
    res_detail     <- GET(url_loc_detail, add_headers("X-API-Key" = api_key))
    detail_data    <- fromJSON(content(res_detail, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (!is.null(detail_data$results) && nrow(detail_data$results) > 0) {
      sensors_raw <- detail_data$results$sensors[[1]]
      
      date_from <- detail_data$results$datetimeFirst.utc[1]
      date_to   <- detail_data$results$datetimeLast.utc[1]
      
      if (is.null(date_from) || is.na(date_from) || date_from == "") date_from <- "2015-01-01T00:00:00Z"
      if (is.null(date_to) || is.na(date_to) || date_to == "") date_to <- "2026-01-01T00:00:00Z"
      
      if (!is.null(sensors_raw) && length(sensors_raw) > 0) {
        sensor_ids <- sensors_raw$id
        for (sid in sensor_ids) {
          all_sensors[[length(all_sensors) + 1]] <- data.frame(
            sensor_id     = sid,
            location_id   = loc_id,
            location_name = loc_name,
            date_from     = as.character(date_from),
            date_to       = as.character(date_to),
            stringsAsFactors = FALSE
          )
        }
        cat("Location", i, "-", loc_name, "- sensors:", length(sensor_ids), "\n")
      } else {
        cat("Location", i, "-", loc_name, "- no sensors\n")
      }
    }
    Sys.sleep(1) # Rate limiting
  }
  
  sensors_df <- bind_rows(all_sensors)
  cat("\nTotal sensors found:", nrow(sensors_df), "\n")
  
  # Collect measurements from each sensor
  all_rows <- list()
  for (i in 1:nrow(sensors_df)) {
    sid       <- sensors_df$sensor_id[i]
    loc_name  <- sensors_df$location_name[i]
    date_from <- sensors_df$date_from[i]
    date_to   <- sensors_df$date_to[i]
    
    url_meas <- paste0(
      "https://api.openaq.org/v3/sensors/", sid, "/measurements",
      "?datetime_from=", date_from,
      "&datetime_to=",   date_to,
      "&limit=100"
    )
    
    res      <- GET(url_meas, add_headers("X-API-Key" = api_key))
    raw_text <- content(res, "text", encoding = "UTF-8")
    
    tryCatch({
      data <- fromJSON(raw_text, flatten = TRUE)
      if (!is.null(data$results) && length(data$results) > 0 && nrow(data$results) > 0) {
        df <- data$results
        df$sensor_id     <- sid
        df$location_name <- loc_name
        df$country       <- "Bangladesh"
        all_rows[[i]]    <- df
        cat("Sensor", sid, "-", loc_name, "- rows:", nrow(df), "\n")
      } else {
        cat("Sensor", sid, "-", loc_name, "- no data\n")
      }
    }, error = function(e) {
      cat("Sensor", sid, "- ERROR:", conditionMessage(e), "\n")
    })
    
    Sys.sleep(1) # Rate limiting
  }
  
  air_quality_raw <- bind_rows(all_rows)
  cat("\nTotal rows collected (raw):", nrow(air_quality_raw), "\n")
  
  return(air_quality_raw)
}

#' Perform Missing Value Audit
#' @param df The raw dataframe
audit_missing_values <- function(df) {
  na_counts    <- sapply(df, function(x) sum(is.na(x)))
  blank_counts <- sapply(df, function(x) sum(str_trim(ifelse(is.na(x), "", as.character(x))) == ""))
  
  missing_audit <- data.frame(
    Column            = names(na_counts),
    NA_Count          = as.integer(na_counts),
    BlankString_Count = as.integer(blank_counts),
    TotalMissing      = as.integer(na_counts + blank_counts),
    row.names         = NULL
  )
  
  cat("\n--- Missing Value Audit (NA + Blank Strings) ---\n")
  print(missing_audit)
}

#' Clean and engineer features for Air Quality Data
#' @param df The raw dataframe
#' @return A cleaned and transformed dataframe
clean_data <- function(df) {
  rows_before <- nrow(df)
  
  clean_air <- df %>%
    filter(!is.na(value)) %>%
    filter(!is.na(location_name)) %>%
    filter(!is.na(parameter.name)) %>%
    mutate(
      datetime_clean = as.POSIXct(period.datetimeFrom.utc,
                                  format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      Year      = as.integer(format(datetime_clean, "%Y")),
      Month     = format(datetime_clean, "%b"),
      Pollutant = str_to_upper(str_trim(parameter.name)),
      AQI_Category = case_when(
        Pollutant == "PM25" & value <= 12.0  ~ "Good",
        Pollutant == "PM25" & value <= 35.4  ~ "Moderate",
        Pollutant == "PM25" & value <= 55.4  ~ "Unhealthy for Sensitive Groups",
        Pollutant == "PM25" & value <= 150.4 ~ "Unhealthy",
        Pollutant == "PM25" & value >  150.4 ~ "Hazardous",
        TRUE ~ "Other"
      )
    ) %>%
    filter(!is.na(Year)) %>%
    distinct() %>%
    mutate(RowID = row_number()) %>%
    relocate(RowID)
  
  rows_after   <- nrow(clean_air)
  rows_removed <- rows_before - rows_after
  
  cat("\nRows before cleaning:", rows_before)
  cat("\nRows after cleaning :", rows_after)
  cat("\nRows removed        :", rows_removed, "\n")
  
  return(clean_air)
}

#' Perform Exploratory Data Analysis and save plots
#' @param clean_df Cleaned dataset
#' @param output_dir Directory to save the plots
perform_eda <- function(clean_df, output_dir) {
  th  <- theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"))
  pm25 <- clean_df %>% filter(Pollutant == "PM25", value >= 0, value <= 500)
  aqi_cols <- c("Good"="#2dc653","Moderate"="#f4d03f",
                "Unhealthy for Sensitive Groups"="#f0a500",
                "Unhealthy"="#e74c3c","Hazardous"="#8e44ad")
  mon_ord <- c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")

  # Plot 1: BAR CHART — Top 10 Pollutants
  top_poll <- clean_df %>% count(Pollutant, sort = TRUE) %>% slice_head(n = 10)
  p1 <- ggplot(top_poll, aes(x = reorder(Pollutant, n), y = n, fill = n)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.5) +
    scale_fill_gradient(low = "#f9c74f", high = "#e63946") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() + th +
    labs(title = "Bar Chart: Top 10 Pollutants by Measurement Count",
         x = "Pollutant", y = "Measurements")
  ggsave(file.path(output_dir, "plot1_bar_pollutants.png"), p1, width = 10, height = 6, dpi = 300)

  # Plot 2: HISTOGRAM + density overlay — PM2.5
  p2 <- ggplot(pm25, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40,
                   fill = "#4cc9f0", color = "white", alpha = 0.8) +
    geom_density(color = "#e63946", linewidth = 1) +
    geom_vline(xintercept = 15, linetype = "dashed", color = "black") +
    annotate("text", x = 17, y = Inf, label = "WHO Limit", vjust = 2, hjust = 0, size = 3.5) +
    th + labs(title = "Histogram: PM2.5 Distribution with Density Overlay",
              x = expression("PM2.5 (\u00b5g/m\u00b3)"), y = "Density")
  ggsave(file.path(output_dir, "plot2_histogram_pm25.png"), p2, width = 10, height = 6, dpi = 300)

  # Plot 3: DENSITY PLOT — PM2.5 by AQI Category
  pm25_cat <- pm25 %>% filter(AQI_Category != "Other")
  p3 <- ggplot(pm25_cat, aes(x = value, fill = AQI_Category, color = AQI_Category)) +
    geom_density(alpha = 0.35, linewidth = 0.9) +
    scale_fill_manual(values = aqi_cols) +
    scale_color_manual(values = aqi_cols) + th +
    labs(title = "Density Plot: PM2.5 Distribution by AQI Category",
         x = expression("PM2.5 (\u00b5g/m\u00b3)"), y = "Density",
         fill = "AQI", color = "AQI")
  ggsave(file.path(output_dir, "plot3_density_aqi.png"), p3, width = 11, height = 6, dpi = 300)

  # Plot 4: BOX PLOT — PM2.5 by Month
  p4 <- ggplot(pm25 %>% mutate(Month = factor(Month, levels = mon_ord)),
               aes(x = Month, y = value, fill = Month)) +
    geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "red") +
    scale_fill_brewer(palette = "Set3") + th +
    labs(title = "Box Plot: PM2.5 Concentration by Month (Seasonal Pattern)",
         x = "Month", y = expression("PM2.5 (\u00b5g/m\u00b3)"))
  ggsave(file.path(output_dir, "plot4_boxplot_month.png"), p4, width = 12, height = 6, dpi = 300)

  # Plot 5: VIOLIN PLOT — PM2.5 by Top 8 Locations
  top8 <- pm25 %>% count(location_name, sort = TRUE) %>%
    slice_head(n = 8) %>% pull(location_name)
  p5 <- ggplot(pm25 %>% filter(location_name %in% top8),
               aes(x = reorder(location_name, value, FUN = median),
                   y = value, fill = location_name)) +
    geom_violin(trim = TRUE, alpha = 0.7, show.legend = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    coord_flip() + scale_fill_brewer(palette = "Paired") + th +
    labs(title = "Violin Plot: PM2.5 Distribution by Top 8 Locations",
         x = "Location", y = expression("PM2.5 (\u00b5g/m\u00b3)"))
  ggsave(file.path(output_dir, "plot5_violin_location.png"), p5, width = 12, height = 7, dpi = 300)

  # Plot 6: SCATTER PLOT — PM2.5 readings over time
  samp <- pm25 %>% arrange(datetime_clean) %>% slice_sample(n = min(2000, nrow(pm25)))
  p6 <- ggplot(samp, aes(x = datetime_clean, y = value, color = AQI_Category)) +
    geom_point(alpha = 0.4, size = 1.2) +
    geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "red") +
    scale_color_manual(values = aqi_cols) +
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
    th + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Scatter Plot: PM2.5 Readings Over Time",
         x = "Date", y = expression("PM2.5 (\u00b5g/m\u00b3)"), color = "AQI")
  ggsave(file.path(output_dir, "plot6_scatter_time.png"), p6, width = 13, height = 6, dpi = 300)

  # Plot 7: STACKED BAR — AQI proportion by Month
  aqi_mon <- pm25_cat %>%
    mutate(Month = factor(Month, levels = mon_ord)) %>%
    count(Month, AQI_Category) %>%
    group_by(Month) %>% mutate(pct = n / sum(n))
  p7 <- ggplot(aqi_mon, aes(x = Month, y = pct, fill = AQI_Category)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = aqi_cols) + th +
    labs(title = "Stacked Bar: AQI Category Proportion by Month",
         x = "Month", y = "Proportion", fill = "AQI Category")
  ggsave(file.path(output_dir, "plot7_stacked_bar_aqi.png"), p7, width = 12, height = 6, dpi = 300)

  # Plot 8: HEATMAP — Location x Pollutant
  top10l <- clean_df %>% count(location_name, sort = TRUE) %>%
    slice_head(n = 10) %>% pull(location_name)
  hm <- clean_df %>%
    filter(Pollutant %in% top_poll$Pollutant, location_name %in% top10l) %>%
    count(location_name, Pollutant)
  p8 <- ggplot(hm, aes(x = Pollutant, y = location_name, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = comma(n)), size = 3, color = "white", fontface = "bold") +
    scale_fill_gradient(low = "#023e8a", high = "#e63946", labels = comma) +
    th + theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(title = "Heatmap: Measurement Count — Location x Pollutant",
         x = "Pollutant", y = "Location", fill = "Count")
  ggsave(file.path(output_dir, "plot8_heatmap.png"), p8, width = 13, height = 7, dpi = 300)

  # Plot 9: BAR CHART — Top 15 Locations
  top15l <- clean_df %>% count(location_name, sort = TRUE) %>% slice_head(n = 15)
  p9 <- ggplot(top15l, aes(x = reorder(location_name, n), y = n, fill = n)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.5) +
    scale_fill_gradient(low = "#74c69d", high = "#1b4332") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() + th +
    labs(title = "Bar Chart: Top 15 Locations by Measurement Count",
         x = "Location", y = "Measurements")
  ggsave(file.path(output_dir, "plot9_bar_locations.png"), p9, width = 12, height = 7, dpi = 300)

  cat("\n9 EDA plots saved to", output_dir, "\n")
}

#' Train Classification Model (Random Forest)
#' @param clean_df Cleaned dataset
#' @param output_dir Directory to save the plots
#' @return The trained model object and its accuracy
train_rf_model <- function(clean_df, output_dir) {
  clf_data <- clean_df %>%
    filter(Pollutant == "PM25") %>%
    filter(AQI_Category != "Other") %>%
    mutate(
      AQI_Category  = factor(AQI_Category),
      location_name = factor(location_name),
      Month         = factor(Month)
    ) %>%
    select(AQI_Category, location_name, Month, Year)
  
  cat("\nRandom Forest Class distribution:\n")
  print(table(clf_data$AQI_Category))
  
  set.seed(42)
  train_idx <- sample(seq_len(nrow(clf_data)), size = 0.8 * nrow(clf_data))
  train_df  <- clf_data[train_idx, ]
  test_df   <- clf_data[-train_idx, ]
  
  set.seed(42)
  rf_model <- ranger(
    formula     = AQI_Category ~ location_name + Month + Year,
    data        = train_df,
    num.trees   = 500,
    importance  = "impurity",
    probability = TRUE
  )
  
  pred_prob  <- predict(rf_model, data = test_df)$predictions
  pred_class <- colnames(pred_prob)[max.col(pred_prob)]
  pred_class <- factor(pred_class, levels = levels(test_df$AQI_Category))
  
  accuracy <- mean(pred_class == test_df$AQI_Category)
  cat("\nRanger Random Forest Accuracy:", round(accuracy, 4), "\n")
  
  cat("\nConfusion Matrix:\n")
  print(table(Predicted = pred_class, Actual = test_df$AQI_Category))
  
  # Plot 10: BAR CHART — Random Forest Feature Importance
  imp    <- sort(rf_model$variable.importance, decreasing = TRUE)
  imp_df <- data.frame(Feature = names(imp), Importance = as.numeric(imp))
  p10 <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = round(Importance, 1)), hjust = -0.1, size = 4) +
    scale_fill_gradient(low = "#c77dff", high = "#5a189a") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() + theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold")) +
    labs(title = "Bar Chart: Random Forest Feature Importance",
         x = "Feature", y = "Mean Decrease in Impurity")
  ggsave(file.path(output_dir, "plot10_bar_rf_importance.png"), p10, width = 10, height = 6, dpi = 300)
  
  return(list(model = rf_model, accuracy = accuracy))
}

#' Perform ANOVA and Tukey HSD on PM2.5 data
#' @param clean_df Cleaned dataset
#' @param output_dir Directory to save the plots
perform_anova_tukey <- function(clean_df, output_dir) {
  anova_data <- clean_df %>%
    filter(Pollutant == "PM25", value >= 0, value <= 500)
  
  # One-Way ANOVA: Month
  anova_month <- aov(value ~ Month, data = anova_data)
  cat("\n--- One-Way ANOVA: PM2.5 ~ Month ---\n")
  print(summary(anova_month))
  
  # One-Way ANOVA: Location
  anova_location <- aov(value ~ location_name, data = anova_data)
  cat("\n--- One-Way ANOVA: PM2.5 ~ Location ---\n")
  print(summary(anova_location))
  
  # Tukey HSD Post-hoc: Month
  cat("\n--- Tukey HSD: Month Pairs ---\n")
  tukey_month <- TukeyHSD(anova_month)
  print(tukey_month)
  
  # Plot 11: BOX PLOT — PM2.5 by Location (ANOVA groups visualized)
  top6_locs <- anova_data %>% count(location_name, sort = TRUE) %>%
    slice_head(n = 6) %>% pull(location_name)
  p11 <- ggplot(anova_data %>% filter(location_name %in% top6_locs),
                aes(x = reorder(location_name, value, FUN = median),
                    y = value, fill = location_name)) +
    geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
    geom_jitter(width = 0.15, alpha = 0.15, size = 0.8, color = "gray30") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "red") +
    coord_flip() + scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold")) +
    labs(title = "Box Plot: PM2.5 by Top Locations (ANOVA Groups)",
         x = "Location", y = expression("PM2.5 (\u00b5g/m\u00b3)"))
  ggsave(file.path(output_dir, "plot11_boxplot_anova_location.png"), p11, width = 12, height = 6, dpi = 300)

  # Plot 12: POINTRANGE — Tukey HSD pairwise differences by Month
  tukey_df <- as.data.frame(tukey_month$Month)
  tukey_df$pair <- rownames(tukey_df)
  p12 <- ggplot(tukey_df, aes(
    x = reorder(pair, diff), y = diff, ymin = lwr, ymax = upr,
    color = ifelse(lwr > 0 | upr < 0, "Significant", "Not Significant")
  )) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_color_manual(values = c("Significant" = "red", "Not Significant" = "gray50")) +
    theme_minimal() +
    labs(title = "Tukey HSD: Pairwise PM2.5 Differences by Month",
         x = "Month Pair", y = "Mean Difference (µg/m³)", color = "Significance")
  print(p12)
  ggsave(file.path(output_dir, "plot12_tukey_hsd_month.png"), p12, width = 14, height = 8, dpi = 300)
}

#' Train XGBoost Classification Model
#' @param clean_df Cleaned dataset
#' @param output_dir Directory to save the plots
train_xgboost_model <- function(clean_df, output_dir) {
  xgb_data <- clean_df %>%
    filter(Pollutant == "PM25", AQI_Category != "Other") %>%
    mutate(
      location_enc = as.integer(factor(location_name)),
      month_enc    = as.integer(factor(Month)),
      label        = as.integer(factor(AQI_Category)) - 1  # XGBoost needs 0-indexed
    ) %>%
    select(label, location_enc, month_enc, Year)
  
  cat("\nXGBoost Class distribution:\n")
  print(table(xgb_data$label))
  
  set.seed(42)
  train_idx_xgb <- sample(seq_len(nrow(xgb_data)), size = 0.8 * nrow(xgb_data))
  train_xgb <- xgb_data[train_idx_xgb, ]
  test_xgb  <- xgb_data[-train_idx_xgb, ]
  
  x_train <- as.matrix(train_xgb %>% select(-label))
  y_train <- train_xgb$label
  x_test  <- as.matrix(test_xgb %>% select(-label))
  y_test  <- test_xgb$label
  
  num_classes <- length(unique(xgb_data$label))
  
  dtrain <- xgb.DMatrix(data = x_train, label = y_train)
  dtest  <- xgb.DMatrix(data = x_test, label = y_test)
  
  params <- list(
    objective = "multi:softmax",
    num_class = num_classes,
    max_depth = 6,
    learning_rate = 0.1
  )
  
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100
  )
  
  xgb_pred <- predict(xgb_model, x_test)
  xgb_accuracy <- mean(xgb_pred == y_test)
  cat("\nXGBoost Accuracy:", round(xgb_accuracy, 4), "\n")
  
  cat("\nConfusion Matrix (XGBoost):\n")
  print(table(Predicted = xgb_pred, Actual = y_test))
  
  # Feature Importance
  imp_xgb <- xgb.importance(
    feature_names = colnames(x_train),
    model = xgb_model
  )
  cat("\nXGBoost Feature Importance:\n")
  print(imp_xgb)
  
  # Plot 13: BAR CHART — XGBoost Feature Importance
  p13 <- ggplot(imp_xgb, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = round(Gain, 3)), hjust = -0.1, size = 4) +
    scale_fill_gradient(low = "#ffd166", high = "#ef233c") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() + theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold")) +
    labs(title = "Bar Chart: XGBoost Feature Importance (Gain)",
         x = "Feature", y = "Gain")
  print(p13)
  ggsave(file.path(output_dir, "plot13_bar_xgb_importance.png"), p13, width = 10, height = 6, dpi = 300)
  
  return(list(model = xgb_model, accuracy = xgb_accuracy))
}

#' Perform ARIMA Forecasting for PM2.5
#' @param clean_df Cleaned dataset
#' @param output_dir Directory to save the plots
perform_arima_forecast <- function(clean_df, output_dir) {
  # Aggregate monthly average PM2.5
  arima_data <- clean_df %>%
    filter(Pollutant == "PM25", value >= 0, value <= 500) %>%
    mutate(YearMonth = as.Date(format(datetime_clean, "%Y-%m-01"))) %>%
    group_by(YearMonth) %>%
    summarise(avg_pm25 = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(YearMonth)
  
  cat("\nARIMA data points:", nrow(arima_data), "\n")
  
  # Convert to time series
  ts_pm25 <- ts(
    arima_data$avg_pm25,
    start     = c(as.integer(format(min(arima_data$YearMonth), "%Y")),
                  as.integer(format(min(arima_data$YearMonth), "%m"))),
    frequency = 12
  )
  
  # Fit ARIMA (auto selects best p,d,q)
  arima_model <- auto.arima(ts_pm25, seasonal = TRUE)
  cat("\nARIMA Model Summary:\n")
  print(summary(arima_model))
  
  # Forecast 24 months ahead
  arima_forecast <- forecast(arima_model, h = 24)
  cat("\nARIMA Forecast (next 24 months):\n")
  print(arima_forecast)
  
  # Plot forecast
  forecast_df <- data.frame(
    Date  = seq(max(arima_data$YearMonth) %m+% months(1),
                by = "month", length.out = 24),
    Mean  = as.numeric(arima_forecast$mean),
    Lo80  = as.numeric(arima_forecast$lower[, 1]),
    Hi80  = as.numeric(arima_forecast$upper[, 1]),
    Lo95  = as.numeric(arima_forecast$lower[, 2]),
    Hi95  = as.numeric(arima_forecast$upper[, 2])
  )
  
  p_arima <- ggplot() +
    geom_line(data = arima_data, aes(x = YearMonth, y = avg_pm25),
              color = "steelblue", linewidth = 0.9) +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo95, ymax = Hi95),
                fill = "orange", alpha = 0.2) +
    geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo80, ymax = Hi80),
                fill = "orange", alpha = 0.35) +
    geom_line(data = forecast_df, aes(x = Date, y = Mean),
              color = "red", linewidth = 1, linetype = "dashed") +
    geom_hline(yintercept = 15, linetype = "dotted", color = "black") +
    annotate("text", x = min(arima_data$YearMonth), y = 17,
             label = "WHO Limit (15)", size = 3, hjust = 0) +
    labs(
      title = "PM2.5 Forecast - Bangladesh (ARIMA, 24 months ahead)",
      subtitle = "Blue = historical | Red dashed = forecast | Orange = confidence interval",
      x = "Date", y = expression("Avg PM2.5 (µg/m³)")
    ) +
    theme_minimal()
  
  print(p_arima)
  ggsave(file.path(output_dir, "plot9_arima_forecast.png"), plot = p_arima, width = 14, height = 7, dpi = 300)
}

# ============================================================
# Main Execution Block
# ============================================================
main <- function() {
  # --- Setup ---
  API_KEY <- "50ba0606df2aaa50a02b98aff75fe0a5e7916612e66ec2af030b6d345b6d15fe"
  OUTPUT_DIR <- getwd()  # Change if needed, originally: "D:/AIUB Notes/Introduction to Data Science/final-project/v2"
  
  cat("Starting Air Quality Analysis Pipeline...\n")
  
  # 1. Fetch Data
  cat("\n[Step 1] Fetching Data from OpenAQ...\n")
  raw_data <- fetch_openaq_data(api_key = API_KEY, country_id = 128)
  
  # 2. Audit Missing Values
  cat("\n[Step 2] Auditing Missing Values...\n")
  audit_missing_values(raw_data)
  
  # 3. Clean and Engineer Features
  cat("\n[Step 3] Cleaning Data...\n")
  clean_data_df <- clean_data(raw_data)
  
  # 4. Perform EDA
  cat("\n[Step 4] Performing Exploratory Data Analysis...\n")
  perform_eda(clean_data_df, OUTPUT_DIR)
  
  # 5. Train Random Forest Model
  cat("\n[Step 5] Training Random Forest Classification Model...\n")
  rf_results <- train_rf_model(clean_data_df, OUTPUT_DIR)
  
  # 6. Perform ANOVA & Tukey
  cat("\n[Step 6] Running ANOVA and Tukey HSD Analysis...\n")
  perform_anova_tukey(clean_data_df, OUTPUT_DIR)
  
  # 7. Train XGBoost Model
  cat("\n[Step 7] Training XGBoost Classification Model...\n")
  xgb_results <- train_xgboost_model(clean_data_df, OUTPUT_DIR)
  
  cat("\nModel Comparison:\n")
  cat("Random Forest Accuracy: ", round(rf_results$accuracy, 4), "\n")
  cat("XGBoost Accuracy:       ", round(xgb_results$accuracy, 4), "\n")
  
  # 8. Perform ARIMA Forecasting
  cat("\n[Step 8] Running ARIMA Forecasting...\n")
  perform_arima_forecast(clean_data_df, OUTPUT_DIR)
  
  cat("\nPipeline Execution Complete!\n")
}

# Run the pipeline if script is executed directly
main()
