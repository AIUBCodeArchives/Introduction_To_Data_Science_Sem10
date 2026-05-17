# ============================================================
# Air Pollution Intelligence: Asia-Wide AQI Analysis
# ============================================================

# Part 1: Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ranger)

setwd("D:/AIUB Notes/Introduction to Data Science/final-project/asia-type/")

# ============================================================
# Part 2: Asian Countries (OpenAQ country IDs)
# Top 10 by OpenAQ coverage
# ============================================================

# NEW — top 10 by OpenAQ data coverage
asian_countries <- data.frame(
  country_id   = c(128, 100, 197, 203, 116, 114, 64, 179, 141, 92),
  country_name = c("Bangladesh", "India", "Taiwan", "Thailand", "Japan",
                   "Indonesia", "China", "Singapore", "Malaysia", "Hong Kong"),
  stringsAsFactors = FALSE
)

api_key <- "50ba0606df2aaa50a02b98aff75fe0a5e7916612e66ec2af030b6d345b6d15fe"

# ============================================================
# Part 3: Data Collection (loop over all countries)
# ============================================================

all_rows <- list()

for (ci in 1:nrow(asian_countries)) {
  
  country_id   <- asian_countries$country_id[ci]
  country_name <- asian_countries$country_name[ci]
  cat("\n====", country_name, "====\n")
  
  # Step A: Get locations
  url_loc  <- paste0("https://api.openaq.org/v3/locations?limit=100&countries_id=", country_id)
  
  locations <- tryCatch({
    res_loc  <- GET(url_loc, add_headers("X-API-Key" = api_key))
    loc_data <- fromJSON(content(res_loc, "text", encoding = "UTF-8"), flatten = TRUE)
    loc_data$results
  }, error = function(e) {
    cat("ERROR fetching locations for", country_name, ":", conditionMessage(e), "\n")
    NULL
  })
  
  if (is.null(locations) || length(locations) == 0 ||
      !is.data.frame(locations) || nrow(locations) == 0) {
    cat("Skipping", country_name, "- no valid locations\n")
    next
  }
  cat("Locations:", nrow(locations), "\n")
  
  # Step B: Collect sensors
  country_sensors <- list()
  
  for (i in 1:nrow(locations)) {
    loc_id   <- locations$id[i]
    loc_name <- locations$name[i]
    
    url_detail  <- paste0("https://api.openaq.org/v3/locations/", loc_id)
    res_detail  <- GET(url_detail, add_headers("X-API-Key" = api_key))
    detail_data <- fromJSON(content(res_detail, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (!is.null(detail_data$results) && nrow(detail_data$results) > 0) {
      sensors_raw <- detail_data$results$sensors[[1]]
      date_from   <- detail_data$results$datetimeFirst.utc[1]
      date_to     <- detail_data$results$datetimeLast.utc[1]
      
      if (is.null(date_from) || is.na(date_from) || date_from == "") date_from <- "2015-01-01T00:00:00Z"
      if (is.null(date_to)   || is.na(date_to)   || date_to   == "") date_to   <- "2026-01-01T00:00:00Z"
      
    
      if (!is.null(sensors_raw) && is.data.frame(sensors_raw) && 
          nrow(sensors_raw) > 0 && "id" %in% names(sensors_raw)) {
          for (sid in sensors_raw$id) {
            country_sensors[[length(country_sensors) + 1]] <- data.frame(
              sensor_id     = sid,
              location_name = loc_name,
              country       = country_name,
              date_from     = as.character(date_from),
              date_to       = as.character(date_to),
              stringsAsFactors = FALSE
            )
          }
      }
    }
    Sys.sleep(0.8)
  }
  
  if (length(country_sensors) == 0) {
    cat("No sensors found for", country_name, "\n")
    next
  }
  
  sensors_df <- bind_rows(country_sensors)
  cat("Sensors:", nrow(sensors_df), "\n")
  
  # Step C: Fetch measurements (cap at 50 sensors per country for speed)
  sensors_df <- sensors_df %>% slice_head(n = 50)
  
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
      if (!is.null(data$results) && nrow(data$results) > 0) {
        df               <- data$results
        df$sensor_id     <- sid
        df$location_name <- loc_name
        df$country       <- country_name
        all_rows[[length(all_rows) + 1]] <- df
      }
    }, error = function(e) {
      cat("Sensor", sid, "ERROR:", conditionMessage(e), "\n")
    })
    
    Sys.sleep(0.8)
  }
  
  cat("Rows so far:", length(all_rows), "chunks\n")
}

air_quality_raw <- bind_rows(all_rows)
cat("\nTotal raw rows:", nrow(air_quality_raw), "\n")

# ============================================================
# Part 4: Missing Value Audit
# ============================================================

rows_before  <- nrow(air_quality_raw)
na_counts    <- sapply(air_quality_raw, function(x) sum(is.na(x)))
blank_counts <- sapply(air_quality_raw, function(x) sum(str_trim(ifelse(is.na(x), "", as.character(x))) == ""))

missing_audit <- data.frame(
  Column            = names(na_counts),
  NA_Count          = as.integer(na_counts),
  BlankString_Count = as.integer(blank_counts),
  TotalMissing      = as.integer(na_counts + blank_counts),
  row.names         = NULL
)

cat("\n--- Missing Value Audit ---\n")
print(missing_audit)

# ============================================================
# Part 5: Cleaning & Feature Engineering
# ============================================================

clean_air <- air_quality_raw %>%
  filter(!is.na(value), !is.na(location_name), !is.na(parameter.name)) %>%
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
cat("\nRows before:", rows_before, "| After:", rows_after, "| Removed:", rows_removed, "\n")

# ============================================================
# Part 6: EDA - Pollutant Distribution (Asia-wide)
# ============================================================

top_pollutants <- clean_air %>%
  count(Pollutant, sort = TRUE) %>%
  slice_head(n = 10)

p1 <- ggplot(top_pollutants, aes(x = reorder(Pollutant, n), y = n)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = n), hjust = -0.1, size = 4) +
  coord_flip() +
  expand_limits(y = max(top_pollutants$n) * 1.15) +
  labs(title = "Top 10 Pollutants by Measurement Count (Asia)",
       x = "Pollutant", y = "Count")
print(p1)
ggsave("plot1_pollutant_distribution.png", plot = p1, width = 10, height = 6, dpi = 300)

# ============================================================
# Part 7: EDA - Temporal Distribution
# ============================================================

yearly_all <- clean_air %>%
  count(Year, name = "MeasurementCount") %>%
  arrange(Year)

p2 <- ggplot(yearly_all, aes(x = MeasurementCount)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Histogram: Distribution of Yearly Measurement Counts (Asia)",
       x = "Measurement Count per Year", y = "Number of Years")
print(p2)
ggsave("plot2_yearly_histogram.png", plot = p2, width = 10, height = 6, dpi = 300)

# ============================================================
# Part 8: EDA - Trend Over Time
# ============================================================

p3 <- ggplot(yearly_all, aes(x = Year, y = MeasurementCount)) +
  geom_point(color = "darkorange", size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_log10() +
  labs(title = "Scatter Plot: Year vs Measurement Count - Asia (Log Y)",
       x = "Year", y = "Measurement Count (log scale)")
print(p3)
ggsave("plot3_trend_over_time.png", plot = p3, width = 10, height = 6, dpi = 300)

# ============================================================
# Part 9: EDA - Location Distribution
# ============================================================

top_locations <- clean_air %>%
  count(location_name, sort = TRUE) %>%
  slice_head(n = 15)

p4 <- ggplot(top_locations, aes(x = reorder(location_name, n), y = n)) +
  geom_col(fill = "seagreen") +
  geom_text(aes(label = n), hjust = -0.1, size = 4) +
  coord_flip() +
  expand_limits(y = max(top_locations$n) * 1.15) +
  labs(title = "Top 15 Locations by Measurement Count (Asia)",
       x = "Location", y = "Count")
print(p4)
ggsave("plot4_location_distribution.png", plot = p4, width = 12, height = 7, dpi = 300)

# ============================================================
# Part 10: EDA - Heatmap Location x Pollutant
# ============================================================

top10_pollutants <- clean_air %>% count(Pollutant, sort = TRUE) %>% slice_head(n = 10) %>% pull(Pollutant)
top10_locations  <- clean_air %>% count(location_name, sort = TRUE) %>% slice_head(n = 10) %>% pull(location_name)

heatmap_data <- clean_air %>%
  filter(Pollutant %in% top10_pollutants, location_name %in% top10_locations) %>%
  count(location_name, Pollutant, name = "n")

p5 <- ggplot(heatmap_data, aes(x = Pollutant, y = location_name, fill = n)) +
  geom_tile(color = "white") +
  labs(title = "Heatmap: Location x Pollutant (Asia)",
       x = "Pollutant", y = "Location") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(p5)
ggsave("plot5_heatmap.png", plot = p5, width = 12, height = 7, dpi = 300)

# ============================================================
# Part 11: COMPARISON PLOT A - Avg PM2.5 by Country (Bar Chart)
# ============================================================

pm25_by_country <- clean_air %>%
  filter(Pollutant == "PM25") %>%
  group_by(country) %>%
  summarise(avg_pm25 = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_pm25))

p6 <- ggplot(pm25_by_country, aes(x = reorder(country, avg_pm25), y = avg_pm25, fill = avg_pm25)) +
  geom_col() +
  geom_text(aes(label = round(avg_pm25, 1)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "yellow", high = "red") +
  expand_limits(y = max(pm25_by_country$avg_pm25, na.rm = TRUE) * 1.15) +
  labs(title = "Average PM2.5 by Country (Asia)",
       x = "Country", y = expression("Avg PM2.5 (µg/m³)"),
       fill = expression("µg/m³")) +
  theme_minimal()
print(p6)
ggsave("plot6_avg_pm25_by_country.png", plot = p6, width = 12, height = 7, dpi = 300)

# ============================================================
# Part 12: COMPARISON PLOT B - Boxplot PM2.5 Distribution by Country
# ============================================================

pm25_box <- clean_air %>%
  filter(Pollutant == "PM25", value >= 0, value <= 500)

p7 <- ggplot(pm25_box, aes(x = reorder(country, value, FUN = median), y = value, fill = country)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 300)) +
  labs(title = "PM2.5 Distribution by Country (Asia)",
       x = "Country", y = expression("PM2.5 (µg/m³)")) +
  theme_minimal() +
  theme(legend.position = "none")
print(p7)
ggsave("plot7_boxplot_pm25_by_country.png", plot = p7, width = 12, height = 8, dpi = 300)

# ============================================================
# Part 13: COMPARISON PLOT C - Country Ranking (WHO Threshold Map)
# ============================================================

who_limit <- 15  # WHO annual PM2.5 guideline µg/m³

country_rank <- pm25_by_country %>%
  mutate(
    WHO_Status = ifelse(avg_pm25 > who_limit, "Exceeds WHO Limit", "Within WHO Limit"),
    rank       = rank(-avg_pm25)
  )

p8 <- ggplot(country_rank, aes(x = reorder(country, -avg_pm25), y = avg_pm25, fill = WHO_Status)) +
  geom_col() +
  geom_hline(yintercept = who_limit, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_text(aes(label = paste0("#", rank)), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Exceeds WHO Limit" = "#e63946", "Within WHO Limit" = "#2a9d8f")) +
  labs(title = "Country PM2.5 Ranking vs WHO Guideline (15 µg/m³)",
       subtitle = "Dashed line = WHO annual PM2.5 limit",
       x = "Country", y = expression("Avg PM2.5 (µg/m³)"),
       fill = "WHO Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
print(p8)
ggsave("plot8_country_ranking_who.png", plot = p8, width = 14, height = 7, dpi = 300)

# ============================================================
# Part 14: COMPARISON PLOT D - PM2.5 Trend Lines Per Country
# ============================================================

pm25_trend <- clean_air %>%
  filter(Pollutant == "PM25", value >= 0, value <= 500) %>%
  group_by(country, Year) %>%
  summarise(avg_pm25 = mean(value, na.rm = TRUE), .groups = "drop") %>%
  filter(Year >= 2015)

p9 <- ggplot(pm25_trend, aes(x = Year, y = avg_pm25, color = country, group = country)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = who_limit, linetype = "dashed", color = "black", linewidth = 0.7) +
  annotate("text", x = min(pm25_trend$Year), y = who_limit + 3,
           label = "WHO Limit (15)", size = 3, hjust = 0) +
  scale_color_viridis_d(option = "turbo") +
  labs(title = "PM2.5 Trend Over Years by Country (Asia)",
       x = "Year", y = expression("Avg PM2.5 (µg/m³)"),
       color = "Country") +
  theme_minimal()
print(p9)
ggsave("plot9_trend_per_country.png", plot = p9, width = 14, height = 7, dpi = 300)

# ============================================================
# Part 15: Classification Model (Random Forest)
# ============================================================

clf_data <- clean_air %>%
  filter(Pollutant == "PM25", AQI_Category != "Other") %>%
  mutate(
    AQI_Category  = factor(AQI_Category),
    location_name = factor(location_name),
    Month         = factor(Month),
    country       = factor(country)
  ) %>%
  select(AQI_Category, country, location_name, Month, Year)

cat("\nClass distribution:\n")
print(table(clf_data$AQI_Category))

set.seed(42)
train_idx <- sample(seq_len(nrow(clf_data)), size = 0.8 * nrow(clf_data))
train_df  <- clf_data[train_idx, ]
test_df   <- clf_data[-train_idx, ]

set.seed(42)
rf_model <- ranger(
  formula     = AQI_Category ~ country + location_name + Month + Year,
  data        = train_df,
  num.trees   = 500,
  importance  = "impurity",
  probability = TRUE
)

pred_prob  <- predict(rf_model, data = test_df)$predictions
pred_class <- colnames(pred_prob)[max.col(pred_prob)]
pred_class <- factor(pred_class, levels = levels(test_df$AQI_Category))

accuracy <- mean(pred_class == test_df$AQI_Category)
cat("\nRandom Forest Accuracy:", round(accuracy, 4), "\n")
cat("\nConfusion Matrix:\n")
print(table(Predicted = pred_class, Actual = test_df$AQI_Category))

imp    <- sort(rf_model$variable.importance, decreasing = TRUE)
imp_df <- data.frame(Feature = names(imp), Importance = as.numeric(imp))

p10 <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest - Asia)",
       x = "Feature", y = "Importance")
print(p10)
ggsave("plot10_feature_importance.png", plot = p10, width = 10, height = 6, dpi = 300)

cat("\nAll plots saved successfully.\n")