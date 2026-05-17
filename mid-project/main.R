# ============================================================
# Video Games Sales Analysis
# Adapted from Mid-term Project Structure
# Dataset: Video Games Sales (as at 22 Dec 2016)
# ============================================================

# Task-1: Setup Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(tidyr)

# Task-2: Load and Prepare Data
vg_data <- read_csv("Video_Games_Sales_as_at_22_Dec_2016_2.csv")

# Rename columns for cleaner access
new_colnames <- c("Name", "Platform", "Year", "Genre", "Publisher",
                  "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales",
                  "Global_Sales", "Critic_Score", "Critic_Count",
                  "User_Score", "User_Count", "Developer", "Rating")
colnames(vg_data) <- new_colnames

# Data Inspection
dim(vg_data)
head(vg_data)
str(vg_data)
summary(vg_data)


# Task-3: Identify Missing Values
cat("\n[Task 3] Checking for missing values...\n")
working_data <- vg_data  # working copy

na_counts <- colSums(is.na(working_data))
print(na_counts)

# Total NAs
total_na <- sum(na_counts)
cat("Total missing cells:", total_na, "\n")

# Visualization: Missing Data
par(mar = c(8, 5, 4, 2))
barplot(na_counts[na_counts > 0],
        main = "Count of Missing Entries per Column",
        col  = "darkcyan",
        las  = 2,
        cex.names = 0.8)


# Task-4: Imputation & Handling Invalid Data
calc_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if (length(uniqv) == 0) return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Pre-calculate fill values (numeric columns → median)
avg_year         <- median(working_data$Year,         na.rm = TRUE)
avg_critic_score <- median(working_data$Critic_Score, na.rm = TRUE)
avg_critic_count <- median(working_data$Critic_Count, na.rm = TRUE)
avg_user_score   <- median(working_data$User_Score,   na.rm = TRUE)
avg_user_count   <- median(working_data$User_Count,   na.rm = TRUE)

# Categorical columns → mode
mode_genre     <- calc_mode(working_data$Genre)
mode_publisher <- calc_mode(working_data$Publisher)
mode_developer <- calc_mode(working_data$Developer)
mode_rating    <- calc_mode(working_data$Rating)

# Apply Imputation
processed_data <- working_data %>%
  mutate(
    Year         = replace_na(Year,         avg_year),
    Critic_Score = replace_na(Critic_Score, avg_critic_score),
    Critic_Count = replace_na(Critic_Count, avg_critic_count),
    User_Score   = replace_na(User_Score,   avg_user_score),
    User_Count   = replace_na(User_Count,   avg_user_count),
    
    Genre     = replace_na(Genre,     mode_genre),
    Publisher = replace_na(Publisher, mode_publisher),
    Developer = replace_na(Developer, mode_developer),
    Rating    = replace_na(Rating,    mode_rating)
  )

# Verify cleanup
cat("\n[Check] Remaining NAs:\n")
print(colSums(is.na(processed_data)))

# Create a binary target: High-selling game (Global Sales > median)
median_sales <- median(processed_data$Global_Sales, na.rm = TRUE)
processed_data <- processed_data %>%
  mutate(High_Seller = ifelse(Global_Sales > median_sales, 1, 0))


# Task-5: Outlier Management
ggplot(processed_data, aes(x = Global_Sales)) +
  geom_boxplot(fill = "orange", alpha = 0.7,
               outlier.colour = "purple", outlier.size = 2) +
  labs(title = "Boxplot: Global Sales (Pre-cleaning)", x = "Global Sales (millions)") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# IQR Filtering on Global Sales
quantiles  <- quantile(processed_data$Global_Sales, probs = c(0.25, 0.75), na.rm = TRUE)
iqr_value  <- IQR(processed_data$Global_Sales, na.rm = TRUE)
lower_bound <- quantiles[1] - 1.5 * iqr_value
upper_bound <- quantiles[2] + 1.5 * iqr_value

processed_data <- processed_data %>%
  filter(Global_Sales >= lower_bound & Global_Sales <= upper_bound)


# Task-6: Remove Duplicates & Normalize
cat("\n[Task 6] Removing Duplicates...\n")
initial_rows <- nrow(processed_data)
processed_data <- distinct(processed_data)
cat("Rows dropped:", initial_rows - nrow(processed_data), "\n")

# Normalize Critic Score (0-1 Scaling)
min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

processed_data$Critic_Score_Norm <- min_max_norm(processed_data$Critic_Score)
summary(processed_data$Critic_Score_Norm)


# Task-7: Balance Classes & Split Data
cat("\n[Task 7] Balancing Data...\n")
# Downsample majority class (High_Seller 0 vs 1)
target_counts  <- table(processed_data$High_Seller)
min_class_size <- min(target_counts)

balanced_data <- processed_data %>%
  group_by(High_Seller) %>%
  sample_n(min_class_size) %>%
  ungroup()

print(table(balanced_data$High_Seller))

# Splitting 80/20
set.seed(123)
balanced_data$row_id <- 1:nrow(balanced_data)
train_df <- balanced_data %>% sample_frac(0.8)
test_df  <- anti_join(balanced_data, train_df, by = "row_id") %>% select(-row_id)
train_df <- train_df %>% select(-row_id)


# Task-8: EDA & Visualization

# 1. Global Sales by Genre
ggplot(processed_data, aes(x = reorder(Genre, Global_Sales, median),
                           y = Global_Sales, fill = Genre)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Global Sales by Genre",
       x = "Genre", y = "Global Sales (millions)") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

# 2. Average Sales by Platform
platform_stats <- processed_data %>%
  group_by(Platform) %>%
  summarise(Mean_Sales = mean(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Mean_Sales)) %>%
  slice_head(n = 15)   # top 15 platforms for readability

ggplot(platform_stats, aes(x = reorder(Platform, Mean_Sales), y = Mean_Sales)) +
  geom_col(fill = "steelblue", width = 0.6) +
  labs(title = "Top 15 Platforms by Avg Global Sales",
       x = "Platform", y = "Avg Sales (millions)") +
  theme_light() +
  coord_flip()

# 3. Game Count by Rating
ggplot(processed_data, aes(x = Rating)) +
  geom_bar(fill = "#69b3a2", color = "black") +
  labs(title = "Number of Games per Rating Category",
       x = "ESRB Rating", y = "Count") +
  theme_minimal()

# 4. Critic Score Distribution
ggplot(processed_data, aes(x = Critic_Score)) +
  geom_histogram(bins = 20, fill = "gold", color = "gray20") +
  labs(title = "Histogram of Critic Scores",
       x = "Critic Score", y = "Count") +
  theme_classic()


# Task-9: Correlation & Regression
# Correlation Plot (numeric columns only)
matrix_vars <- processed_data %>%
  select(NA_Sales, EU_Sales, JP_Sales, Other_Sales,
         Global_Sales, Critic_Score, User_Score, High_Seller)

cor_mat <- cor(matrix_vars, use = "pairwise.complete.obs")

corrplot(cor_mat, method = "circle",
         type  = "lower",
         col   = colorRampPalette(c("purple", "white", "orange"))(200),
         addCoef.col = "black",
         tl.col      = "black",
         title       = "Feature Correlations",
         mar         = c(0, 0, 2, 0))


# Task-10: Statistics & Skewness
cat("\n[Task 10] Statistical Summary:\n")
stat_cols <- c("Global_Sales", "Critic_Score", "User_Score")
print(summary(processed_data[stat_cols]))

# Custom Skewness Plotter
visualize_skew <- function(dataset, col_name, plot_title, bar_color) {
  col_val <- dataset[[col_name]]
  avg_val <- mean(col_val, na.rm = TRUE)
  med_val <- median(col_val, na.rm = TRUE)
  
  ggplot(dataset, aes_string(x = col_name)) +
    geom_histogram(bins = 25, fill = bar_color, color = "white", alpha = 0.9) +
    geom_vline(xintercept = avg_val, color = "black", linetype = "solid",  linewidth = 1) +
    geom_vline(xintercept = med_val, color = "red",   linetype = "dashed", linewidth = 1) +
    labs(title = plot_title, subtitle = "Black = Mean, Red = Median", y = "Frequency") +
    theme_linedraw()
}

# Generate Plots
p_sales  <- visualize_skew(processed_data, "Global_Sales",  "Distribution: Global Sales",  "yellow")
p_critic <- visualize_skew(processed_data, "Critic_Score",  "Distribution: Critic Score",  "green")

print(p_sales)
print(p_critic)

