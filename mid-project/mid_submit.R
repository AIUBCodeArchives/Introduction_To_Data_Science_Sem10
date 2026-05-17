# Task-1: Setup Libraries
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("ggplot2")

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Task-2: Load and Prepare Data
vg_data <- read_csv("D:/AIUB Notes/Introduction to Data Science/main/Video_Games_Sales_as_at_22_Dec_2016 2.csv")
vg_data <- vg_data %>% filter(!is.na(Name))
# Rename columns for cleaner access
new_colnames <- c("Name", "Platform", "Year", "Genre", "Publisher", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales",
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

total_na <- sum(na_counts)
cat("Total missing cells:", total_na, "\n")

# Visualization: Missing Data
par(mar = c(8, 5, 4, 2))
barplot(na_counts[na_counts > 0],
        main = "Count of Missing Entries per Column",
        col  = "yellow",
        las  = 2,
        cex.names = 0.8)

# Task-4: Imputation & Handling Invalid Data
calc_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if (length(uniqv) == 0) return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Numeric columns → median
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
cat("\n[Check] Remaining NAs after imputation:\n")
print(colSums(is.na(processed_data)))

# Create binary target column: High_Seller (1 = above median Global Sales)
median_sales <- median(processed_data$Global_Sales, na.rm = TRUE)
processed_data <- processed_data %>%
  mutate(High_Seller = ifelse(Global_Sales > median_sales, 1, 0))

# Task-5: Outlier Management

# IQR Filtering
quantiles   <- quantile(processed_data$Global_Sales, probs = c(0.25, 0.75), na.rm = TRUE)
iqr_value   <- IQR(processed_data$Global_Sales, na.rm = TRUE)
lower_bound <- quantiles[1] - 1.5 * iqr_value
upper_bound <- quantiles[2] + 1.5 * iqr_value

processed_data <- processed_data %>%
  filter(Global_Sales >= lower_bound & Global_Sales <= upper_bound)

cat("\nRows remaining after outlier removal:", nrow(processed_data), "\n")

# Task-6: Remove Duplicates & Normalize
cat("\n[Task 6] Removing Duplicates...\n")
initial_rows <- nrow(processed_data)
processed_data <- distinct(processed_data)
cat("Rows dropped:", initial_rows - nrow(processed_data), "\n")

# Min-Max Normalize Critic Score
min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

processed_data$Critic_Score_Norm <- min_max_norm(processed_data$Critic_Score)
cat("\nCritic Score Norm summary:\n")
print(summary(processed_data$Critic_Score_Norm))

# Task-7: Balance Classes & Split Data
cat("\n[Task 7] Balancing Data...\n")
target_counts  <- table(processed_data$High_Seller)
min_class_size <- min(target_counts)

balanced_data <- processed_data %>%
  group_by(High_Seller) %>%
  sample_n(min_class_size) %>%
  ungroup()

print(table(balanced_data$High_Seller))

# 80/20 Train-Test Split
set.seed(123)
balanced_data$row_id <- 1:nrow(balanced_data)
train_df <- balanced_data %>% sample_frac(0.8)
test_df  <- anti_join(balanced_data, train_df, by = "row_id") %>% select(-row_id)
train_df <- train_df %>% select(-row_id)

cat("\nTraining set rows:", nrow(train_df), "\n")
cat("Test set rows:    ", nrow(test_df),  "\n")

# Data cleaning done
cat("\n[Done] Preprocessed data is ready.\n")
cat("Final columns:\n")
print(colnames(processed_data))
write_csv(processed_data, "VG_Processed.csv")

