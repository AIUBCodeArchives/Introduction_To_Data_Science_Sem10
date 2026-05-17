# Task-1: Setup Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(tidyr)

# Task-2: Load and Prepare Data
mental_data <- read_csv("C:/Users/tmjab/Study/FALL 25-26/data_science/midterm.csv")
# Renaming columns for cleaner access
new_colnames <- c("Gender", "Age", "Ac_Pressure", "Study_Sat", 
                  "Sleep_Dur", "Diet_Habits", "Suicidal", 
                  "Study_Hrs", "Fin_Stress", "Fam_History", "Depression")
colnames(mental_data) <- new_colnames

# Data Inspection
dim(mental_data)
head(mental_data)
str(mental_data)
summary(mental_data)

# Task-3: Identify Missing Values
cat("\n[Task 3] checking for missing values...\n")
working_data <- mental_data # working copy
na_counts <- colSums(is.na(working_data))
print(na_counts)

# Total NAs
total_na <- sum(na_counts)
cat("Total missing cells:", total_na, "\n")

# Visualization: Missing Data
par(mar=c(8, 5, 4, 2)) 
barplot(na_counts[na_counts > 0],
        main = "Count of Missing Entries per Column",
        col = "darkcyan",
        las = 2,
        cex.names = 0.8)

# Task-4: Imputation & Handling Invalid Data
calc_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if (length(uniqv) == 0) return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Pre-calculate fill values
avg_age <- median(working_data$Age, na.rm = TRUE)
avg_study <- median(working_data$Study_Hrs, na.rm = TRUE)
avg_press <- median(working_data$Ac_Pressure, na.rm = TRUE)
avg_fin <- median(working_data$Fin_Stress, na.rm = TRUE)
avg_sat <- median(working_data$Study_Sat, na.rm = TRUE)

# Fill Categorical columns with Mode
mode_sleep <- calc_mode(working_data$Sleep_Dur)
mode_diet <- calc_mode(working_data$Diet_Habits)
mode_gender <- calc_mode(working_data$Gender)
mode_dep <- calc_mode(working_data$Depression)
mode_fam <- calc_mode(working_data$Fam_History)
mode_sui <- calc_mode(working_data$Suicidal)

# Apply Imputation
processed_data <- working_data %>%
  mutate(
    Age = replace_na(Age, avg_age),
    Study_Hrs = replace_na(Study_Hrs, avg_study),
    Ac_Pressure = replace_na(Ac_Pressure, avg_press),
    Fin_Stress = replace_na(Fin_Stress, avg_fin),
    Study_Sat = replace_na(Study_Sat, avg_sat),
    
    Sleep_Dur = replace_na(Sleep_Dur, mode_sleep),
    Diet_Habits = replace_na(Diet_Habits, mode_diet),
    Gender = replace_na(Gender, mode_gender),
    Depression = replace_na(Depression, mode_dep),
    Fam_History = replace_na(Fam_History, mode_fam),
    Suicidal = replace_na(Suicidal, mode_sui)
  )

# Verify cleanup
cat("\n[Check] Remaining NAs:\n")
print(colSums(is.na(processed_data)))

# Create binary targets for analysis
processed_data <- processed_data %>%
  mutate(
    Depression_Bin = ifelse(Depression == "Yes", 1, 0),
    Suicidal_Bin = ifelse(Suicidal == "Yes", 1, 0)
  )

# Task-5: Outlier Management
ggplot(processed_data, aes(x = Study_Hrs)) +
  geom_boxplot(fill = "orange", alpha=0.7, outlier.colour = "purple", outlier.size = 2) +
  labs(title = "Boxplot: Study Hours (Pre-cleaning)", x = "Daily Study Hours") +
  theme_classic() + # Changed theme
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# IQR Filtering
quantiles <- quantile(processed_data$Study_Hrs, probs = c(0.25, 0.75), na.rm = TRUE)
iqr_value <- IQR(processed_data$Study_Hrs, na.rm = TRUE)
lower_bound <- quantiles[1] - 1.5 * iqr_value
upper_bound <- quantiles[2] + 1.5 * iqr_value

processed_data <- processed_data %>%
  filter(Study_Hrs >= lower_bound & Study_Hrs <= upper_bound)

# Task-6: Remove Duplicates & Normalize
cat("\n[Task 6] Removing Duplicates...\n")
initial_rows <- nrow(processed_data)
processed_data <- distinct(processed_data)
cat("Rows dropped:", initial_rows - nrow(processed_data), "\n")

# Normalize Academic Pressure (0-1 Scaling)
# Using a custom function for cleaner code
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

processed_data$Ac_Pressure_Norm <- min_max_norm(processed_data$Ac_Pressure)
summary(processed_data$Ac_Pressure_Norm)

# Task-7: Balance Classes & Split Data
cat("\n[Task 7] Balancing Data...\n")
# Downsampling majority class
target_counts <- table(processed_data$Depression_Bin)
min_class_size <- min(target_counts)

balanced_data <- processed_data %>%
  group_by(Depression_Bin) %>%
  sample_n(min_class_size) %>%
  ungroup()

print(table(balanced_data$Depression_Bin))

# Splitting 80/20
set.seed(123) # Ensure reproducibility
balanced_data$row_id <- 1:nrow(balanced_data)
train_df <- balanced_data %>% sample_frac(0.8)
test_df <- anti_join(balanced_data, train_df, by = "row_id") %>% select(-row_id)
train_df <- train_df %>% select(-row_id)

# Task-8: EDA & Visualization
# 1. Depression vs Pressure
ggplot(processed_data, aes(x = Depression, y = Ac_Pressure, fill = Depression)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("cadetblue", "coral")) + # New colors
  labs(title = "Impact of Depression on Academic Pressure", 
       y = "Pressure Level", x = "Depression") +
  theme_bw()

# 2. Sleep vs Study Hours
sleep_stats <- processed_data %>%
  group_by(Sleep_Dur) %>%
  summarise(Mean_Study = mean(Study_Hrs))

ggplot(sleep_stats, aes(x = reorder(Sleep_Dur, Mean_Study), y = Mean_Study)) +
  geom_col(fill = "steelblue", width = 0.6) + # geom_col is alt to geom_bar(stat="identity")
  labs(title = "Study Habits vs Sleep", x = "Sleep Duration", y = "Avg Study Hours") +
  theme_light() +
  coord_flip() # Flipped for better label reading

# 3. Diet Habits Count
ggplot(processed_data, aes(x = Diet_Habits)) +
  geom_bar(fill = "#69b3a2", color = "black") +
  labs(title = "Survey of Dietary Habits", x = "Diet Type", y = "Participants") +
  theme_minimal()

# 4. Pressure Distribution
ggplot(processed_data, aes(x = Ac_Pressure)) +
  geom_histogram(bins = 8, fill = "gold", color = "gray20") +
  labs(title = "Histogram of Academic Pressure", x = "Score", y = "Count") +
  theme_classic()

# Task-9: Correlation & Regression
# Correlation Plot
matrix_vars <- processed_data %>% 
  select(Age, Ac_Pressure, Study_Sat, Study_Hrs, Fin_Stress, Depression_Bin)

cor_mat <- cor(matrix_vars, use = "pairwise.complete.obs")

# Changed style to 'circle' and palette to Purple-Orange
corrplot(cor_mat, method = "circle", 
         type = "lower", 
         col = colorRampPalette(c("purple", "white", "orange"))(200),
         addCoef.col = "black", 
         tl.col = "black", 
         title = "Feature Correlations", 
         mar = c(0,0,2,0))

# Task-10: Statistics & Skewness
cat("\n[Task 10] Statistical Summary:\n")
stat_cols <- c("Study_Hrs", "Ac_Pressure", "Age")
print(summary(processed_data[stat_cols]))

# Custom Skewness Plotter
visualize_skew <- function(dataset, col_name, plot_title, bar_color) {
  col_val <- dataset[[col_name]]
  avg_val <- mean(col_val, na.rm = TRUE)
  med_val <- median(col_val, na.rm = TRUE)
  
  ggplot(dataset, aes_string(x = col_name)) +
    geom_histogram(bins = 25, fill = bar_color, color = "white", alpha = 0.9) +
    geom_vline(xintercept = avg_val, color = "black", linetype = "solid", linewidth = 1) +
    geom_vline(xintercept = med_val, color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = plot_title, subtitle = "Black = Mean, Red = Median", y = "Frequency") +
    theme_linedraw()
}

# Generate Plots
p_study <- visualize_skew(processed_data, "Study_Hrs", "Distribution: Study Hours", "yellow")
p_age <- visualize_skew(processed_data, "Age", "Distribution: Age", "green")

print(p_study)
print(p_age)

