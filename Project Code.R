# Loading necessary libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(jtools)
library(readxl)
library(tidyr)
library(gridExtra)
library(grid)


# Loading the filtered, pre-processed, and merged data-set
processed_data <- read_xlsx("2022 MEPS dataset.xlsx")

# Defining High ER utilization
processed_data <- processed_data %>%
  mutate(high_er_utilization = ifelse(total_visits > 2, 1, 0))  

# Defining any ER utilization
processed_data <- processed_data %>%
  mutate(any_er_utilization = ifelse(total_visits > 0, 1, 0))

# Defining insurance type 
processed_data$insurance <- recode(processed_data$insurance,
                                   `1` = "HDHP",
                                   `2` = "LDHP",
                                   `3` = "No Insurance",
                                   .default = NA_character_)

# Exclude "No Insurance" from the dataset
processed_data <- processed_data %>%
  filter(insurance != "No Insurance")

# Creating the descriptive statistics table
stargazer(
  processed_data,
  type = "text",
  title = "Descriptive Statistics of Key Variables",
  summary = TRUE,
  digits = 2
)

# Logistic regression: Predict high ER utilization
logistic_model_high <- glm(
  high_er_utilization ~ insurance + family_income + age + education_years +
    high_bp + cancer + family_size,
  data = processed_data,
  family = "binomial"
)

# Logistic regression: Predict any ER utilization
logistic_model_any <- glm(
  any_er_utilization ~ insurance + family_income + age + education_years +
    high_bp + cancer + family_size,
  data = processed_data,
  family = "binomial"
)

# Summary of models
summary(logistic_model_high)
summary(logistic_model_any)

# Stargazer regression table for high ER utilization
stargazer(
  logistic_model_high,
  type = "text",
  title = "Logistic Regression: Predicting High ER Utilization",
  dep.var.labels = c("High ER Utilization"),
  covariate.labels = c(
    "Insurance Type (HDHP, LDHP)",
    "Family Income",
    "Age",
    "Education (Years)",
    "High Blood Pressure",
    "Cancer Diagnosis",
    "Family Size"
  ),
  omit.stat = c("LL", "ser", "f"),
  align = TRUE,
  digits = 3
)

# Stargazer regression table for any ER utilization
stargazer(
  logistic_model_any,
  type = "text",
  title = "Logistic Regression: Predicting Any ER Utilization",
  dep.var.labels = c("Any ER Utilization"),
  covariate.labels = c(
    "Insurance Type (HDHP, LDHP)",
    "Family Income",
    "Age",
    "Education (Years)",
    "High Blood Pressure",
    "Cancer Diagnosis",
    "Family Size"
  ),
  omit.stat = c("LL", "ser", "f"),
  align = TRUE,
  digits = 3
)

# Save outputs as LaTeX or HTML 
stargazer(
  logistic_model_high,
  type = "html",
  title = "Logistic Regression: Predicting High ER Utilization",
  dep.var.labels = c("High ER Utilization"),
  covariate.labels = c(
    "Insurance Type (HDHP, LDHP)",
    "Family Income",
    "Age",
    "Education (Years)",
    "High Blood Pressure",
    "Cancer Diagnosis",
    "Family Size"
  ),
  omit.stat = c("LL", "ser", "f"),
  out = "Logistic Regression High ER Table.html"
)

stargazer(
  logistic_model_any,
  type = "html",
  title = "Logistic Regression: Predicting Any ER Utilization",
  dep.var.labels = c("Any ER Utilization"),
  covariate.labels = c(
    "Insurance Type (HDHP, LDHP)",
    "Family Income",
    "Age",
    "Education (Years)",
    "High Blood Pressure",
    "Cancer Diagnosis",
    "Family Size"
  ),
  omit.stat = c("LL", "ser", "f"),
  out = "Logistic Regression Any ER Table.html"
)

# Creating Figure 1
processed_data$predicted_prob_high <- predict(
  logistic_model_high, type = "response")
processed_data$predicted_prob_any <- predict(
  logistic_model_any, type = "response")
processed_data$insurance <- factor(
  processed_data$insurance, levels = c("HDHP", "LDHP"))

subset_LDHP <- subset(processed_data, insurance == "LDHP")
subset_HDHP <- subset(processed_data, insurance == "HDHP")

hist_LDHP <- ggplot(subset_LDHP, aes(
  x = predicted_prob_high, fill = insurance)) +
  geom_histogram(
    binwidth = 0.05, alpha = 0.7, color = "black", position = "identity") +
  scale_fill_manual(values = c("red3"), aesthetics = "fill") +
  labs(
    x = "Predicted Probability",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman", size = 12)) +  
  ggtitle("LDHP")

hist_HDHP <- ggplot(
  subset_HDHP, aes(x = predicted_prob_high, fill = insurance)) +
  geom_histogram(
    binwidth = 0.05, alpha = 0.7, color = "black", position = "identity") +
  scale_fill_manual(values = c("royalblue3"), aesthetics = "fill") +
  labs(
    x = "Predicted Probability",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman", size = 12)) +  
  ggtitle("HDHP")

grid.arrange(hist_HDHP, hist_LDHP, ncol = 2, top = textGrob(
  "Histogram of Predicted Probabilities of High ER Utilization", 
  gp = gpar(fontfamily = "Times New Roman", fontsize = 14)))


# Creating Figure 2
summary_data <- processed_data %>%
  group_by(insurance) %>%
  summarise(
    mean_er_visits = mean(total_visits, na.rm = TRUE),
    proportion_high_utilizers = mean(high_er_utilization, na.rm = TRUE),
    proportion_any_utilizers = mean(any_er_utilization, na.rm = TRUE)
  )

summary_data_long <- summary_data %>%
  pivot_longer(cols = c(
    mean_er_visits, proportion_high_utilizers, proportion_any_utilizers),
               names_to = "Metric",
               values_to = "Value")

ggplot(summary_data_long, aes(x = insurance, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "ER Utilization by Insurance Type",
    x = "Insurance Type",
    y = "Value",
    fill = "Metric"
  ) +
  scale_fill_manual(
    labels = c(
      "Mean ER Visits", "Proportion of High Utilizers", 
      "Proportion of Any Utilizers"),
    values = c("red3", "royalblue3", "darkgreen")
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12)
  )

# Creating Table 3
filtered_data <- processed_data %>%
  filter(high_bp %in% c(-1, 1, 2), cancer %in% c(-1, 1, 2))
summary_table_filtered <- filtered_data %>%
  group_by(insurance) %>%
  summarize(
    'Average Age' = mean(age, na.rm = TRUE),
    'High Blood Pressure' = 
      paste0(round((sum(high_bp == 1, na.rm = TRUE) / n()) * 100, 2), "%"),
    'Cancer' = 
      paste0(round((sum(cancer == 1, na.rm = TRUE) / n()) * 100, 2), "%"),
    'Total Population' = n(),
    'Average Family Income' = 
      mean((family_income[family_income > 0]), na.rm = TRUE)
  )

# Creating Figure 3
visit_count_summary <- processed_data %>%
  mutate(total_visits_grouped = 
           ifelse(total_visits > 10, "10+ Visits",
                  as.character(total_visits))) %>%
  group_by(total_visits_grouped) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 2))

custom_palette <- c(
  "1" = "red3",
  "4" = "royalblue3",
  "9" = "darkgreen",
  "10+ Visits" = "darkred"
)
ggplot(visit_count_summary, aes(x = "",
                                y = count, fill = total_visits_grouped)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribution of ER Visits by Number (Grouped)",
    fill = "Number of ER Visits"
  ) +
  scale_fill_manual(values = custom_palette) +
  theme_void() +  
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")  
  ) +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3,
            family = "Times New Roman")
