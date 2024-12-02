# These are necessary libraries used for the analysis of the data set 
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

# Creating the descriptive statistics table
stargazer(
  processed_data,
  type = "text",
  title = "Descriptive Statistics of Key Variables",
  summary = TRUE,
  digits = 2
)

# Logistic regression: Predict high ER utilization
logistic_model <- glm(
  high_er_utilization ~ insurance + family_income + age + education_years +
    high_bp + cancer + family_size,
  data = processed_data,
  family = "binomial"
)

# A preview of a summary of the model 
summary(logistic_model)

# Stargazer regression table
stargazer(
  logistic_model,
  type = "text",
  title = "Logistic Regression: Predicting High ER Utilization",
  dep.var.labels = c("High ER Utilization"),
  covariate.labels = c(
    "Insurance Type",
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
  logistic_model,
  type = "html",
  title = "Logistic Regression: Predicting High ER Utilization",
  dep.var.labels = c("High ER Utilization"),
  covariate.labels = c(
    "Insurance Type",
    "Family Income",
    "Age",
    "Education (Years)",
    "High Blood Pressure",
    "Cancer Diagnosis",
    "Family Size"
  ),
  omit.stat = c("LL", "ser", "f"),
  out = "Logistic Regression Table.html"
)

# Creating Figure 1
processed_data$insurance <- recode(processed_data$insurance,
                                   `1` = "HDHP",
                                   `2` = "LDHP",
                                   `3` = "No Insurance",
                                   .default = NA_character_)
processed_data$predicted_prob <- predict(logistic_model, type = "response")
processed_data$insurance <- factor(processed_data$insurance, levels = c("HDHP", "LDHP", "No Insurance"))
subset_LDHP <- subset(processed_data, insurance == "LDHP")
subset_HDHP <- subset(processed_data, insurance == "HDHP")
hist_LDHP <- ggplot(subset_LDHP, aes(x = predicted_prob, fill = insurance)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, color = "black", position = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Predicted Probability",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +  
  ggtitle("LDHP")

hist_HDHP <- ggplot(subset_HDHP, aes(x = predicted_prob, fill = insurance)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, color = "black", position = "identity") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    x = "Predicted Probability",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +  
  ggtitle("HDHP")

grid.arrange(hist_HDHP, hist_LDHP, ncol = 2, top = textGrob("Histogram of Predicted Probabilities of High ER Utilization"))


# Creating Figure 2
summary_data <- processed_data %>%
  group_by(insurance) %>%
  summarise(
    mean_er_visits = mean(total_visits, na.rm = TRUE),
    proportion_high_utilizers = mean(high_er_utilization, na.rm = TRUE)
  )

summary_data_long <- summary_data %>%
  pivot_longer(cols = c(mean_er_visits, proportion_high_utilizers),
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
    labels = c("Mean ER Visits", "Proportion of High Utilizers"),
    values = c("coral", "darkseagreen3")
  ) +
  theme_minimal()

# Creating Table 3
filtered_data <- processed_data %>%
  filter(high_bp %in% c(-1, 1, 2), cancer %in% c(-1, 1, 2))
summary_table_filtered <- filtered_data %>%
  group_by(insurance) %>%
  summarize(
    'Average Age' = mean(age, na.rm = TRUE),
    'High Blood Pressure' = paste0(round((sum(high_bp == 1, na.rm = TRUE) / n()) * 100, 2), "%"),
    'Cancer' = paste0(round((sum(cancer == 1, na.rm = TRUE) / n()) * 100, 2), "%"),
    'Total Population' = n(),
    'Average Family Income' = mean((family_income[family_income > 0]), na.rm = TRUE)
  )
