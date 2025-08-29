# >> Change Path <<
setwd("C:/Users/YourUsername/Documents/YourProjectDirectory") 

## Load Data
data <- read.csv("watson_healthcare_modified.csv")
data <- data[, !(names(data) %in% c("EmployeeCount", "Over18", "StandardHours", "EmployeeID"))]
attach(data)

## Load Required Packages
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("vcd")
library(vcd)
library(GGally)
install.packages("moments")

#### IV. EDA ####

## Preprocessing ##
# Ensure Attrition is factor type
data$Attrition <- as.factor(data$Attrition)
# education
data$Education <- as.factor(data$Education)
# environment_satisfaction
data$EnvironmentSatisfaction <- as.factor(data$EnvironmentSatisfaction)
# job involvement
data$JobInvolvement <- as.factor(data$JobInvolvement)
# job satisfaction
data$JobSatisfaction <- as.factor(data$JobSatisfaction)
# job level
data$JobLevel <- as.factor(data$JobLevel)
# performancerating
data$PerformanceRating <- as.factor(data$PerformanceRating)
# relationship satisfaction
data$RelationshipSatisfaction <- as.factor(data$RelationshipSatisfaction)
# shift
data$Shift <- as.factor(data$Shift)
# work life balance
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)

# Check Attrition distribution
table(data$Attrition)

# Get numeric variables
numeric_vars <- names(data)[sapply(data, is.numeric)]

# Convert text variables to factors
data <- data %>% mutate(across(where(is.character), as.factor))

# Calculate skewness and create plots
library(moments)  # For calculating skewness
for (var in numeric_vars) {
  cat("Variable:", var, "\n")
  
  # Skewness coefficient
  skewness_val <- skewness(data[[var]], na.rm = TRUE)
  cat("Skewness:", skewness_val, "\n")
  
  ##### Table 1: Create distribution plots #####
  hist(data[[var]], main = paste("Histogram of", var), xlab = var, col = "skyblue")
}

# Transformation
data$Log_DistanceFromHome <- log(data$DistanceFromHome + 1)  # Add 1 to prevent log(0)
data$Log_NumCompaniesWorked <- log(data$NumCompaniesWorked + 1)
data$Log_PercentSalaryHike <- log(data$PercentSalaryHike + 1)
data$Log_TotalWorkingYears <- log(data$TotalWorkingYears + 1)
data$Log_YearsAtCompany <- log(data$YearsAtCompany + 1)
data$Log_YearsInCurrentRole <- log(data$YearsInCurrentRole + 1)
data$Log_YearsSinceLastPromotion <- log(data$YearsSinceLastPromotion + 1)
data$Log_YearsWithCurrManager <- log(data$YearsWithCurrManager + 1)

# Save original variables
original_vars <- data[, c("DistanceFromHome", "NumCompaniesWorked", "PercentSalaryHike", 
                          "TotalWorkingYears", "YearsAtCompany", "YearsInCurrentRole", 
                          "YearsSinceLastPromotion", "YearsWithCurrManager")]

## End Preprocessing ##

### 1. When y-axis is attrition=no, x-axis shows distribution of different factor variables ###
# Create a new data dataframe to calculate subcategory frequencies
selected_columns <- c("BusinessTravel", "Department", "EducationField", "EnvironmentSatisfaction", 
                      "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", 
                      "OverTime", "RelationshipSatisfaction", "Shift", "WorkLifeBalance","Attrition")
data2<-data[selected_columns]
data_yes <- data2[data2$Attrition == "Yes", ]
data_no <- data2[data2$Attrition == "No", ]

no.frequency <- data.frame()
# Calculate subcategory frequencies for each X item and store
for (col in selected_columns) {
  temp_table <- table(data_no[[col]])
  temp_df <- as.data.frame(temp_table)
  temp_df$X_item <- col  # Add X item name
  colnames(temp_df) <- c("SubCategory", "Freq", "X_item")  # Set column names
  # Combine results into total dataframe
  no.frequency <- rbind(no.frequency, temp_df)
}
no.frequency<-no.frequency[no.frequency$X_item != "Attrition", ]

#no.frequency
ggplot(no.frequency, aes(x = X_item, y = Freq, fill = Freq)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  geom_text(aes(label = paste(SubCategory, "\n", Freq)),  # Label frequencies and subcategory names
            position = position_stack(vjust = 0.5), size = 2.3) +  # Display text in bar center
  labs(title = "Attrition=No by X Item", 
       x = "X Items", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate X-axis labels
        panel.grid = element_blank(),  # Hide background grid
        plot.margin = margin(2, 2, 2, 2)) +  # Adjust margins
  scale_fill_gradient(low = "blue", high = "red") 

### 2. When y-axis is attrition=yes, x-axis shows distribution of different factor variables ###
yes.frequency <- data.frame()
# Calculate subcategory frequencies for each X item and store
for (col in selected_columns) {
  temp_table <- table(data_yes[[col]])
  temp_df <- as.data.frame(temp_table)
  temp_df$X_item <- col  # Add X item name
  colnames(temp_df) <- c("SubCategory", "Freq", "X_item")  # Set column names
  # Combine results into total dataframe
  yes.frequency <- rbind(yes.frequency, temp_df)
}
yes.frequency<-yes.frequency[yes.frequency$X_item != "Attrition", ]

# Create stacked bar chart
#yes.frequency
ggplot(yes.frequency, aes(x = X_item, y = Freq, fill = Freq)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  geom_text(aes(label = paste(SubCategory, "\n", Freq)),  # Label frequencies and subcategory names
            position = position_stack(vjust = 0.5), size = 2.3) +  # Display text in bar center
  labs(title = "Attrition=Yes by X Item", 
       x = "X Items", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate X-axis labels
        panel.grid = element_blank(),  # Hide background grid
        plot.margin = margin(2, 2, 2, 2)) +  # Adjust margins
  scale_fill_gradient(low = "blue", high = "red")  # Color gradient from blue to red based on frequency

# Restore original plotting parameters
par(mfrow = c(1, 1))

### 3. Mosaic Plot of JobSatisfaction, JobInvolvement, RelationshipSatisfaction, and WorkLifeBalance ###
 
xtabs(~JobSatisfaction+JobInvolvement+RelationshipSatisfaction+WorkLifeBalance, data=data)

# Create contingency table
job_table_1 <- xtabs(~ JobSatisfaction + JobInvolvement + RelationshipSatisfaction + WorkLifeBalance, 
                   data = data)

# Create Mosaic Plot
mosaicplot(job_table_1, 
           main = "Mosaic Plot of JobSatisfaction, JobInvolvement, RelationshipSatisfaction, and WorkLifeBalance", 
           color = TRUE,  # Use colors to distinguish different blocks
           xlab = "Job Satisfaction", 
           ylab = "Job Involvement",
           )       # Display axis labels vertically

### 4. Mosaic Plot of Shift, EnvironmentSatisfaction, JobSatisfaction, and RelationshipSatisfaction ### 

xtabs(~Shift+EnvironmentSatisfaction+JobSatisfaction+RelationshipSatisfaction, data=data)
# Create contingency table
job_table_2 <- xtabs(~Shift+EnvironmentSatisfaction+JobSatisfaction+RelationshipSatisfaction, 
                   data = data)

# Create Mosaic Plot
mosaicplot(job_table_2, 
           main = "Mosaic Plot of Shift, EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction", 
           color = TRUE,  # Use colors to distinguish different blocks
           xlab = "Shift", 
           ylab = "EnvironmentSatisfaction",
           )      

### 5. Create scatter plot of DailyRate and MonthlyIncome ###
ggplot() +
  geom_point(data = data, aes(x = Age, y = DailyRate, color = "DailyRate")) +
  geom_point(data = data, aes(x = Age, y = MonthlyIncome, color = "MonthlyIncome")) +
  labs(title = "Age and DailyRate/MonthlyIncome", x = "Age", y = "Money") +
  scale_color_manual(values = c("DailyRate" = "blue", "MonthlyIncome" = "red"))

### 6. Bar chart of TrainingTimesLastYear and Department ###
ggplot(data, aes(x = TrainingTimesLastYear, fill = Department)) +
  geom_bar(position = "dodge") +  # Use dodge to display bars side by side
  labs(title = "TrainingTimesLastYear and Department", x = "Training Time Last Year", y = "Department") +
  scale_fill_brewer(palette = "Set3")

#### V. Model ####

### 1. Original Model ###

# Remove original variables
data$DistanceFromHome <- NULL
data$NumCompaniesWorked <- NULL
data$PercentSalaryHike <- NULL
data$TotalWorkingYears <- NULL
data$YearsAtCompany <- NULL
data$YearsInCurrentRole <- NULL
data$YearsSinceLastPromotion <- NULL
data$YearsWithCurrManager <- NULL

# Build original logistic regression model
logistic_model <- glm(Attrition ~ ., data = data, family = "binomial")

# View original model summary
summary(logistic_model)

# Extract model summary
model_summary <- summary(logistic_model)

# Extract coefficient table (including p-value)
coefficients_table <- model_summary$coefficients

# Filter variables with p-value < 0.05
significant_vars <- coefficients_table[coefficients_table[, "Pr(>|z|)"] < 0.05, ]

# Sort by p-value in ascending order
sorted_significant_vars <- significant_vars[order(significant_vars[, "Pr(>|z|)"]), ]

# Display sorted significant features
print(sorted_significant_vars)

# Extract significant feature names
significant_var_names <- rownames(sorted_significant_vars)
print(significant_var_names)

# Use stepwise regression to select best model
best_model <- step(logistic_model, direction = "both", trace = 1)

# View best model summary
summary(best_model)

# Extract best model summary
best_model_summary <- summary(best_model)

# Extract coefficient table (including p-value)
best_model_coefficients <- best_model_summary$coefficients

# Filter variables with p-value < 0.05
best_significant_vars <- best_model_coefficients[best_model_coefficients[, "Pr(>|z|)"] < 0.05, ]

# Sort by p-value in ascending order
best_sorted_significant_vars <- best_significant_vars[order(best_significant_vars[, "Pr(>|z|)"]), ]

# Display sorted significant features from stepwise regression model
print(best_sorted_significant_vars)

# Extract significant feature names
best_significant_var_names <- rownames(best_sorted_significant_vars)
print(best_significant_var_names)

# Check Attrition distribution (Yes/No counts)
attrition_counts <- table(data$Attrition)
print(attrition_counts)

# Calculate proportions for each category
attrition_proportions <- prop.table(attrition_counts)
print(attrition_proportions)

# Display proportions formatted as percentages
attrition_percentages <- round(attrition_proportions * 100, 2)
cat("Attrition ratio (percentage):\n")
print(attrition_percentages)

# Build null model (intercept only)
null_model <- glm(Attrition ~ 1, data = data, family = "binomial")

# Extract null model deviance and degrees of freedom
null_deviance <- null_model$deviance
null_df <- null_model$df.residual

# Calculate chi-square statistic and degrees of freedom
chi_squared_original <- null_deviance - logistic_model$deviance
df_original <- null_df - logistic_model$df.residual

# Calculate p-value
p_value_original <- pchisq(chi_squared_original, df = df_original, lower.tail = FALSE)

# Output results
cat("Original model vs null model:\n")
cat("Chi-squared statistic:", chi_squared_original, "\n")
cat("Degrees of freedom difference (df):", df_original, "\n")
cat("p-value:", p_value_original, "\n")

# Determine if significant
if (p_value_original < 0.05) {
  cat("Result: Significant\n")
} else {
  cat("Result: Not significant\n")
}

### 2. Bidirectional stepwise regression on original model ###

# Calculate chi-square statistic and degrees of freedom
chi_squared_stepwise <- null_deviance - best_model$deviance
df_stepwise <- null_df - best_model$df.residual

# Calculate p-value
p_value_stepwise <- pchisq(chi_squared_stepwise, df = df_stepwise, lower.tail = FALSE)

# Output results
cat("\nStepwise regression model vs null model:\n")
cat("Chi-squared statistic:", chi_squared_stepwise, "\n")
cat("Degrees of freedom difference (df):", df_stepwise, "\n")
cat("p-value:", p_value_stepwise, "\n")

# Determine if significant
if (p_value_stepwise < 0.05) {
  cat("Result: Significant\n")
} else {
  cat("Result: Not significant\n")
}

# Add back original variables
data$DistanceFromHome <- original_vars$DistanceFromHome
data$NumCompaniesWorked <- original_vars$NumCompaniesWorked
data$PercentSalaryHike <- original_vars$PercentSalaryHike
data$TotalWorkingYears <- original_vars$TotalWorkingYears
data$YearsAtCompany <- original_vars$YearsAtCompany
data$YearsInCurrentRole <- original_vars$YearsInCurrentRole
data$YearsSinceLastPromotion <- original_vars$YearsSinceLastPromotion
data$YearsWithCurrManager <- original_vars$YearsWithCurrManager

### Theme 1: Do salary increases and promotions have different effects on reducing turnover rates across different job levels? Does the company have an optimal salary structure? ###

# Select numeric variables and exclude Attrition 
numeric_data <- data %>%
  select(where(is.numeric)) 

# Calculate correlation coefficient matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Find highly correlated variables
high_corr_pairs <- which(abs(cor_matrix) > 0.6 & abs(cor_matrix) < 1, arr.ind = TRUE)

# Convert results to dataframe for easier processing
high_corr_df <- data.frame(
  Var1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Var2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

# Remove duplicate pairs (e.g., A-B and B-A)
high_corr_df <- high_corr_df[high_corr_df$Var1 < high_corr_df$Var2, ]
print(high_corr_df)

selected_variables <- c(
  "Age", "BusinessTravel", "Department", "EducationField", 
  "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobRole", 
  "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "MonthlyRate", 
  "OverTime", "RelationshipSatisfaction", "Shift","TrainingTimesLastYear", "WorkLifeBalance","Log_DistanceFromHome", "Log_NumCompaniesWorked",
  "Log_TotalWorkingYears", "Log_YearsInCurrentRole", "Log_YearsSinceLastPromotion", "Log_YearsWithCurrManager"
)
# Dynamically generate formula
formula <- as.formula(paste("Attrition ~", paste(selected_variables, collapse = " + ")))

# Print formula for confirmation
print(formula)

# Calculate count for each JobLevel
job_level_counts <- data %>%
  group_by(JobLevel) %>%
  summarise(Count = n())

# Create MonthlyIncome distribution plot by JobLevel with count annotations
library(ggplot2)

job_level_attrition <- data %>%
  group_by(JobLevel) %>%
  summarise(
    AttritionYesRatio = mean(Attrition == "Yes", na.rm = TRUE),
    Count = n()
  )
print(job_level_attrition)
# Create boxplot with mean values
ggplot(data, aes(x = JobLevel, y = MonthlyIncome, fill = JobLevel)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +  # Create boxplot
  stat_summary(fun = mean, geom = "point", color = "purple", size = 3) +  # Show mean as point
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
               vjust = -1, color = "purple") +  # Annotate mean
  stat_summary(fun = max, geom = "text", aes(label = round(..y.., 2)),
               vjust = -0.5, color = "blue") +  # Annotate maximum
  stat_summary(fun = min, geom = "text", aes(label = round(..y.., 2)),
               vjust = 1.5, color = "darkgreen") +  # Annotate minimum
  geom_text(data = job_level_counts, aes(x = JobLevel, y = max(data$MonthlyIncome, na.rm = TRUE), 
                                         label = paste("Count:", Count)),
            vjust = -1, color = "black") +  # Add count annotation for each JobLevel
  labs(title = "Income Distribution by Job Level",
       x = "Job Level",
       y = "Monthly Income") +
  theme_minimal()

data_level1 <- data %>% filter(JobLevel == 1)

# Dynamically generate formula, removing JobLevel
formula <- as.formula(
  paste(
    "Attrition ~", 
    paste(selected_variables[selected_variables != "JobLevel"], collapse = " + ")
  )
)
print(formula)
# Perform logistic regression to check if Income affects Attrition
model_income_attrition <- glm(formula, 
                              data = data_level1, 
                              family = binomial)
# Display regression results
summary(model_income_attrition)

data_level1 <- data %>% filter(JobLevel == 1| JobLevel == 2)
formula <- as.formula(
  paste(
    "Attrition ~", 
    paste(selected_variables, collapse = " + ")
  )
)

# Perform logistic regression to check if Income affects Attrition
model_income_attrition <- glm(formula, 
                              data = data_level1, 
                              family = binomial)
# Display regression results
summary(model_income_attrition)

### Theme 2: Job satisfaction is significantly related to turnover. What factors have the greatest impact on job satisfaction? ###

# Remove original variables
data$DistanceFromHome <- NULL
data$NumCompaniesWorked <- NULL
data$PercentSalaryHike <- NULL
data$TotalWorkingYears <- NULL
data$YearsAtCompany <- NULL
data$YearsInCurrentRole <- NULL
data$YearsSinceLastPromotion <- NULL
data$YearsWithCurrManager <- NULL

# Ensure necessary variables are factor type
data$JobSatisfaction <- as.factor(data$JobSatisfaction)
data$OverTime <- as.factor(data$OverTime)
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)
data$EnvironmentSatisfaction <- as.factor(data$EnvironmentSatisfaction)
data$RelationshipSatisfaction <- as.factor(data$RelationshipSatisfaction)

# New base model (without interactions)
new_base_model <- glm(
  Attrition ~ JobSatisfaction + OverTime + WorkLifeBalance + Age + 
    EnvironmentSatisfaction + RelationshipSatisfaction + 
    Log_DistanceFromHome + Log_YearsWithCurrManager + 
    Log_NumCompaniesWorked + Log_TotalWorkingYears + 
    TrainingTimesLastYear,
  data = data,
  family = "binomial"
)

# View base model summary
summary(new_base_model)

# Add interactions and build new model
interaction_model1 <- glm(
  Attrition ~ JobSatisfaction * WorkLifeBalance + 
    EnvironmentSatisfaction * RelationshipSatisfaction + 
    Log_NumCompaniesWorked * Log_TotalWorkingYears + 
    OverTime * Age + 
    OverTime + WorkLifeBalance + JobSatisfaction + 
    Age + EnvironmentSatisfaction + RelationshipSatisfaction + 
    Log_DistanceFromHome + Log_YearsWithCurrManager + 
    TrainingTimesLastYear,
  data = data,
  family = "binomial"
)

# View new model summary
summary(interaction_model1)

# Add interactions and build new model
interaction_model2 <- glm(
  Attrition ~ JobSatisfaction * WorkLifeBalance + 
    EnvironmentSatisfaction + RelationshipSatisfaction + 
    Log_NumCompaniesWorked * Log_TotalWorkingYears + 
    OverTime + Age + WorkLifeBalance + JobSatisfaction + 
    Log_DistanceFromHome + Log_YearsWithCurrManager + 
    TrainingTimesLastYear,
  data = data,
  family = "binomial"
)

# View new model summary
summary(interaction_model2)

# Compare AIC of base model and new models
cat("New Base Model AIC:", AIC(new_base_model), "\n")
cat("Interaction Model1 AIC:", AIC(interaction_model1), "\n")
cat("Interaction Model2 AIC:", AIC(interaction_model2), "\n")

### Theme 3: Verifying the impact of environmental satisfaction on turnover rates across different departments ###

# Categorical variable list
cat_vars <- c("Department", "EnvironmentSatisfaction")

# Function: Calculate attrition rate for each category
calculate_attrition_rate <- function(data, var) {
  data %>%
    group_by(!!sym(var)) %>%
    summarise(
      total_count = n(),
      attrition_count = sum(Attrition == "Yes"),
      attrition_rate = attrition_count / total_count * 100
    ) %>%
    arrange(desc(attrition_rate))
}

# Calculate attrition rate for each categorical variable
attrition_analysis <- lapply(cat_vars, function(var) {
  calculate_attrition_rate(data, var)
})

# Plot attrition rate bar chart
plot_attrition_rate <- function(data, var) {
  ggplot(data, aes(x = !!sym(var), y = attrition_rate)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = paste("Attrition Rate by", var),
         x = var, 
         y = "Attrition Rate (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate charts
plots <- lapply(seq_along(cat_vars), function(i) {
  plot_attrition_rate(attrition_analysis[[i]], cat_vars[i])
})
plots

# Add interaction between EnvironmentSatisfaction and Department
best_model_interact <- glm(formula = Attrition ~ Age + BusinessTravel + DailyRate + 
                             Department + EducationField + EnvironmentSatisfaction + JobInvolvement + 
                             JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + 
                             OverTime + RelationshipSatisfaction + Shift + TrainingTimesLastYear + 
                             WorkLifeBalance + Log_DistanceFromHome + Log_NumCompaniesWorked + 
                             Log_TotalWorkingYears + Log_YearsInCurrentRole + Log_YearsSinceLastPromotion + 
                             Log_YearsWithCurrManager + EnvironmentSatisfaction:Department, family = "binomial", data = data)
summary(best_model_interact)

# Create contingency table
contingency_table <- table(data$Department, 
                           data$EnvironmentSatisfaction, 
                           data$Attrition)

print(contingency_table)

# Perform chi-square test for each department
departments <- unique(data$Department)

chi_square_results <- lapply(departments, function(dept) {
  dept_data <- data[data$Department == dept, ]
  
  cont_table <- table(
    dept_data$EnvironmentSatisfaction, 
    dept_data$Attrition
  )
  
  # Chi-square test
  chi_test <- chisq.test(cont_table)
  
  return(list(
    Department = dept,
    Chi_Square = chi_test$statistic,
    P_Value = chi_test$p.value
  ))
})

# Convert to dataframe for readability
chi_results_df <- do.call(rbind, chi_square_results)
print(chi_results_df)

library(ggplot2)
library(dplyr)

# Plot relationship between environmental satisfaction and attrition rate for each department
ggplot(data, aes(x = factor(EnvironmentSatisfaction), fill = Attrition)) +
  geom_bar(position = "fill") +  
  facet_wrap(~ Department) +
  labs(
    title = "Attrition Proportion by Environment Satisfaction and Department",
    x = "Environment Satisfaction",
    y = "Proportion"
  ) +
  theme_minimal()

# Interaction model with only EnvironmentSatisfaction and Department
interact_model <- glm(
  Attrition ~ EnvironmentSatisfaction * Department, 
  data = data, 
  family = binomial
)
summary(interact_model)
anova(best_model, best_model_interact, test="Chisq")

# Calculate odds ratios for environmental satisfaction
coefficients <- c(-1.746, -2.702, -3.230) # Input coefficients from model
odds_ratios <- exp(coefficients)          # Calculate odds ratios
odds_ratios