# >> 更改路徑 <<
setwd("/Users/ariianakkk/Desktop") 

## 讀取數據
data <- read.csv("watson_healthcare_modified.csv")
data <- data[, !(names(data) %in% c("EmployeeCount", "Over18", "StandardHours", "EmployeeID"))]
attach(data)

##載入所需的套件
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

#### 肆、EDA ####

## 前處理 ##
# 確保 Attrition 為因子類型
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

# 檢查 Attrition 分佈
table(data$Attrition)

# 獲取數值型變數
numeric_vars <- names(data)[sapply(data, is.numeric)]

# 將文字變數轉為因子
data <- data %>% mutate(across(where(is.character), as.factor))

# 計算偏態和繪圖
library(moments)  # 用於計算偏態
for (var in numeric_vars) {
  cat("變數:", var, "\n")
  
  # 偏態系數
  skewness_val <- skewness(data[[var]], na.rm = TRUE)
  cat("偏態 (Skewness):", skewness_val, "\n")
  
  ##### 表格1 繪製分布圖 #####
  hist(data[[var]], main = paste("Histogram of", var), xlab = var, col = "skyblue")
}

# tranformation
data$Log_DistanceFromHome <- log(data$DistanceFromHome + 1)  # 加 1 防止 log(0)
data$Log_NumCompaniesWorked <- log(data$NumCompaniesWorked + 1)
data$Log_PercentSalaryHike <- log(data$PercentSalaryHike + 1)
data$Log_TotalWorkingYears <- log(data$TotalWorkingYears + 1)
data$Log_YearsAtCompany <- log(data$YearsAtCompany + 1)
data$Log_YearsInCurrentRole <- log(data$YearsInCurrentRole + 1)
data$Log_YearsSinceLastPromotion <- log(data$YearsSinceLastPromotion + 1)
data$Log_YearsWithCurrManager <- log(data$YearsWithCurrManager + 1)

# 保存原始變數
original_vars <- data[, c("DistanceFromHome", "NumCompaniesWorked", "PercentSalaryHike", 
                          "TotalWorkingYears", "YearsAtCompany", "YearsInCurrentRole", 
                          "YearsSinceLastPromotion", "YearsWithCurrManager")]


## 前處理結束 ##


### 1.當y軸為attrition=no，x軸為不同的factor變數的分布情況 ###
#建立一個新的data數據框來計算子類別的次數
selected_columns <- c("BusinessTravel", "Department", "EducationField", "EnvironmentSatisfaction", 
                      "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", 
                      "OverTime", "RelationshipSatisfaction", "Shift", "WorkLifeBalance","Attrition")
data2<-data[selected_columns]
data_yes <- data2[data2$Attrition == "Yes", ]
data_no <- data2[data2$Attrition == "No", ]



no.frequency <- data.frame()
#為每個X項目計算子類別的頻次並儲存
for (col in selected_columns) {
  temp_table <- table(data_no[[col]])
  temp_df <- as.data.frame(temp_table)
  temp_df$X_item <- col  # 添加X項目的名稱
  colnames(temp_df) <- c("SubCategory", "Freq", "X_item")  # 設定欄位名稱
  # 將結果合併到總資料框中
  no.frequency <- rbind(no.frequency, temp_df)
}
no.frequency<-no.frequency[no.frequency$X_item != "Attrition", ]

#no.frequency
ggplot(no.frequency, aes(x = X_item, y = Freq, fill = Freq)) +
  geom_bar(stat = "identity", position = "stack") +  # 堆疊長條圖
  geom_text(aes(label = paste(SubCategory, "\n", Freq)),  # 標示次數與子類別名稱
            position = position_stack(vjust = 0.5), size = 2.3) +  # 在長條中間顯示文本
  labs(title = "Attrition=No by X Item", 
       x = "X Items", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # X軸標籤旋轉
        panel.grid = element_blank(),  # 隱藏背景網格
        plot.margin = margin(2, 2, 2, 2)) +  # 調整邊距
  scale_fill_gradient(low = "blue", high = "red") 





### 2.當y軸為attrition=yes，x軸為不同的factor變數的分布情況 ###
yes.frequency <- data.frame()
#為每個X項目計算子類別的頻次並儲存
for (col in selected_columns) {
  temp_table <- table(data_yes[[col]])
  temp_df <- as.data.frame(temp_table)
  temp_df$X_item <- col  # 添加X項目的名稱
  colnames(temp_df) <- c("SubCategory", "Freq", "X_item")  # 設定欄位名稱
  # 將結果合併到總資料框中
  yes.frequency <- rbind(yes.frequency, temp_df)
}
yes.frequency<-yes.frequency[yes.frequency$X_item != "Attrition", ]

#繪製堆疊長條圖
#yes.frequency
ggplot(yes.frequency, aes(x = X_item, y = Freq, fill = Freq)) +
  geom_bar(stat = "identity", position = "stack") +  # 堆疊長條圖
  geom_text(aes(label = paste(SubCategory, "\n", Freq)),  # 標示次數與子類別名稱
            position = position_stack(vjust = 0.5), size = 2.3) +  # 在長條中間顯示文本
  labs(title = "Attrition=Yes by X Item", 
       x = "X Items", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # X軸標籤旋轉
        panel.grid = element_blank(),  # 隱藏背景網格
        plot.margin = margin(2, 2, 2, 2)) +  # 調整邊距
  scale_fill_gradient(low = "blue", high = "red")  # 顏色根據頻率從藍色到紅色漸變



# 恢復原始繪圖參數
par(mfrow = c(1, 1))


### 3. Mosaic Plot of JobSatisfaction, JobInvolvement, RelationshipSatisfaction, and WorkLifeBalance ###
 
xtabs(~JobSatisfaction+JobInvolvement+RelationshipSatisfaction+WorkLifeBalance, data=data)

# 建立列聯表
job_table_1 <- xtabs(~ JobSatisfaction + JobInvolvement + RelationshipSatisfaction + WorkLifeBalance, 
                   data = data)

# 繪製 Mosaic Plot
mosaicplot(job_table_1, 
           main = "Mosaic Plot of JobSatisfaction, JobInvolvement, RelationshipSatisfaction, and WorkLifeBalance", 
           color = TRUE,  # 使用顏色區分不同區塊
           xlab = "Job Satisfaction", 
           ylab = "Job Involvement",
           )       # 將軸標籤垂直顯示



### 4. Mosaic Plot of Shift, EnvironmentSatisfaction, JobSatisfaction, and RelationshipSatisfaction ### 

xtabs(~Shift+EnvironmentSatisfaction+JobSatisfaction+RelationshipSatisfaction, data=data)
# 建立列聯表
job_table_2 <- xtabs(~Shift+EnvironmentSatisfaction+JobSatisfaction+RelationshipSatisfaction, 
                   data = data)

# 繪製 Mosaic Plot
mosaicplot(job_table_2, 
           main = "Mosaic Plot of Shift, EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction", 
           color = TRUE,  # 使用顏色區分不同區塊
           xlab = "Shift", 
           ylab = "EnvironmentSatisfaction",
           )      



### 5. 繪製DailyRate和MonthlyIncome散布圖 ###
ggplot() +
  geom_point(data = data, aes(x = Age, y = DailyRate, color = "DailyRate")) +
  geom_point(data = data, aes(x = Age, y = MonthlyIncome, color = "MonthlyIncome")) +
  labs(title = "Age and DailyRate/MonthlyIncome", x = "Age", y = "Money") +
  scale_color_manual(values = c("DailyRate" = "blue", "MonthlyIncome" = "red"))



### 6. TrainingTimesLastYear and Department的長條圖 ###
ggplot(data, aes(x = TrainingTimesLastYear, fill = Department)) +
  geom_bar(position = "dodge") +  # 使用dodge讓長條並排顯示
  labs(title = "TrainingTimesLastYear and Department", x = "Training Time Last Year", y = "Department") +
  scale_fill_brewer(palette = "Set3")


#### 伍、Model ####

### 1. 原始模型 ###

# 移除原始變數
data$DistanceFromHome <- NULL
data$NumCompaniesWorked <- NULL
data$PercentSalaryHike <- NULL
data$TotalWorkingYears <- NULL
data$YearsAtCompany <- NULL
data$YearsInCurrentRole <- NULL
data$YearsSinceLastPromotion <- NULL
data$YearsWithCurrManager <- NULL

# 建立原始邏輯迴歸模型
logistic_model <- glm(Attrition ~ ., data = data, family = "binomial")

# 查看原始模型摘要
summary(logistic_model)

# 提取模型摘要
model_summary <- summary(logistic_model)

# 提取係數表（包含 p-value）
coefficients_table <- model_summary$coefficients

# 篩選 p-value 小於 0.05 的變數
significant_vars <- coefficients_table[coefficients_table[, "Pr(>|z|)"] < 0.05, ]

# 按 p-value 升序排序
sorted_significant_vars <- significant_vars[order(significant_vars[, "Pr(>|z|)"]), ]

# 顯示顯著特徵的排序結果
print(sorted_significant_vars)

# 提取顯著特徵名稱
significant_var_names <- rownames(sorted_significant_vars)
print(significant_var_names)

# 使用逐步迴歸選擇最佳模型
best_model <- step(logistic_model, direction = "both", trace = 1)

# 查看最佳模型摘要
summary(best_model)

# 提取最佳模型摘要
best_model_summary <- summary(best_model)

# 提取係數表（包含 p-value）
best_model_coefficients <- best_model_summary$coefficients

# 篩選 p-value 小於 0.05 的變數
best_significant_vars <- best_model_coefficients[best_model_coefficients[, "Pr(>|z|)"] < 0.05, ]

# 按 p-value 升序排序
best_sorted_significant_vars <- best_significant_vars[order(best_significant_vars[, "Pr(>|z|)"]), ]

# 顯示逐步迴歸模型顯著特徵的排序結果
print(best_sorted_significant_vars)

# 提取顯著特徵名稱
best_significant_var_names <- rownames(best_sorted_significant_vars)
print(best_significant_var_names)

# 檢查 Attrition 分佈（Yes/No 的次數）
attrition_counts <- table(data$Attrition)
print(attrition_counts)

# 計算每個類別的比例
attrition_proportions <- prop.table(attrition_counts)
print(attrition_proportions)

# 顯示比例並格式化成百分比
attrition_percentages <- round(attrition_proportions * 100, 2)
cat("離職比例 (百分比):\n")
print(attrition_percentages)

# 建立空模型（只有截距項）
null_model <- glm(Attrition ~ 1, data = data, family = "binomial")

# 提取空模型的 deviance 和自由度
null_deviance <- null_model$deviance
null_df <- null_model$df.residual

# 計算卡方統計量和自由度
chi_squared_original <- null_deviance - logistic_model$deviance
df_original <- null_df - logistic_model$df.residual

# 計算 p-value
p_value_original <- pchisq(chi_squared_original, df = df_original, lower.tail = FALSE)

# 輸出結果
cat("原始模型 vs 空模型:\n")
cat("卡方統計量 (Chi-squared):", chi_squared_original, "\n")
cat("自由度差異 (df):", df_original, "\n")
cat("p-value:", p_value_original, "\n")

# 判斷是否顯著
if (p_value_original < 0.05) {
  cat("結果：顯著\n")
} else {
  cat("結果：不顯著\n")
}

### 2. 對原模型做雙向逐步迴歸 ###

# 計算卡方統計量和自由度
chi_squared_stepwise <- null_deviance - best_model$deviance
df_stepwise <- null_df - best_model$df.residual

# 計算 p-value
p_value_stepwise <- pchisq(chi_squared_stepwise, df = df_stepwise, lower.tail = FALSE)

# 輸出結果
cat("\n逐步迴歸模型 vs 空模型:\n")
cat("卡方統計量 (Chi-squared):", chi_squared_stepwise, "\n")
cat("自由度差異 (df):", df_stepwise, "\n")
cat("p-value:", p_value_stepwise, "\n")

# 判斷是否顯著
if (p_value_stepwise < 0.05) {
  cat("結果：顯著\n")
} else {
  cat("結果：不顯著\n")
}

# 加回原始變數
data$DistanceFromHome <- original_vars$DistanceFromHome
data$NumCompaniesWorked <- original_vars$NumCompaniesWorked
data$PercentSalaryHike <- original_vars$PercentSalaryHike
data$TotalWorkingYears <- original_vars$TotalWorkingYears
data$YearsAtCompany <- original_vars$YearsAtCompany
data$YearsInCurrentRole <- original_vars$YearsInCurrentRole
data$YearsSinceLastPromotion <- original_vars$YearsSinceLastPromotion
data$YearsWithCurrManager <- original_vars$YearsWithCurrManager

### 主題一、不同職等下，加薪和升遷對降低離職率的效果是否不同？公司是否有最適薪資結構？ ###

# 選擇數值型變數，並排除 Attrition 
numeric_data <- data %>%
  select(where(is.numeric)) 

# 計算相關係數矩陣
cor_matrix <- cor(numeric_data, use = "complete.obs")

# 找出高相關變數
high_corr_pairs <- which(abs(cor_matrix) > 0.6 & abs(cor_matrix) < 1, arr.ind = TRUE)

# 將結果轉為資料框，方便後續處理
high_corr_df <- data.frame(
  Var1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Var2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

# 移除重複的對（例如，A-B 和 B-A）
high_corr_df <- high_corr_df[high_corr_df$Var1 < high_corr_df$Var2, ]
print(high_corr_df)

selected_variables <- c(
  "Age", "BusinessTravel", "Department", "EducationField", 
  "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobRole", 
  "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "MonthlyRate", 
  "OverTime", "RelationshipSatisfaction", "Shift","TrainingTimesLastYear", "WorkLifeBalance","Log_DistanceFromHome", "Log_NumCompaniesWorked",
  "Log_TotalWorkingYears", "Log_YearsInCurrentRole", "Log_YearsSinceLastPromotion", "Log_YearsWithCurrManager"
)
# 動態生成公式
formula <- as.formula(paste("Attrition ~", paste(selected_variables, collapse = " + ")))

# 印出公式確認
print(formula)

# 計算每個 JobLevel 的數量
job_level_counts <- data %>%
  group_by(JobLevel) %>%
  summarise(Count = n())

# 繪製不同 JobLevel 的 MonthlyIncome 分布圖，並加上數量標註
library(ggplot2)

job_level_attrition <- data %>%
  group_by(JobLevel) %>%
  summarise(
    AttritionYesRatio = mean(Attrition == "Yes", na.rm = TRUE),
    Count = n()
  )
print(job_level_attrition)
# 繪製箱型圖並加上平均數
ggplot(data, aes(x = JobLevel, y = MonthlyIncome, fill = JobLevel)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +  # 繪製箱型圖
  stat_summary(fun = mean, geom = "point", color = "purple", size = 3) +  # 顯示平均數為點
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
               vjust = -1, color = "purple") +  # 標註平均數
  stat_summary(fun = max, geom = "text", aes(label = round(..y.., 2)),
               vjust = -0.5, color = "blue") +  # 標註最大值
  stat_summary(fun = min, geom = "text", aes(label = round(..y.., 2)),
               vjust = 1.5, color = "darkgreen") +  # 標註最小值
  geom_text(data = job_level_counts, aes(x = JobLevel, y = max(data$MonthlyIncome, na.rm = TRUE), 
                                         label = paste("Count:", Count)),
            vjust = -1, color = "black") +  # 加上每個 JobLevel 的數量標註
  labs(title = "Income Distribution by Job Level",
       x = "Job Level",
       y = "Monthly Income") +
  theme_minimal()


data_level1 <- data %>% filter(JobLevel == 1)

# 動態生成公式，移除 JobLevel
formula <- as.formula(
  paste(
    "Attrition ~", 
    paste(selected_variables[selected_variables != "JobLevel"], collapse = " + ")
  )
)
print(formula)
# 執行邏輯回歸，檢查 Income 是否影響 Attrition
model_income_attrition <- glm(formula, 
                              data = data_level1, 
                              family = binomial)
# 顯示回歸結果
summary(model_income_attrition)

data_level1 <- data %>% filter(JobLevel == 1| JobLevel == 2)
formula <- as.formula(
  paste(
    "Attrition ~", 
    paste(selected_variables, collapse = " + ")
  )
)

# 執行邏輯回歸，檢查 Income 是否影響 Attrition
model_income_attrition <- glm(formula, 
                              data = data_level1, 
                              family = binomial)
# 顯示回歸結果
summary(model_income_attrition)


### 主題二、工作滿意度與離職顯著相關，何種因素對於工作滿意度的影響最大？ ###

# 移除原始變數
data$DistanceFromHome <- NULL
data$NumCompaniesWorked <- NULL
data$PercentSalaryHike <- NULL
data$TotalWorkingYears <- NULL
data$YearsAtCompany <- NULL
data$YearsInCurrentRole <- NULL
data$YearsSinceLastPromotion <- NULL
data$YearsWithCurrManager <- NULL

# 確保必要的變數為 factor 類型
data$JobSatisfaction <- as.factor(data$JobSatisfaction)
data$OverTime <- as.factor(data$OverTime)
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)
data$EnvironmentSatisfaction <- as.factor(data$EnvironmentSatisfaction)
data$RelationshipSatisfaction <- as.factor(data$RelationshipSatisfaction)

# 新的基礎模型（無交互項）
new_base_model <- glm(
  Attrition ~ JobSatisfaction + OverTime + WorkLifeBalance + Age + 
    EnvironmentSatisfaction + RelationshipSatisfaction + 
    Log_DistanceFromHome + Log_YearsWithCurrManager + 
    Log_NumCompaniesWorked + Log_TotalWorkingYears + 
    TrainingTimesLastYear,
  data = data,
  family = "binomial"
)

# 查看基礎模型摘要
summary(new_base_model)

# 添加交互項並構建新模型
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

# 查看新模型摘要
summary(interaction_model1)

# 添加交互項並構建新模型
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

# 查看新模型摘要
summary(interaction_model2)

# 比較基礎模型和新模型的 AIC
cat("New Base Model AIC:", AIC(new_base_model), "\n")
cat("Interaction Model1 AIC:", AIC(interaction_model1), "\n")
cat("Interaction Model2 AIC:", AIC(interaction_model2), "\n")

### 主題三、驗證環境滿意度在不同部門對離職率的影響 ###

# 類別變數列表
cat_vars <- c("Department", "EnvironmentSatisfaction")

# 函數：計算各類別的離職率
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

# 對每個類別變數計算離職率
attrition_analysis <- lapply(cat_vars, function(var) {
  calculate_attrition_rate(data, var)
})

# 繪製離職率柱狀圖
plot_attrition_rate <- function(data, var) {
  ggplot(data, aes(x = !!sym(var), y = attrition_rate)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = paste("Attrition Rate by", var),
         x = var, 
         y = "Attrition Rate (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 生成圖表
plots <- lapply(seq_along(cat_vars), function(i) {
  plot_attrition_rate(attrition_analysis[[i]], cat_vars[i])
})
plots


# 加入EnvironmentSatisfaction 跟 Department的交互作用
best_model_interact <- glm(formula = Attrition ~ Age + BusinessTravel + DailyRate + 
                             Department + EducationField + EnvironmentSatisfaction + JobInvolvement + 
                             JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + 
                             OverTime + RelationshipSatisfaction + Shift + TrainingTimesLastYear + 
                             WorkLifeBalance + Log_DistanceFromHome + Log_NumCompaniesWorked + 
                             Log_TotalWorkingYears + Log_YearsInCurrentRole + Log_YearsSinceLastPromotion + 
                             Log_YearsWithCurrManager + EnvironmentSatisfaction:Department, family = "binomial", data = data)
summary(best_model_interact)



# 創建contingency table
contingency_table <- table(data$Department, 
                           data$EnvironmentSatisfaction, 
                           data$Attrition)


print(contingency_table)

# 對每個部門進行卡方檢驗
departments <- unique(data$Department)

chi_square_results <- lapply(departments, function(dept) {
  dept_data <- data[data$Department == dept, ]
  
  
  cont_table <- table(
    dept_data$EnvironmentSatisfaction, 
    dept_data$Attrition
  )
  
  # 卡方檢驗
  chi_test <- chisq.test(cont_table)
  
  return(list(
    Department = dept,
    Chi_Square = chi_test$statistic,
    P_Value = chi_test$p.value
  ))
})

# 轉換為dataframe以便閱讀
chi_results_df <- do.call(rbind, chi_square_results)
print(chi_results_df)

library(ggplot2)
library(dplyr)

# 繪製每個部門環境滿意度與離職率的關係
ggplot(data, aes(x = factor(EnvironmentSatisfaction), fill = Attrition)) +
  geom_bar(position = "fill") +  
  facet_wrap(~ Department) +
  labs(
    title = "Attrition Proportion by Environment Satisfaction and Department",
    x = "Environment Satisfaction",
    y = "Proportion"
  ) +
  theme_minimal()

# 捨棄其他變數的EnvironmentSatisfaction 跟 Department交互作用模型
interact_model <- glm(
  Attrition ~ EnvironmentSatisfaction * Department, 
  data = data, 
  family = binomial
)
summary(interact_model)
anova(best_model, best_model_interact, test="Chisq")

# 計算環境滿意度的勝算比
coefficients <- c(-1.746, -2.702, -3.230) # 輸入模型中的係數
odds_ratios <- exp(coefficients)          # 計算勝算比
odds_ratios