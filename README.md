# Medical Staff Attrition Analysis

A statistical study of what drives healthcare staff turnover, using exploratory data analysis and logistic regression in R. The goal is to identify the key factors behind nursing staff attrition and turn them into practical retention recommendations for hospitals.

> 📄 Full write-up: [Medical Staff Attrition Analysis Report.pdf](./Medical%20Staff%20Attrition%20Analysis%20Report.pdf)

## Motivation

Healthcare workforce shortages are growing in both the U.S. and Taiwan. Mercer (2021) projected a U.S. shortfall of more than 400,000 home care workers and 29,400 specialized nurses by 2025, and Taiwan's nursing gap reached an estimated 16,000–24,000 people in 2023. This project looks at which factors drive turnover so institutions can act on them.

## Dataset

- **Source:** [Employee Attrition for Healthcare (Kaggle)](https://www.kaggle.com/datasets/jpmiller/employee-attrition-for-healthcare), a healthcare adaptation of the IBM Watson HR dataset (`watson_healthcare_modified.csv`)
- **Size:** 1,676 records, 35 columns
- **Target:** `Attrition` (Yes ≈ 12%, No ≈ 88%)
- **Features:** demographics, department (Cardiology / Maternity / Neurology), job level and role, income, overtime, shift, distance from home, tenure, and several satisfaction scores

> The CSV is not included in this repo. Download it from Kaggle before running the script.

## Methodology

1. **Preprocessing**
   - Dropped constant or identifier columns: `EmployeeCount`, `Over18`, `StandardHours`, `EmployeeID`
   - Converted ordinal and categorical fields (Education, satisfaction scores, JobLevel, Shift, and others) to factors
   - Applied `log(x + 1)` to right-skewed variables such as `DistanceFromHome`, `TotalWorkingYears` and the `Years*` fields
2. **Exploratory data analysis**: skewness and histograms, factor distributions by attrition status, mosaic plots of the satisfaction variables, and income/age/training plots
3. **Modeling**
   - Full logistic regression (`glm(Attrition ~ ., family = "binomial")`) tested against the null model with a likelihood-ratio χ² test
   - Bidirectional stepwise selection by AIC to get the best model
4. **Three research themes**, covered in the next section

## Key Findings

### Theme 1: Salary vs. promotion across job levels
- Monthly income significantly lowers attrition **only at Job Level 1**. It has no significant effect at Levels 2–5.
- Promotion from **Level 1 to 2 significantly reduces** turnover, while **Level 2 to 3 increases** it, possibly because more outside opportunities open up at that level.
- For entry-level staff, matching the retention effect of one promotion would take about **$7,436/month** in extra pay (−2.93 / −0.000394). Promotion is far more cost-effective.
- A marginal cost–benefit analysis (turnover cost ≈ 200% of annual salary, 5% discount rate) finds that raising entry-level pay costs more than the turnover it prevents, assuming constant marginal effects.

### Theme 2: Drivers related to job satisfaction
- The base model plus interaction terms was compared by AIC. **Interaction Model 2** was selected.
- Significant interactions:
  - `JobSatisfaction × WorkLifeBalance`: high job satisfaction offsets the effect of poor work-life balance.
  - `NumCompaniesWorked × TotalWorkingYears`: staff who have changed jobs often **and** have long careers are more likely to leave.
- Emotional factors such as environment, relationships, overtime and commute distance all matter for retention.

### Theme 3: Environment satisfaction across departments
- Environment satisfaction of 1 has clearly higher attrition. Cardiology has the highest attrition of the three departments.
- Chi-square tests per department show that the effect is significant only in **Maternity**. The `EnvironmentSatisfaction × Department` interaction is not significant in the full model.
- Odds ratios compared with satisfaction level 1: level 2 → **−82.6%**, level 3 → **−93.3%**, level 4 → **−96.1%** turnover odds.

## Recommendations

- Put compensation efforts into **entry-level (Level 1)** staff, and prefer **promotion** over pay raises where Level 2 positions exist.
- Be careful with Level 2 → 3 promotions, and make sure the employee has a strong sense of belonging first.
- Create growth and incentive programs for **experienced staff who have changed jobs often**.
- Improve the **work environment**: a collaborative culture, recognition events, fair evaluations, break rooms, fitness facilities and ergonomic workspaces.

## Repository Structure

```
.
├── Attrition_Analysis_Regression_Model.R      # Full pipeline: preprocessing, EDA, models, themes 1–3
├── Medical Staff Attrition Analysis Report.pdf # Full report with figures and model outputs
└── README.md
```

## How to Run

1. Download `watson_healthcare_modified.csv` from Kaggle.
2. Edit the `setwd(...)` line at the top of `Attrition_Analysis_Regression_Model.R` so it points to the folder that holds the CSV.
3. Install the dependencies:
   ```r
   install.packages(c("ggplot2", "tidyr", "dplyr", "RColorBrewer", "vcd", "GGally", "moments"))
   ```
4. Run the script in R or RStudio.

## Limitations

The data comes from a single, partly synthetic source (an adaptation of IBM Watson) and may not represent all healthcare institutions. Small attrition counts at higher job levels caused convergence problems in some subgroup models.

## Authors

Group 11: Richard Wen, Ting-Wei Xu, Zhi-Xian Li, Hong-Ning Ruan, Yu-Ru Wang, Xiang-Yu Lin, Cheng-Ying Gao
