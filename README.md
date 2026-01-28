# Bank Marketing Response Prediction Using Classification Models

## 1. Problem Description

The objective of this project is to analyze a bank marketing dataset and develop classification models to predict whether a customer will subscribe to a term deposit (`y = yes/no`). The dataset consists of demographic, financial, and campaign-related attributes collected during direct marketing campaigns conducted by a bank.

The primary goal is to build predictive models using **only customer-related variables**, while excluding variables that may introduce data leakage. This ensures that the models reflect realistic decision-making scenarios where predictions must be made before customer contact.

---

## 2. Dataset Description

- **Dataset Name:** Bank Marketing Dataset  
- **Number of Observations:** 45,211  
- **Number of Variables:** 17  
- **Target Variable:** `y` (subscription outcome: yes/no)  
- **Missing Values:** None  

The dataset is highly imbalanced, with approximately 88% of observations labeled as `no` and 12% labeled as `yes`. This imbalance influences model training, evaluation, and threshold selection.

---

## 3. Data Preprocessing

### 3.1 Feature Selection and Leakage Handling

To prevent information leakage, variables that are only known during or after customer contact were excluded from the modeling process.

**Excluded Variables:**
- `contact`
- `day`
- `month`
- `duration`
- `campaign`
- `pdays`
- `previous`
- `poutcome`

### 3.2 Final Modeling Variables

The final set of predictors consists of customer-specific attributes, including:
- `age`
- `job`
- `marital`
- `education`
- `default`
- `balance`
- `housing`
- `loan`

---

## 4. Exploratory Data Analysis (EDA)

Exploratory data analysis was performed to understand variable distributions and relationships with the target variable. Key observations include:

- Subscription rates vary across age groups, with relatively higher response rates among younger and older customers.
- Most customers were contacted a small number of times (typically 1–3).
- Call duration shows a strong relationship with subscription outcome, but was excluded due to leakage concerns.
- The class imbalance necessitates the use of evaluation metrics beyond accuracy.

---

## 5. Train–Test Split

The dataset was randomly divided into:
- **70% Training Set**
- **30% Testing Set**

This split ensures sufficient data for model training while maintaining a representative sample for evaluation.

---

## 6. Models Implemented

### 6.1 Decision Tree (CART – rpart)

- Implemented using the `rpart` package
- Trained with `method = "class"`
- Class priors were adjusted to address class imbalance
- Cost-complexity pruning was applied using cross-validation
- The final tree was selected using the one standard error (1-SE) rule

---

### 6.2 C5.0 Decision Tree and Rule-Based Model

- Implemented using the `C50` package
- Both decision tree and rule-based models were evaluated
- A cost matrix was applied to penalize false negatives more heavily

---

### 6.3 Random Forest

- Implemented using the `randomForest` package
- Models were trained using different values of `ntree` (200, 500, and 1000)
- Variable importance measures were analyzed
- Alternative probability thresholds were evaluated to improve performance on the minority class

---

## 7. Model Evaluation

Due to the imbalanced nature of the dataset, multiple evaluation metrics were used:

- Confusion Matrix
- Accuracy
- Precision and Recall
- Receiver Operating Characteristic (ROC) Curve
- Area Under the ROC Curve (AUC)
- Lift and Decile Lift Analysis

Lift analysis was used to assess how effectively models rank customers based on their likelihood of subscription, which is particularly relevant for marketing applications.

---

## 8. Results and Observations

- Addressing class imbalance significantly impacts model performance.
- Pruned decision trees demonstrate improved generalization compared to unpruned trees.
- Ensemble methods such as Random Forest provide more stable and robust predictions.
- Lift and decile analysis offer more actionable insights than accuracy alone for marketing decision-making.

---

## 9. Implementation Details

### Software and Tools
- **Programming Language:** R  
- **Development Environment:** RStudio  

### R Packages Used
- `tidyverse`
- `rpart`
- `rpart.plot`
- `C50`
- `randomForest`
- `ROCR`

---


## 10. Files Included in This Repository

### Required Files
- `bank-full.csv` – Dataset used for analysis
- `classification models bankMarketing.Rmd` – Main analysis notebook
- `Assigment 1.R` – Script-based implementation
- `Data Mining (Assignment 1).pdf` – Final project report
- `README.md`

### Optional Files
- `.gitignore`
- Exported figures (ROC curves, lift charts)
- Model comparison summary tables

---

## 11. Conclusion

This project demonstrates the application of classification techniques to a real-world bank marketing dataset. Emphasis is placed on appropriate feature selection, handling of class imbalance, and evaluation using business-relevant metrics. The results highlight the importance of model interpretability and ranking performance in customer targeting problems.

