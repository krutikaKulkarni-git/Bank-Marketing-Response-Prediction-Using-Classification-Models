#Bank Marketing Response Prediction Using Classification Models

1. Problem Description

The objective of this project is to analyze a bank marketing dataset and build classification models to predict whether a customer will subscribe to a term deposit (y = yes/no). The dataset contains demographic, financial, and campaign-related attributes collected during direct marketing campaigns conducted by a bank.

The key goal is to develop predictive models using only customer-related attributes, avoiding variables that introduce data leakage (i.e., variables that are known only after contacting the client).

2. Dataset Description

Dataset name: Bank Marketing Dataset

Observations: 45,211

Variables: 17

Target variable: y (subscription outcome: yes/no)

Missing values: None

The dataset is imbalanced, with approximately:

88% of observations labeled as no

12% labeled as yes

This imbalance requires careful consideration during model training and evaluation.

3. Data Preprocessing
3.1 Feature Selection

To avoid data leakage and ensure realistic prediction, only customer-related variables were used for modeling.

Excluded variables:

contact

day

month

duration

campaign

pdays

previous

poutcome

These variables either describe campaign execution details or contain information unavailable at prediction time.

3.2 Final Modeling Features

Examples of retained variables include:

age

job

marital

education

default

balance

housing

loan

4. Exploratory Data Analysis (EDA)

Key findings from EDA include:

Subscription rates vary across age groups, with relatively higher response rates among younger and older clients.

The number of campaign contacts per client is generally low (1–3 calls).

Call duration shows strong correlation with subscription outcome, but is excluded due to data leakage concerns.

Class imbalance is evident and influences model evaluation strategy.

5. Train–Test Split

The dataset was split into:

70% training set

30% testing set

The split was performed using random sampling to ensure representative class distribution.

6. Models Implemented
6.1 Decision Tree (CART – rpart)

Trained using rpart() with method = "class"

Class priors were adjusted to handle imbalance

Cost-complexity pruning was applied using cross-validation

Final tree selected using the 1-SE rule

6.2 C5.0 Decision Tree and Rule-Based Model

Implemented using the C50 package

Both tree and rule-based models were evaluated

A cost matrix was applied to penalize false negatives more heavily

6.3 Random Forest

Implemented using the randomForest package

Multiple values of ntree (200, 500, 1000) were tested

Variable importance measures were analyzed

Different probability thresholds were evaluated to address class imbalance

7. Model Evaluation Metrics

Given the imbalanced nature of the dataset, multiple evaluation metrics were used:

Confusion Matrix

Accuracy

Precision and Recall

ROC Curve and AUC

Lift and Decile Lift Analysis

Lift analysis was used to assess how effectively models rank customers by likelihood of subscription, which is especially relevant for marketing decision-making.

8. Results and Observations

Handling class imbalance (via priors, costs, or thresholds) significantly impacts model performance.

Pruned decision trees generalize better than unpruned trees.

Lift and decile analysis provide more business-relevant insight than accuracy alone.

Ensemble methods such as Random Forest demonstrate improved predictive stability.

9. Implementation Details
Software and Tools

Language: R

IDE: RStudio

R Packages Used

tidyverse

rpart, rpart.plot

C50

randomForest

ROCR
