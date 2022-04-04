logistic_regression_and_tree_based_techniques

In the BMC Medical Informatics and Decision Making journal, a recent article from February 2020
entitled “Machine learning can predict survival of patients with heart failure from serum creatinine and
ejection fraction alone” demonstrates machine learning applied to medical records to predict the survival
of each patient having heart failure symptoms. The twelve clinical features included in the associated
data and located in the course website are as follows:
•age: age of the patient (years)
•anaemia: decrease of red blood cells or hemoglobin (boolean)
•high blood pressure: if the patient has hypertension (boolean)
•creatinine phosphokinase (CPK): level of the CPK enzyme in the blood (mcg/L)
•diabetes: if the patient has diabetes (boolean)
•ejection fraction: percentage of blood leaving the heart at each contraction (percentage)
•platelets: platelets in the blood (kiloplatelets/mL)
•sex: woman or man (binary)
•serum creatinine: level of serum creatinine in the blood (mg/dL)
•serum sodium: level of serum sodium in the blood (mEq/L)
•smoking: if the patient smokes or not (boolean)
•death (target variable): if the patient deceased during the follow-up period (boolean)

classification modeling was done to predict death based on the provided features. The primary
goal was to build a meaningful, interpretable model with good predictive performance. Two
models were built: (i) logistic regression (or variant) and (ii) a decision tree.
