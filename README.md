# 💳 Credit Scoring Project

## 🎯 Objective
The goal of this project is to predict the probability of default (PD) for bank customers and identify key risk factors.

---

## 📊 Data
The project uses 3 datasets:
- Customer credit details
- Customer information
- Customer profiles

All datasets were merged into a master dataset using `custid`.

---

## 🧹 Data Preparation
- Removed duplicates and invalid values
- Handled missing values
- Recoded categorical variables
- Created grouped variables (e.g., debt-to-income categories)

---

## 📈 Exploratory Data Analysis (EDA)

Key insights:
- 📍 Zone is the strongest predictor of default (variation >15%)
- 💰 High debt-to-income ratio increases default risk
- 🏦 Having a deposit/account does NOT reduce risk significantly
- 👨‍👩‍👧 Demographic variables have moderate influence

---

## 🤖 Models Used

### Logistic Regression
- AUC (Test): **0.7566**
- No multicollinearity (VIF < 1.8)
- Interpretable and stable model

### Machine Learning Models
| Model | AUC (Test) |
|------|----------|
| Logistic Regression | 0.7566 |
| Naive Bayes | 0.7423 |
| Decision Tree | 0.7381 |
| Random Forest | 0.7885 |

---

## ⚠️ Model Selection

Although Random Forest has the highest AUC,
it fails to detect defaulters (very low sensitivity).

👉 Final Model: **Logistic Regression**

---

## 💬 Customer Feedback Analysis

Sentiment analysis shows:
- Positive: fast processing, helpful staff
- Negative: delays, high interest rates, poor communication

👉 Recommendation:
Improve processing speed and transparency

---

## 🏁 Conclusion

The model can be used to:
- Identify high-risk customers
- Improve credit approval decisions
- Reduce default rates
