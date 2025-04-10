# Tipping Behavior in Ride-Sharing Services – Applied Statistics Project

**Author:** Georg Khella  
**Course:** Applied Statistics – MATH-516  
**Date:** March 2025

## 🚗 Overview

This project analyzes factors influencing **tipping behavior** in ride-sharing services, using a large dataset from Chicago TNPs. It applies **logistic regression** and **decision trees** to model the probability of tipping based on trip attributes.

## 🧾 Dataset

- ~4.5 million records of ride-sharing trips (Oct 2024)
- Key variables: Fare, Tip, Distance, Duration, Shared Ride, Time of Day, Dropoff Location

## 🧠 Methods

- **Logistic Regression (GLM):**
  - Feature engineering, VIF filtering, MLE estimation
  - Final model includes log Fare, log Trip Miles, ride type, dropoff area, time
- **Decision Tree:**
  - Gini-based splitting, feature importance, recursive partitioning
- **Model Evaluation:**
  - Accuracy, ROC/AUC, 5-fold Cross-Validation
  - Confusion matrix analysis to assess imbalance

## 📈 Key Findings

- Tipping occurs in ~25.8% of rides
- **Higher fares** and **longer distances** → higher tip probability
- **Shared rides** significantly reduce tipping
- **Time of day** and **dropoff location** matter
- Logistic regression: AUC ≈ 0.79 (better than decision tree's AUC ≈ 0.74)

## 📁 Files

- `Project2_Georg_Khella.pdf`: Full report with data analysis, model details, and results

## 📚 References

- Dobson & Barnett (2018) – GLMs  
- Breiman et al. (1984) – CART  
- Hastie et al. (2009) – Elements of Statistical Learning  
- Mhalla (2025) – MATH-516 Course Notes
