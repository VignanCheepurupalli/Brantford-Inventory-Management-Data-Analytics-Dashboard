# Brantford Inventory Management — Data Analytics & ML Dashboard

A data analytics and machine learning project focused on optimizing inventory management for laptops at the Brantford location. Built as part of the Master of Applied Computing program at Wilfrid Laurier University.

---

## Tech Stack

- **Language:** R
- **Dashboard:** R Shiny
- **Libraries:** ggplot2, dplyr, randomForest, rpart, caret, forecast, reshape2, class

---

## Project Overview

The project analyzes Amazon laptop sales data (August–October 2024) to identify demand patterns, forecast future purchases, and recommend optimal inventory levels. The analysis covers the full pipeline from raw data cleaning to an interactive dashboard.

---

## Files

| File | Description |
|---|---|
| `DAFullCode.R` | Full analysis pipeline — data cleaning, EDA, all ML models |
| `Dashboard.R` | Interactive Shiny dashboard with brand and price filters |
| `DA-Report.pdf` | Final project report with methodology, findings, and recommendations |

---

## Features

### Exploratory Data Analysis
- Distribution of product ratings (histogram)
- Monthly purchase trends across August, September, and October (bar chart)
- Correlation heatmap across Price, Stars, Rating_count, and purchase volumes
- Product count by brand (horizontal bar chart)
- Stock vs. purchases scatter plot

### Machine Learning Models
- **Linear Regression** — predicts inventory levels based on price, ratings, and purchase history
- **Decision Tree** — pruned regression tree; Purchase_September identified as the most important predictor (52% importance)
- **K-Nearest Neighbors (KNN)** — predicts total purchases using scaled features, k=5, 80/20 train-test split
- **Random Forest** — 100 trees, explains 95.44% of variance in total purchases (MSE: 0.023)
- **Time Series Analysis** — combines 3-month sales data to visualize and forecast demand trends

### Inventory Optimization
- **ABC Classification** — categorizes 1,182 products into A (top 10%), B (next 30%), C (remaining 60%) based on purchase volume
- **Safety Stock Calculation** — computes buffer stock using 95% service level (Z=1.65) and 2-month lead time (result: 3.36 units)
- **Reorder Point Optimization** — determines restocking trigger point (result: 5.12 units)

### Interactive Shiny Dashboard
- Filter by brand and price range
- Four tabs: Overview, Correlation, Brand Analysis, Inventory Analysis
- Live safety stock and reorder point calculations based on filtered data

---

## Dataset

The analysis uses an Amazon laptop dataset (`Amazon-final.csv`) containing 1,182 products with columns including ASIN, Brand, Price, Stars, Rating_count, In_stock, Purchase_August, Purchase_September, and Purchase_October.

> The dataset is not included in this repository due to source restrictions. To run the R scripts, place `Amazon-final.csv` in the same directory as the R files.

---

## How to Run

### Analysis script
1. Install R and RStudio
2. Install required packages:
```r
install.packages(c("readr", "dplyr", "ggplot2", "rpart", "rpart.plot",
                   "class", "randomForest", "forecast", "reshape2", "caret"))
```
3. Place `Amazon-final.csv` in the project folder
4. Open and run `DAFullCode.R` in RStudio

### Shiny Dashboard
1. Install Shiny: `install.packages("shiny")`
2. Place `Amazon-final.csv` in the project folder
3. Open `Dashboard.R` in RStudio and click **Run App**
