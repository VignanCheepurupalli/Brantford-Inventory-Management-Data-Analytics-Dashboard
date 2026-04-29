# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(class)
library(randomForest)
library(forecast)
library(reshape2)
library(caret)

# Read the data
data <- read.csv("Amazon-final.csv")

# Summarize the data
summary(data)

# Clean and convert `Price` to numeric
data$Price <- as.numeric(gsub("[$,]", "", data$Price))

# Clean and convert `Rating_count` to numeric
data$Rating_count <- as.numeric(gsub(",", "", data$Rating_count))

# Check for remaining non-numeric columns
str(data)

# Step 1.1: Remove irrelevant columns
data <- data %>% select(-`...17`)

# Step 1.2: Clean and convert `Price` to numeric
data$Price <- as.numeric(gsub("[$,]", "", data$Price))

# Step 1.3: Check for missing values
print("Missing Values Before Cleaning:")
print(colSums(is.na(data)))

# Fill missing values in `Stars` and `Rating_count` with their mean
data$Stars[is.na(data$Stars)] <- mean(data$Stars, na.rm = TRUE)
data$Rating_count[is.na(data$Rating_count)] <- mean(data$Rating_count, na.rm = TRUE)

# Fill missing values in `Purchase_September` with its mean
data$Purchase_September[is.na(data$Purchase_September)] <- mean(data$Purchase_September, na.rm = TRUE)

# Fill missing values in `Ship` and `Seller` with their mode
mode_value <- function(x) {
  uniq_vals <- unique(x[!is.na(x)])
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

data$Ship[is.na(data$Ship)] <- mode_value(data$Ship)
data$Seller[is.na(data$Seller)] <- mode_value(data$Seller)

# Verify data after handling missing values
cat("\nMissing Values After Handling:\n")
print(colSums(is.na(data)))

# Check the structure of the cleaned dataset
str(data)

# 1. Distribution of Product Ratings
ggplot(data, aes(x = Stars)) +
  geom_histogram(binwidth = 0.1, fill = "dodgerblue", color = "black") +
  labs(title = "Distribution of Product Ratings (Stars)", x = "Stars", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 2. Monthly Purchases (Bar Chart)
monthly_purchases <- colSums(data[, c("Purchase_August", "Purchase_September", "Purchase_October")])
monthly_df <- data.frame(Month = names(monthly_purchases), Purchases = monthly_purchases)

ggplot(monthly_df, aes(x = Month, y = Purchases, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Monthly Purchases", x = "Months", y = "Total Purchases") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")

# Correlation Heatmap
numeric_data <- data[, c("Stars", "Rating_count", "Price", "Purchase_August", "Purchase_September", "Purchase_October", "In_stock")]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
heatmap <- melt(correlation_matrix)

ggplot(heatmap, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap with Values", x = "Variables", y = "Variables") +
  theme_minimal()

# Count of Products by Brand
brand_counts <- data %>%
  group_by(Brand) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

ggplot(brand_counts, aes(x = reorder(Brand, -Count), y = Count, fill = Brand)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Count of Products by Brand", x = "Brand", y = "Count") +
  theme_minimal()

# Summing purchases across months
data$Total_Purchases <- data$Purchase_August + data$Purchase_September + data$Purchase_October
print(data$Total_Purchases)

# Scatter plot of Stock vs. Purchases
ggplot(data, aes(x = In_stock, y = Total_Purchases)) + 
  geom_point(color = "purple") + 
  labs(title = "Stock vs. Purchases", x = "In Stock", y = "Total Purchases") + 
  theme_minimal()

# ---- Model Implementations ----

# 1. Linear Regression Model (Predicting Total Purchases)
lm_model <- lm(In_stock ~ Price + Rating_count + Stars + Purchase_August + Purchase_September + Purchase_October, data = data)
summary(lm_model)


# 2. Decision Tree Model (Predict Total Purchases)
data <- na.omit(data)

data$Total_Purchases <- as.numeric(data$Total_Purchases)

# Fit a decision tree model
decision_tree <- rpart(Total_Purchases ~ Price + Rating_count + Stars + In_stock + 
                         Purchase_August + Purchase_September + Purchase_October, 
                       data = data)

# Print a summary of the decision tree
summary(decision_tree)

# Visualize the decision tree
rpart.plot(decision_tree, type = 3, extra = 1, box.palette = "RdBu", main = "Decision Tree for Total Purchases")

# Make predictions on the same data (or you can use a separate test dataset)
predictions <- predict(decision_tree, data)

# Calculate Mean Squared Error (MSE)
mse <- mean((predictions - data$Total_Purchases)^2, na.rm = TRUE)

# Display the MSE
cat("Mean Squared Error (MSE):", mse)

# Optionally, you can tune the decision tree model by adjusting the parameters
decision_tree_pruned <- rpart(Total_Purchases ~ Price + Rating_count + Stars + In_stock + 
                                Purchase_August + Purchase_September + Purchase_October, 
                              data = data, cp = 0.01)

# Visualize the pruned tree
rpart.plot(decision_tree_pruned, type = 3, extra = 1, box.palette = "RdBu", main = "Pruned Decision Tree for Total Purchases")

# Make predictions using the pruned model
predictions_pruned <- predict(decision_tree_pruned, data)

# Calculate MSE for the pruned model
mse_pruned <- mean((predictions_pruned - data$Total_Purchases)^2, na.rm = TRUE)

# Display the MSE for the pruned model
cat("Pruned Model Mean Squared Error (MSE):", mse_pruned)

# 4. K-Nearest Neighbors Model (Predict Total Purchases)
data_scaled <- scale(data[, c("Price", "Rating_count", "Stars", "In_stock", "Purchase_August", "Purchase_September", "Purchase_October")])
set.seed(42)
train_indices <- sample(1:nrow(data), nrow(data)*0.8)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
train_scaled <- data_scaled[train_indices, ]
test_scaled <- data_scaled[-train_indices, ]
knn_pred <- knn(train_scaled, test_scaled, train_data$Total_Purchases, k = 5)
print(knn_pred)

# 5. Random Forest Model (Predict Total Purchases)
rf_model <- randomForest(Total_Purchases ~ Price + Rating_count + Stars + In_stock + Purchase_August + Purchase_September + Purchase_October, 
                         data = data, ntree = 100)

# Print the Random Forest model summary
print(rf_model)

# Predict total purchases using the trained model
rf_pred <- predict(rf_model, test_data)

# Calculate RMSE (Root Mean Squared Error)
rf_rmse <- sqrt(mean((rf_pred - test_data$Total_Purchases)^2))
cat("Random Forest RMSE:", rf_rmse, "\n")


# ---- Find the Brand with the Highest Purchases ----
brand_total_purchases <- data %>%
  group_by(Brand) %>%
  summarise(Total_Purchases = sum(Total_Purchases)) %>%
  arrange(desc(Total_Purchases))

# Print the brand with highest total purchases
print(brand_total_purchases)

# Top Brand by Purchases
top_brand <- brand_total_purchases %>%
  top_n(1, Total_Purchases)

cat("\nTop Brand with the Highest Purchases:\n")
print(top_brand)

#  Calculate Inventory for Top Brand
top_brand_inventory <- data %>%
  filter(Brand == top_brand$Brand) %>%
  summarise(Required_Inventory = max(Total_Purchases)) # You can adjust this with different logic if needed.

# Print the required inventory for the top brand
cat("\nRequired Inventory for Top Brand:\n")
print(top_brand_inventory)

# 6. Demand Forecasting using Time Series for combined three months
monthly_sales <- colSums(data.frame(August = data$Purchase_August,
                                    September = data$Purchase_September,
                                    October = data$Purchase_October))
combine_month_sales <- ts(as.vector(t(monthly_sales)), start = c(2024, 8), frequency = 12)

bar_center <-barplot(monthly_sales,
                     names.arg = c("August", "September", "October"),
                     col = c("red", "green", "dodgerblue"),
                     main = "August - October Sales",
                     xlab = "Month",
                     ylab = "Sales (overall)")
lines(bar_center, monthly_sales, type = "o", col = "black", lwd = 2)



# 7. ABC Classification based on Total Purchases
# Calculate total purchases and sort products by their total purchases
data <- data[order(-data$Total_Purchases), ]  
data$cumulative_sales <- cumsum(data$Total_Purchases) / sum(data$Total_Purchases)  

# Assign ABC classification
data$ABC_Class <- ifelse(data$cumulative_sales <= 0.1, "A",
                         ifelse(data$cumulative_sales <= 0.4, "B", "C"))

# View the number of products in each class
table(data$ABC_Class)

# Extract lists of products by classification
A_products <- data[data$ABC_Class == "A", ]
B_products <- data[data$ABC_Class == "B", ]
C_products <- data[data$ABC_Class == "C", ]

# Output products in each class
A_products
B_products
C_products


      
# 8. Safety Stock Calculation
average_demand <- mean(data$Total_Purchases)
std_dev_demand <- sd(data$Total_Purchases)
lead_time <- 2  # Lead time in months
z_factor <- 1.65  # Z-factor for 95% service level
safety_stock <- z_factor * std_dev_demand * sqrt(lead_time)
safety_stock
      
# 9. Reorder Point Calculation
avg_lead_time <- 2  # Lead time in months
reorder_point <- average_demand * avg_lead_time + safety_stock
reorder_point
      
