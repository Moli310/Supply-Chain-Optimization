#Capstone 4 

# Load Required Libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
library(cluster)
library(factorextra)
library(caret)
library(randomForest)
library(knitr) 

# Load Dataset
delhivery <- read.csv("C:/Users/TUF GAMING/Downloads/delhivery.csv")

# View Dataset Structure (EDA)
head(delhivery)
tail(delhivery)

# structure of dataset
str(delhivery)

#Summary of dataset
summary(delhivery)

# Check Missing Values
colSums(is.na(delhivery))

# Calculate Delivery Delays
delhivery$delivery_delay <- delhivery$actual_time - delhivery$osrm_time

#1. Identify Vendors & Routes Responsible for Delivery Delays
# Vendor Analysis
top_vendors <- aggregate(delhivery$delivery_delay, 
                         by = list(delhivery$source_name), 
                         FUN = mean, na.rm = TRUE)

colnames(top_vendors) <- c("source_name", "avg_delay")
top_vendors <- top_vendors[order(-top_vendors$avg_delay), ]  # Sort in descending order
top_vendors <- head(top_vendors, 10)  # Select top 10

print(top_vendors)

# Route Analysis
top_routes <- aggregate(delhivery$delivery_delay, 
                        by = list(delhivery$route_type), 
                        FUN = mean, na.rm = TRUE)

colnames(top_routes) <- c("route_type", "avg_delay")
top_routes <- top_routes[order(-top_routes$avg_delay), ]  # Sort in descending order
top_routes <- head(top_routes, 10)  # Select top 10

print(top_routes)

# Visualization
ggplot(top_vendors, aes(x = reorder(source_name, avg_delay), y = avg_delay)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Vendors with Highest Delivery Delays", x = "Vendor", y = "Average Delay (days)")

# Identify Top 10 Vendors with Lowest Delivery Delays
lowest_vendors <- delhivery %>%
  group_by(source_name) %>%
  summarise(avg_delay = mean(delivery_delay, na.rm = TRUE)) %>%
  arrange(avg_delay) %>%
  head(10)

# Visualization: Top 10 Vendors with Lowest Delivery Delays
ggplot(lowest_vendors, aes(x = reorder(source_name, avg_delay), y = avg_delay)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Vendors with Lowest Delivery Delays", x = "Vendor", y = "Average Delay (days)") +
  theme_minimal()

delhivery$delivery_delay <- delhivery$actual_time - delhivery$osrm_time

# Load data
inventory_data <- read.csv("C:/Users/TUF GAMING/Downloads/delhivery.csv")

# Aggregate demand and shipments
demand_summary <- aggregate(inventory_data$destination_name, by = list(inventory_data$destination_name), FUN = length)
colnames(demand_summary) <- c("destination", "demand")

shipment_summary <- aggregate(inventory_data$source_name, by = list(inventory_data$source_name), FUN = length)
colnames(shipment_summary) <- c("source", "shipments")

# Merge demand and supply data
inventory_data <- merge(demand_summary, shipment_summary, by.x = "destination", by.y = "source", all.x = TRUE)
inventory_data[is.na(inventory_data)] <- 0  # Replace NA with 0 for missing shipments

# Cost coefficients
holding_cost <- 2
transport_cost <- 5

# Define optimization model
costs <- c(holding_cost, transport_cost)

A <- matrix(c(1, 1, -1, 0, 0, -1), nrow = 3, byrow = TRUE)
b <- c(sum(inventory_data$demand), -min(inventory_data$demand), -max(inventory_data$demand))
directions <- c(">=", "<=", "<=")

# Solve optimization
install.packages("lpSolve")
library(lpSolve)
solution <- lp("min", costs, A, directions, b)

# Extract results
inventory_level <- solution$solution[1]
shipment_plan <- solution$solution[2]
min_cost <- solution$objval

# Data for Visualization
cost_data <- data.frame(
  Category = c("Holding Cost", "Transport Cost"),
  Cost = c(inventory_level * holding_cost, shipment_plan * transport_cost)
)

# Plot: Cost Breakdown
ggplot(cost_data, aes(x = Category, y = Cost, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cost Breakdown", y = "Total Cost", x = "Cost Type")

# Inventory vs Demand Plot
inventory_plot_data <- data.frame(
  Type = c("Total Inventory", "Total Demand"),
  Units = c(inventory_level, sum(inventory_data$demand))
)

ggplot(inventory_plot_data, aes(x = Type, y = Units, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Inventory vs Demand", y = "Units", x = "Category")

# Shipment Plan Plot
shipment_data <- data.frame(
  Shipment = "Optimal Shipment",
  Units = shipment_plan
)

ggplot(shipment_data, aes(x = Shipment, y = Units, fill = Shipment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Optimal Shipment Plan", y = "Units", x = "Shipment Type")

### **3. Shipping Routes and Warehouse Placement**
# Load necessary library
library(caret)

# Clustering for Optimization
clustering_data <- delhivery %>%
  select(actual_distance_to_destination, actual_time, osrm_time, segment_osrm_distance) %>%
  na.omit()
clustering_data <- scale(clustering_data)

# K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 3)
delhivery$cluster <- as.factor(kmeans_result$cluster)

# Cluster Visualization
ggplot(delhivery, aes(x = actual_distance_to_destination, y = actual_time, color = cluster)) +
geom_point(alpha = 0.7) +labs(title = "Warehouse Optimization Clustering", x = "Distance to Destination", y = "Actual Time Taken")

# Confusion Matrix (If true labels are available)
if ("true_labels" %in% colnames(delhivery)) {
  conf_matrix <- table(Predicted = delhivery$cluster, Actual = delhivery$true_labels)
  
  # Print Confusion Matrix as Table
  print(kable(conf_matrix, caption = "Confusion Matrix: K-Means Clustering vs. True Labels"))
  
  # Detailed Performance Metrics
  print(confusionMatrix(conf_matrix))
} else {
  print("No ground-truth labels available. Showing cluster distribution:")
  print(kable(table(delhivery$cluster), caption = "Cluster Distribution"))
}
#4. Geographic Variation in Supplier Reliability
# Supplier Reliability Analysis
supplier_reliability <- delhivery %>%
  group_by(source_name) %>%
  summarise(avg_delay = mean(delivery_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay)) %>%
  head(10)

# Visualization
ggplot(supplier_reliability, aes(x = reorder(source_name, avg_delay), y = avg_delay)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Supplier Reliability by Location", x = "Supplier", y = "Average Delay (days)")

#5. Machine Learning Model for Forecasting Delays
# Create Binary Target Variable
delhivery$delayed <- ifelse(delhivery$delivery_delay > 2, 1, 0)

# Split Data into Training and Testing Sets
set.seed(123)
trainIndex <- createDataPartition(delhivery$delayed, p = 0.7, list = FALSE)
train_data <- delhivery[trainIndex, ]
test_data <- delhivery[-trainIndex, ]

# Train Logistic Regression Model
log_model <- glm(delayed ~ actual_distance_to_destination + osrm_time + factor(route_type), 
                 data = train_data, family = binomial)
summary(log_model)

# Predictions on Test Data
predictions <- predict(log_model, test_data, type = "response")
test_data$predicted_delay <- ifelse(predictions > 0.5, 1, 0)

# Calculate Model Accuracy
accuracy <- sum(test_data$predicted_delay == test_data$delayed) / nrow(test_data)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

# Extract the model summary
summary_log_model <- summary(log_model)

# Extract test statistics and p-values for each coefficient
test_statistics <- summary_log_model$coefficients[, "z value"]
p_values <- summary_log_model$coefficients[, "Pr(>|z|)"]

# Critical value for a 95% confidence level (two-tailed test)
critical_value <- qnorm(1 - 0.05 / 2)

# Output the test statistics, p-values, and critical value
test_statistics
p_values
critical_value
