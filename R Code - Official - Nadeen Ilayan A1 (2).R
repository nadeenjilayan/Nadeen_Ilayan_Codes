#' Title: A1: Retail Stores EDA - Individual
#' Purpose: Data Analysis to Maximize Profits for Diageo
#' Author: Nadeen Ilayan
#' Date: January 24, 2024

# Set WD
setwd("C:/Users/nadee/OneDrive/Desktop/Hult_Visualizing_Analyzing_Data_with_R/personalFiles")

# Libraries
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(lubridate)

# Reading Data
data <- read.csv("C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\personalfiles\\a1_EDA_case_complete.csv")

# Create DataFrame for Diageo
diageo_data <- data %>%
  filter(`Vendor.Name` == "DIAGEO AMERICAS") %>%
  select(`Invoice.Item.Number`, Date, `Store.Number`, `Store.Name`, Address, City, `Zip.Code`, County, 
         `Category.Name`, `Item.Description`, Pack, `Bottle.Volume..ml.`, `State.Bottle.Cost`, `State.Bottle.Retail`, 
         `Bottles.Sold`, `Sale..Dollars.`, Month)

# Data Exploration
str(data)
dim(data)
class(data)
sapply(data, class)
head(data)
summary(data)

# Data Cleaning: Column Names
names(data) <- make.names(names(data), unique = TRUE)
names(data) <- tolower(names(data))
names(data) <- gsub("invoice.item.number", "invoice_number", names(data))
names(data) <- gsub("store.number", "store_number", names(data))
names(data) <- gsub("store.name", "store_name", names(data))
names(data) <- gsub("zip.code", "zip_code", names(data))
names(data) <- gsub("category.name", "category", names(data))
names(data) <- gsub("vendor.name", "vendor", names(data))
names(data) <- gsub("item.description", "description", names(data))
names(data) <- gsub("bottle.volume..ml.", "bottle_volume_ml", names(data))
names(data) <- gsub("state.bottle.cost", "bottle_cost", names(data))
names(data) <- gsub("state.bottle.retail", "bottle_retail", names(data))
names(data) <- gsub("bottles.sold", "bottles_sold", names(data))
names(data) <- gsub("sale..dollars.", "sales_dollars", names(data))
print(names(data))

# Data Integrity Checks
duplicate_rows <- data[duplicated(data), ]
print(paste("Number of duplicate rows: ", nrow(duplicate_rows)))
unique_categories <- unique(data$category)
print(unique_categories)
unique_vendors <- unique(data$vendor)
print(unique_vendors)
invalid_sales <- data[data$sales_dollars < 0, ]
invalid_volume <- data[data$bottle_volume_ml < 0, ]
print(paste("Number of transactions with invalid sales: ", nrow(invalid_sales)))
print(paste("Number of transactions with invalid bottle volume: ", nrow(invalid_volume)))

# Type Conversions
data$bottle_volume_ml <- as.numeric(as.character(data$bottle_volume_ml))
data$bottle_cost <- as.numeric(as.character(data$bottle_cost))
data$bottle_retail <- as.numeric(as.character(data$bottle_retail))
data$bottles_sold <- as.numeric(as.character(data$bottles_sold))
data$sales_dollars <- as.numeric(as.character(data$sales_dollars))

# Handling Missing Values
data <- data[!is.na(data$zip_code), ]
missing_values_summary <- sapply(data, function(x) sum(is.na(x)))
print(missing_values_summary)

# Data Analysis: Aggregations and Descriptive Statistics
length(unique(data$store_name))
mean(data$bottles_sold, na.rm = TRUE)
median(data$bottles_sold, na.rm = TRUE)
sum(data$sales_dollars, na.rm = TRUE)

# Outlier Detection
bottles_sold <- data$bottles_sold
Q1 <- quantile(bottles_sold, 0.25, na.rm = TRUE)
Q3 <- quantile(bottles_sold, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers_bottles_sold <- bottles_sold[bottles_sold < lower_bound | bottles_sold > upper_bound]
length(outliers_bottles_sold)

sales_dollars <- data$sales_dollars
Q1_sales <- quantile(sales_dollars, 0.25, na.rm = TRUE)
Q3_sales <- quantile(sales_dollars, 0.75, na.rm = TRUE)
IQR_sales <- Q3_sales - Q1_sales
lower_bound_sales <- Q1_sales - 1.5 * IQR_sales
upper_bound_sales <- Q3_sales + 1.5 * IQR_sales
outliers_sales_dollars <- sales_dollars[sales_dollars < lower_bound_sales | sales_dollars > upper_bound_sales]
length(outliers_sales_dollars)

# Ensuring Correct Data Types and Formats for Analysis
data$month <- factor(data$month)
data$date <- as.Date(data$date)
data$month <- factor(month(data$date, label = TRUE, abbr = TRUE))
data$year <- year(data$date)
data$weekday <- wday(data$date, label = TRUE, abbr = TRUE)

# Standardizing color scheme for plots
fill_color <- "red"
line_color <- "darkred"
red_palette <- c("darkred", "firebrick", "indianred", "lightcoral", "salmon")

# ------------------ Data Preparation and Transformation ------------------ #

# Ensure 'Date' is in date format for both datasets
data$date <- as.Date(data$date, format = "%Y-%m-%d")
diageo_data$Date <- as.Date(diageo_data$Date, format = "%Y-%m-%d")

# Create 'year', 'month_num', and 'month' columns for both datasets
data$year <- year(data$date)
data$month_num <- month(data$date)
data$month <- factor(month(data$date, label = TRUE, abbr = TRUE))

diageo_data$Year <- year(diageo_data$Date)
diageo_data$month_num <- month(diageo_data$Date)
diageo_data$Month <- floor_date(diageo_data$Date, "month")

# ------------------ Sales Aggregation and Summarization ------------------ #

# Aggregate sales by year and numeric month for both datasets
monthly_sales_agg <- data %>%
  group_by(year, month_num) %>%
  summarize(total_sales = sum(sales_dollars, na.rm = TRUE)) %>%
  ungroup()

monthly_sales_agg_diageo <- diageo_data %>%
  group_by(Year, month_num) %>%
  summarize(total_sales = sum(Sale..Dollars., na.rm = TRUE)) %>%
  ungroup()

# Aggregate and summarize sales by various categories
category_summary <- data %>%
  group_by(category) %>%
  summarize(total_sales = sum(sales_dollars, na.rm = TRUE),
            average_bottles_sold = mean(bottles_sold, na.rm = TRUE)) %>%
  arrange(desc(total_sales))

# ------------------ Data Visualization ------------------ #

# Plot the monthly sales trends using numeric months for both datasets
ggplot(monthly_sales_agg, aes(x = month_num, y = total_sales, group = year, color = as.factor(year))) +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.name, limits = c(1, 12)) +
  labs(title = "Monthly Sales Trends", x = "Month", y = "Sales Dollars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

ggplot(monthly_sales_agg_diageo, aes(x = month_num, y = total_sales, group = Year, color = as.factor(Year))) +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Monthly Sales Trends for Diageo", x = "Month", y = "Sales Dollars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

# Summarize sales by category for Diageo
category_summary_diageo <- diageo_data %>%
  group_by(Category.Name) %>%
  summarise(total_sales = sum(Sale..Dollars., na.rm = TRUE),
            average_bottles_sold = mean(Bottles.Sold, na.rm = TRUE)) %>%
  arrange(desc(total_sales))

top_categories_diageo <- head(category_summary_diageo, 5)

# Visualization of top categories for both datasets
top_categories <- head(category_summary, 5)

ggplot(top_categories, aes(x = reorder(category, -total_sales), y = total_sales, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(total_sales, big.mark = ",")), vjust = 0.5, size = 3.5, color = "black") +
  scale_fill_manual(values = red_palette) +
  labs(title = "Top 5 Product Categories by Total Sales", x = "Product Category", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Visualization of top categories for Diageo
ggplot(top_categories_diageo, aes(x = reorder(Category.Name, -total_sales), y = total_sales, fill = Category.Name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(total_sales, big.mark = ",")), vjust = 0.5, size = 3.5, color = "black") +  # Position text inside the bars
  scale_fill_manual(values = red_palette) +
  labs(title = "Top 5 Diageo Product Categories by Total Sales", x = "Product Category", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


# ------------------ Sales Aggregation for Additional Categories ------------------ #

# Aggregate sales data for Diageo Americas by store and city
diageo_store_sales <- diageo_data %>%
  group_by(Store.Name) %>%
  summarize(total_sales = sum(Sale..Dollars.)) %>%
  arrange(desc(total_sales))

city_sales_summary_diageo <- diageo_data %>%
  group_by(City) %>%
  summarize(total_sales = sum(Sale..Dollars., na.rm = TRUE)) %>%
  arrange(desc(total_sales))

# Aggregate and summarize sales by vendor
vendor_summary <- data %>%
  group_by(vendor) %>%
  summarize(total_sales = sum(sales_dollars, na.rm = TRUE),
            average_bottles_sold = mean(bottles_sold, na.rm = TRUE)) %>%
  arrange(desc(total_sales))

# ------------------ Visualizing Additional Sales Data ------------------ #

# Visualizing top and bottom stores for Diageo Americas
ggplot(head(diageo_store_sales, 5), aes(x = reorder(Store.Name, total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = line_color) +
  coord_flip() +
  labs(title = "Top 5 Stores by Sales for Diageo Americas", x = "Store Name", y = "Total Sales Dollars")

ggplot(tail(diageo_store_sales, 5), aes(x = reorder(Store.Name, total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = line_color) +
  coord_flip() +
  labs(title = "Bottom 5 Stores by Sales for Diageo Americas", x = "Store Name", y = "Total Sales Dollars")

# Visualizing top and bottom cities for Diageo Americas
ggplot(head(city_sales_summary_diageo, 5), aes(x = reorder(City, -total_sales), y = total_sales, fill = City)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(total_sales)), vjust = 1.5, size = 3.5, color = "white") +
  scale_fill_manual(values = red_palette) +
  labs(title = "Top 5 Cities by Diageo Total Sales", x = "City", y = "Total Sales ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggplot(tail(city_sales_summary_diageo, 5), aes(x = reorder(City, total_sales), y = total_sales, fill = City)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(total_sales)), vjust = -0.3, size = 3.5, color = "black") +
  scale_fill_manual(values = red_palette) +
  labs(title = "Bottom 5 Cities by Diageo Total Sales", x = "City", y = "Total Sales ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Visualizing top vendors
ggplot(head(vendor_summary, 5), aes(x = reorder(vendor, -total_sales), y = total_sales, fill = vendor)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(total_sales, big.mark = ",")), vjust = 0.5, size = 3.5, color = "black") +
  scale_fill_manual(values = red_palette) +
  labs(title = "Top 5 Vendors by Total Sales", x = "Vendor", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# ------------------ Time Series Analysis and Visualization ------------------ #

# Transform and aggregate original data
data$date <- as.Date(data$date, format="%Y-%m-%d")
data$month <- floor_date(data$date, "month")
monthly_sales <- data %>%
  group_by(month) %>%
  summarize(total_sales = sum(sales_dollars)) %>%
  mutate(source = "All Stores")

# Transform and aggregate Diageo data
diageo_data$Date <- as.Date(diageo_data$Date, format="%Y-%m-%d")
diageo_data$Month <- floor_date(diageo_data$Date, "month")
monthly_sales_diageo <- diageo_data %>%
  group_by(Month) %>%
  summarize(total_sales = sum(Sale..Dollars.)) %>%
  mutate(source = "Diageo Americas")

# Rename month to Month in Diageo dataset
monthly_sales_diageo <- monthly_sales_diageo %>%
  rename(month = Month)

# Combine datasets
combined_sales <- rbind(monthly_sales, monthly_sales_diageo)

# Plot combined data
ggplot(combined_sales, aes(x = month, y = total_sales, color = source, group = source)) +
  geom_line() +
  labs(title = "Comparative Sales Trends Over Time", x = "Month", y = "Total Sales", color = "Source") +
  scale_color_manual(values = c(fill_color, line_color))

# ------------------ Visualization of Sales by Bottle Volume and Pack Size ------------------ #

# Aggregate sales by bottle volume
volume_sales <- data %>%
  group_by(bottle_volume_ml) %>%
  summarize(total_sales = sum(sales_dollars)) %>%
  arrange(desc(total_sales))

# Bar chart for sales by bottle volume
ggplot(volume_sales, aes(x = as.factor(bottle_volume_ml), y = total_sales)) +
  geom_bar(stat = "identity", fill = line_color, color = line_color) +
  labs(title = "Sales by Bottle Volume", x = "Bottle Volume (ml)", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Aggregate sales by pack size
pack_size_sales <- data %>%
  group_by(pack) %>%
  summarize(total_sales = sum(sales_dollars)) %>%
  arrange(desc(total_sales))

# Bar chart for sales by pack size
ggplot(pack_size_sales, aes(x = as.factor(pack), y = total_sales)) +
  geom_bar(stat = "identity", fill = line_color, color = line_color) +
  labs(title = "Sales by Pack Size", x = "Pack Size", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ------------------ Yearly and Monthly Sales Analysis of Top Vendors ------------------ #

# Aggregate total sales by vendor
top_vendors <- data %>%
  group_by(vendor) %>%
  summarize(total_sales = sum(sales_dollars)) %>%
  arrange(desc(total_sales)) %>%
  top_n(4, total_sales)

# Get the names of top 4 vendors
top_vendors_names <- top_vendors$vendor

# Ensure the 'date' column is in Date format and create 'year' column
data$year <- year(data$date)

# Filter data for top 4 vendors
data_top_vendors_yearly <- data %>%
  filter(vendor %in% top_vendors_names) %>%
  group_by(vendor, year) %>%
  summarize(total_sales = sum(sales_dollars), .groups = 'drop')

# Line Graph for Yearly Sales of Top 4 Vendors
ggplot(data_top_vendors_yearly, aes(x = year, y = total_sales, group = vendor, color = vendor)) +
  geom_line() +
  labs(title = "Yearly Sales Trends of Top 4 Vendors", x = "Year", y = "Total Sales") +
  theme(legend.position = "bottom")

# Filter data for the year 2023 and top 4 vendors for monthly analysis
data_top_vendors_monthly_2023 <- data %>%
  filter(year == 2023 & vendor %in% top_vendors_names) %>%
  group_by(vendor, month) %>%
  summarize(total_sales = sum(sales_dollars), .groups = 'drop')

# Line Graph for Monthly Sales of Top 4 Vendors in 2023
ggplot(data_top_vendors_monthly_2023, aes(x = month, y = total_sales, group = vendor, color = vendor)) +
  geom_line() +
  labs(title = "Monthly Sales Trends of Top 4 Vendors in 2023", x = "Month", y = "Total Sales") +
  theme(legend.position = "bottom")

# END
