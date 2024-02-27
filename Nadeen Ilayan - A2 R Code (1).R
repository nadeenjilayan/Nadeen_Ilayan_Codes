# Author: Nadeen Ilayan
# Date: Jan 27, 2024
# Purpose: Creating a predictive model for BBY

# Libraries
#----------
library(ggplot2)
library(rpart)
library(caret)
library(plyr)
library(dplyr) # Data manipulation
library(MLmetrics) # Evaluate models and compare models 
library(vtreat) # Variable treatment for dummy variables
library(Metrics)
library(ggcorrplot)
library(rpart.plot)
library(readr)

# Working Directory
#------------------
setwd("C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\personalFiles")

# Data Loading
#-------------
# Training data
allTrainingFiles <- list.files(path = 'C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\Cases\\A2_Household_Spend\\studentTables',
                               pattern = 'training',
                               full.names = TRUE)
allTrainingDF <- lapply(allTrainingFiles, read.csv)
allTrainingDF <- join_all(allTrainingDF, by = 'tmpID', type = 'left')

# Testing data
allTestingFiles <- list.files(path = 'C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\Cases\\A2_Household_Spend\\studentTables',
                              pattern = 'testing',
                              full.names = TRUE)
allTestingDF <- lapply(allTestingFiles, read.csv)
allTestingDF <- join_all(allTestingDF, by = 'tmpID', type = 'left')

# Prospects data
allProspectsFiles <- list.files(path = 'C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\Cases\\A2_Household_Spend\\studentTables',
                                pattern = 'prospects',
                                full.names = TRUE)
allProspectsDF <- lapply(allProspectsFiles, read.csv)
allProspectsDF <- join_all(allProspectsDF, by = 'tmpID', type = 'left')

# Sample - Partitioning Data
#---------------------------
set.seed(123) # For reproducibility
trainingIndices <- createDataPartition(y = allTrainingDF$tmpID, p = 0.80, list = FALSE)
train <- allTrainingDF[trainingIndices, ]
validation <- allTrainingDF[-trainingIndices, ]

# Explore - Exploratory Data Analysis
#-------------------------------------
cat("Summary of Training Data:\n")
print(summary(train))
summary(allTrainingDF)
colSums(is.na(allTrainingDF))

# Basic Data Summary
summary(allTrainingDF)

# Checking for Missing Values
colSums(is.na(allTrainingDF))

# Univariate Analysis
# Histogram of yHat
ggplot(allTrainingDF, aes(x = yHat)) + 
  geom_histogram(bins = 30, fill = "maroon", color = "black") + 
  theme_minimal() + 
  ggtitle("Distribution of BBY Spending (yHat)")

# Boxplot for NetWorth
ggplot(allTrainingDF, aes(x = "", y = NetWorth)) + 
  geom_boxplot(fill = "maroon", color = "maroon") + 
  theme_minimal() + 
  ggtitle("Boxplot of Net Worth")

# Categorical Variable Analysis
# Bar Plot for HomeOwnerRenter
ggplot(allTrainingDF, aes(x = HomeOwnerRenter)) + 
  geom_bar(fill = "maroon") + 
  theme_minimal() + 
  ggtitle("Distribution of Home Ownership")

# Bivariate Analysis: Relationship with yHat
# Scatter Plot for yHat vs Age
ggplot(allTrainingDF, aes(x = Age, y = yHat)) + 
  geom_point(alpha = 0.5, color = "maroon") + 
  theme_minimal() + 
  ggtitle("yHat vs Age")

# Correlation Plot for Numerical Variables
# Select numerical variables excluding tmpID
numerical_vars <- allTrainingDF %>% 
  select(where(is.numeric)) %>%
  select(-tmpID)

# Compute the correlation matrix for the numerical variables
correlation_matrix <- cor(numerical_vars, use = "complete.obs")  # Using complete.obs to handle missing values

# Visualize the correlation matrix with selective red highlights
ggcorrplot(correlation_matrix, lab = TRUE, 
           colors = c("white", "white", "maroon"), 
           outline.color = "grey",
           ggtheme = theme_minimal())

# Modify - Preparing Data for Modeling
#-------------------------------------
# All column names, after EXPLORE you should know which variables you want to use
names(allTrainingDF) # all variables

# Choose the columns we need INFORMATIVE FEATURES 1
informativeFeatures <- c(
  'MedianEducationYears', 'NetWorth', 'Investor', 'BusinessOwner',
  'HomeOffice', 'Age', 'storeVisitFrequency', 'UpscaleBuyerInHome',
  'BookBuyerInHome', 'ComputerOwnerInHome', 'DogOwner', 'CatOwner',
  'HomeOwnerRenter', 'Education', 'OccupationIndustry', 'FamilyMagazineInHome',
  'FemaleOrientedMagazineInHome', 'GardeningMagazineInHome',
  'CulinaryInterestMagazineInHome', 'HealthFitnessMagazineInHome',
  'DoItYourselfMagazineInHome', 'FinancialMagazineInHome',
  'EstHomeValue', 'HomePurchasePrice', 'LandValue'
)

# Define the name of the target variable
targetName <- 'yHat'

# Apply vtreat's designTreatmentsN function to create a treatment plan for the informative features
plan <- designTreatmentsN(dframe      = train, 
                          varlist     = informativeFeatures,
                          outcomename = targetName)

# Access the scoreFrame within the plan object and print it
scoreFrame <- plan$scoreFrame
print(scoreFrame)

# Now we apply the treatment plan to the training, validation, testing, and prospect datasets
# Apply the plan to the training set
treatedTrain <- prepare(plan, train)
# Apply the plan to the validation set
treatedValidation <- prepare(plan, validation)
# Apply the plan to the test set
treatedTest <- prepare(plan, allTestingDF)
# Apply the plan to the prospect set
treatedProspects <- prepare(plan, allProspectsDF)

# Model - Building the Models
#----------------------------
# Model 1 - Linear Model
fit <- lm(yHat ~ ., data = treatedTrain)
summary(fit)

# Analyze - Making Predictions and Model Assessment
#--------------------------------------------------
linearTrainPredictions <- predict(fit, treatedTrain)
linearValidationPredictions <- predict(fit, treatedValidation)

# Calculate RMSE for training data
rmse_train <- rmse(treatedTrain$yHat, linearTrainPredictions)

# Calculate RMSE for validation data
rmse_validation <- rmse(treatedValidation$yHat, linearValidationPredictions)

# Print the RMSE values
print(paste("RMSE for Training Data:", rmse_train))
print(paste("RMSE for Validation Data:", rmse_validation))

# Additional Model Building and Assessments
#-----------------------------------------
# Model 2 - Desicion Tree
informartiveFeatures2 <- c(
  'MedianEducationYears', 'NetWorth', 'Investor', 'BusinessOwner',
  'HomeOffice', 'Age', 'storeVisitFrequency', 'UpscaleBuyerInHome',
  'BookBuyerInHome', 'ComputerOwnerInHome', 'DogOwner',
  'HomeOwnerRenter', 'Education', 'OccupationIndustry', 'FamilyMagazineInHome',
  'FemaleOrientedMagazineInHome', 'GardeningMagazineInHome',
  'CulinaryInterestMagazineInHome', 'HealthFitnessMagazineInHome',
  'DoItYourselfMagazineInHome', 'FinancialMagazineInHome'
)

# Apply vtreat's designTreatmentsN function for the new set of features
plan2 <- designTreatmentsN(dframe      = train, 
                           varlist     = informartiveFeatures2,
                           outcomename = targetName)

# Apply the new treatment plan to the datasets
treatedTrain2 <- prepare(plan2, train)
treatedValidation2 <- prepare(plan2, validation)
treatedTest2 <- prepare(plan2, allTestingDF)
treatedProspects2 <- prepare(plan2, allProspectsDF)

# Fit the decision tree model using the new treated training data
decisionTreeModel2 <- rpart(yHat ~ ., data = treatedTrain2, method = "anova", cp = 0.001)

# Summary of the decision tree model
print(summary(decisionTreeModel2))
rpart.plot(decisionTreeModel2)

# Make predictions using the decision tree model for training data
decisionTreeTrainPredictions2 <- predict(decisionTreeModel2, treatedTrain2)

# Make predictions using the decision tree model for validation data
decisionTreeValidationPredictions2 <- predict(decisionTreeModel2, treatedValidation2)

# Calculate RMSE for training data
rmse_train2 <- sqrt(mean((treatedTrain2$yHat - decisionTreeTrainPredictions2)^2))
rmse_validation2 <- sqrt(mean((treatedValidation2$yHat - decisionTreeValidationPredictions2)^2))
print(paste("RMSE for Training Data (Decision Tree Model 2):", rmse_train2))
print(paste("RMSE for Validation Data (Decision Tree Model 2):", rmse_validation2))

# ----------------------
# Model 3 - Linear Model
informativeFeatures3 <- c(
  'MedianEducationYears', 'NetWorth', 'Investor', 'BusinessOwner',
  'HomeOffice', 'Age', 'storeVisitFrequency', 'UpscaleBuyerInHome',
  'BookBuyerInHome', 'ComputerOwnerInHome', 'DogOwner', 
  'HomeOwnerRenter', 'Education', 'OccupationIndustry', 'FamilyMagazineInHome',
  'FemaleOrientedMagazineInHome', 'GardeningMagazineInHome',
  'CulinaryInterestMagazineInHome', 'HealthFitnessMagazineInHome',
  'DoItYourselfMagazineInHome', 'FinancialMagazineInHome'
)

# Apply vtreat's designTreatmentsN function for Model 3
plan3 <- designTreatmentsN(dframe = train, 
                           varlist = informativeFeatures3,
                           outcomename = targetName)

# Apply the new treatment plan to the datasets
treatedTrain3 <- prepare(plan3, train)
treatedValidation3 <- prepare(plan3, validation)
treatedTest3 <- prepare(plan3, allTestingDF)
treatedProspects3 <- prepare(plan3, allProspectsDF)

# Fit the linear model using the new treated training data for Model 3
fit3 <- lm(yHat~., treatedTrain3)
# Summarize the model
summary(fit3)

# Make predictions with Model 3
linearTrainPredictions3 <- predict(fit3, treatedTrain3)
linearValidationPredictions3 <- predict(fit3, treatedValidation3)

# Calculate and print RMSE for Model 3
rmse_train3 <- rmse(treatedTrain3$yHat, linearTrainPredictions3)
rmse_validation3 <- rmse(treatedValidation3$yHat, linearValidationPredictions3)
print(paste("RMSE for Training Data (Model 3):", rmse_train3))
print(paste("RMSE for Validation Data (Model 3):", rmse_validation3))

# Output - Saving Predictions and Final Outputs
#----------------------------------------------
# Make predictions on the prospect dataset using the decision tree model because it has the lowest RMSE
prospectPredictions2 <- predict(decisionTreeModel2, treatedProspects2)

# View or store the predictions
print(prospectPredictions2)

# Adding the predictions as a new column to the prospects DataFrame
allProspectsDF$Predictions <- prospectPredictions2

# Define the path for the output file. Change the path as necessary
output_file_path <- "C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\Cases\\A2_Household_Spend\\studentTables\\Prospects_with_Predictions_official.csv"

# Write the DataFrame with predictions to a CSV file
write.csv(allProspectsDF, output_file_path, row.names = FALSE)

# --------------------
# New EDA on Predicted Model

# Load the dataset
data <- read.csv("C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\Cases\\A2_Household_Spend\\studentTables\\Prospects_with_Predictions_official.csv")

# EDA - Exploratory Data Analysis
#---------------------------------

# Overview of the dataset structure
str(data)

# Summary for each variable in the dataset
summary(data)

# First few rows of the dataset
head(data)

# Missing values across the dataset
total_missing_values <- sum(is.na(data))
cat("Total missing values in dataset:", total_missing_values, "\n")

# Missing values count per column
missing_values_per_column <- sapply(data, function(x) sum(is.na(x)))
print(missing_values_per_column)

# Calculate deciles for the 'Predictions' column and select top 10% decile
data$decile <- ntile(data$Predictions, 10)
top_10_decile <- data[data$decile == 10, ]

# Summary statistics for the top 10% decile
cat("Summary statistics for the top 10% decile:\n")
summary(top_10_decile)

# Structure of the top 10% decile data
str(top_10_decile)

# First few rows of the top 10% decile data
head(top_10_decile)

# Missing values count per column for the top 10% decile
missing_values_top_10 <- sapply(top_10_decile, function(x) sum(is.na(x)))
print(missing_values_top_10)

# Additional summary statistics for numerical variables in top 10% decile
summary(select(top_10_decile, where(is.numeric)))

# Mean and median for key variables in top 10% decile
mean_age <- mean(top_10_decile$Age, na.rm = TRUE)
median_age <- median(top_10_decile$Age, na.rm = TRUE)
mean_predictions <- mean(top_10_decile$Predictions, na.rm = TRUE)
median_predictions <- median(top_10_decile$Predictions, na.rm = TRUE)

# Print calculated mean and median
print(paste("Mean Age:", mean_age))
print(paste("Median Age:", median_age))
print(paste("Mean Predictions:", mean_predictions))
print(paste("Median Predictions:", median_predictions))

# Visualizations
#---------------

# Histogram of Predictions in the Top 10% Decile
ggplot(top_10_decile, aes(x = Predictions)) + 
  geom_histogram(bins = 30, fill = "maroon", color = "black") + 
  theme_minimal() + 
  ggtitle("Distribution of Predictions in Top 10% Decile")

# Histogram of Age in the Top 10% Decile
ggplot(top_10_decile, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "maroon", color = "black") + 
  theme_minimal() + 
  ggtitle("Distribution of Age in Top 10% Decile") +
  xlab("Age") +
  ylab("Count") +
  theme(axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Bar Plot for Home Ownership Status in Top 10% Decile
ggplot(top_10_decile, aes(x = HomeOwnerRenter)) + 
  geom_bar(fill = "maroon", color = "black") + 
  theme_minimal() + 
  ggtitle("Home Ownership Status in Top 10% Decile") +
  xlab("Home Owner vs Renter") +
  ylab("Count") +
  theme(axis.title = element_text(size = 12, face = "bold"))

# Scatter Plot for Predictions vs Age in Top 10% Decile
ggplot(top_10_decile, aes(x = Age, y = Predictions)) + 
  geom_point(alpha = 0.5, color = "maroon") + 
  theme_minimal() + 
  ggtitle("Predictions vs Age")

# Correlation matrix and visualization for numerical variables
numerical_vars <- data %>% 
  select(where(is.numeric)) %>%
  select(-tmpID)  # Exclude 'tmpID' from the analysis

# Compute the correlation matrix
correlation_matrix <- cor(numerical_vars, use = "complete.obs")

# Visualize the full correlation matrix with squares and the maroon color scheme
ggcorrplot(correlation_matrix, 
           method = "square",  # Use squares instead of circles
           lab = TRUE,         # Include correlation coefficients in the plot
           colors = c("white", "white", "maroon"),  # Maintain the color scheme as before
           outline.color = "grey",
           lab_size = 3)       # Set the size of the labels
           

#END
