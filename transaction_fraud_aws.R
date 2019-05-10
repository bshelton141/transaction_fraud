

# R version 3.3.0


# ======================================================
# Configure the environment
# ======================================================

# Install and load required packages
packages <- c("jsonlite",
              "corrplot",
              "dplyr",
              "caret",
              "randomForest",
              "pROC",
              "DMwR",
              "ROSE",
              "Matrix",
              "data.table",
              "xgboost",
              "ggplot2",
              "e1071")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
for (p in packages) library(p, character.only = TRUE, quietly = TRUE)


# ======================================================
# Download .zip file into a temporary directory,
# unzip the .json file,
# and read it into R memory as data.frame
# ======================================================

# location of zip file
transactions_url <- "https://s3.us-east-2.amazonaws.com/example.data/transactions.zip"

# create a temp directory to download the above .zip url into
temp_dir <- tempdir()
temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")

# download the .zip into the temp file
download.file(transactions_url, temp_file)

# unzip the file to the temp directory
trans_name <- unzip(temp_file, list = TRUE)$Name
unzip(temp_file, exdir = temp_dir, overwrite = TRUE)
trans_path <- file.path(temp_dir, trans_name)

# read the data from json into an R data frame
data <- stream_in(file(trans_path))


# ======================================================
# Explore the data
# ======================================================

dim(data)

# Identify how many values from each column are empty
empty_vals <- sapply(data, function(x) sum(x == ""))
print(empty_vals)

# Identify the distribution of each numeric field
num_fields <- colnames(data[sapply(data, is.numeric)])
num_summary <- summary(data[, num_fields])
print(num_summary)

# Identify any correlation between the numeric variables
num_cor <- cor(data[, num_fields], method = "pearson")
plot_cor <- corrplot(num_cor, method = "number")
print(plot_cor)

# Identify the unique elements of each non-numeric columns
non_num_data <- data[, !(colnames(data) %in% num_fields)]
unique_cat <- sapply(non_num_data, function(x) length(unique(x)))
print(unique_cat)

# Identify the variables with near-zero variance
nzvar <- subset(nearZeroVar(non_num_data, saveMetrics = TRUE),
       nzv == TRUE & percentUnique < .5)
print(nzvar)

# Run a Chi-Squared Test for non-numeric values on columns where
# the number of unique values in the column is > 1 and < 51
low_values <- sapply(non_num_data, function(x) (length(unique(x)) < 51 & length(unique(x)) > 1))
chi2_data <- non_num_data[, low_values]


# ======================================================
# Plot the `transactionAmount`
# ======================================================

bin_size <- (max(data$transactionAmount) - min(data$transactionAmount)) / 100
trans_dist <- ggplot(data, aes(x = transactionAmount)) +
# create the histogram
  geom_histogram(binwidth = bin_size, colour = "black", fill = "white") +
# add the median line
  geom_vline(aes(xintercept = median(transactionAmount, na.rm = T), color = "median"),
             linetype = "dashed", size = 1, show.legend = TRUE) +
# add the mean line
  geom_vline(aes(xintercept = mean(transactionAmount, na.rm = T), color = "mean"),
             linetype = "dashed", size = 1, show.legend = TRUE) +
# format the labels
  labs(title = "Distribution of Individual Transaction Amounts",
       x = "Transaction Dollar Amount",
       y = "Count of Transactions") +
# format the legend
  scale_color_manual(name = "Statistics", values = c(median = "blue", mean = "red")) +
# annonate the distribution's Skewness Coefficient
  annotate("text", x = 1500, y = 20000,
           label = paste0("Skewness Coefficient = ",
                          round(skewness(data$transactionAmount), 2)))
print(trans_dist)


# ======================================================
# Plot the log transformed `transactionAmount`
# ======================================================

# Transform the `transactionAmount` using log base 10 transformation
# This transform function is being cited from
# https://www.r-statistics.com/2013/05/log-transformations-for-skewed-and-wide-distributions-from-practical-data-science-with-r/
signedlog10 = function(x) {
ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

data$transformedAmount <- signedlog10(data$transactionAmount)

bin_size2 <- (max(data$transformedAmount) - min(data$transformedAmount)) / 100
log_trans_dist <- ggplot(data, aes(x = transformedAmount)) +
  # create the histogram
  geom_histogram(binwidth = bin_size2, colour = "black", fill = "white") +
  # format the labels
  labs(title = "Distribution of Log Transformed Transaction Amounts",
       x = "Log Base 10 Transformed Dollar Amounts",
       y = "Count of Transactions") +
  # format the legend
  scale_color_manual(name = "Statistics", values = c(median = "blue", mean = "red"))
print(log_trans_dist)

data$transformedAmount <- NULL


# ======================================================
# Data Wrangling: Identifying Reversals
# ======================================================

# Identify reversal transaction count and dollar amounts
reversals <- data %>%
  filter(transactionType == "REVERSAL") %>%
  summarise(reversal_count = n(),
            pcnt_total_counts = n() / nrow(data),
            reversal_dollars = sum(transactionAmount),
            pcnt_total_dollars = reversal_dollars / sum(data$transactionAmount))

print(reversals)

data1 <- data %>%
# create a key for identifying unique transactions by account, merchant, date, and amount
  mutate(transactionDate = substr(transactionDateTime, 1, 10),
         transactionKey = paste(accountNumber,
                                merchantName,
                                transactionDate,
                                transactionAmount, sep = "_"),
# create a key for identifying unique transactions by account, merchant, and amount
         DatelessTransactionKey = paste(accountNumber,
                                        merchantName,
                                        transactionAmount, sep = "_")) %>%
# order the data by unique transactions and time of swipe
  arrange(DatelessTransactionKey, merchantName, transactionDateTime) %>%
# create a field to label a record with 1 when it either followed by a "REVERSAL" for the same transaction
# OR if it is a "REVERSAL" itself
  mutate(rev_association = ifelse((dplyr::lead(DatelessTransactionKey,
                                               n = 1,
                                               default = "NA") == DatelessTransactionKey
                                     & dplyr::lead(transactionType) == "REVERSAL")
                                    | transactionType == "REVERSAL",
                                  "1",
                                  "0"))

# Select all records associated with a reversal
reversal_view <- data1 %>%
  filter(rev_association == "1") %>%
  select(customerId,
         transactionDate,
         merchantName,
         transactionAmount,
         transactionType,
         rev_association)

# Display an example of the reversals and their anchors
head(reversal_view, 8)

# Identify reversal-associated transaction count and dollar amounts (including anchors)
rev_associated <- data1 %>%
  filter(rev_association == "1") %>%
  summarise(reversal_count = n(),
            pcnt_total_count = n() / nrow(data),
            reversal_dollars = sum(transactionAmount),
            pcnt_total_dollars = reversal_dollars / sum(data$transactionAmount))

print(rev_associated)


# ======================================================
# Data Wrangling: Identifying Multi-Swipes
# Because there could be legitimate multiple purchases for the same amount on the same day,
# (e.g., taking an Uber to and from a specific location), multi-swipes are being considered
# if the exact same transaction happens twice within 5 minutes or less.
# ======================================================

data2 <- data1 %>%
# Format the date time stamp field
  mutate(transactionDateTime = as.POSIXct(strptime(gsub("T", " ", transactionDateTime), "%Y-%m-%d %H:%M:%OS")),
# Calculate the number of minutes between swipes for the same transactionKey
         time_from_previous = ifelse (dplyr::lag(transactionKey, n = 1, default = "NA") == transactionKey,
                                      round(as.numeric(transactionDateTime - dplyr::lag(transactionDateTime,
                                                                                        n = 1,
                                                                                        default = NA))/60),
                                      NA)) %>%
# Identify all records where the time of the duplicate transaction was within 5 minutes of
# the same account, merchant, and amount
# and is NOT a "REVERSAL"
  mutate(multi_swipe = ifelse(time_from_previous >= 5
                               | is.na(time_from_previous)
                               | transactionType == "REVERSAL",
                              "0",
                              "1"))

# Identify unique `transactionKey` values associated with multi-swipes
multi_swipe_key <- unique(
  data2%>%
  filter(multi_swipe == 1) %>%
  select(transactionKey)
)

# Identify all transactions that have the same `transactionKey` as a multi-swipe
multi_swipe_view <- data2%>%
  filter(transactionKey %in% multi_swipe_key$transactionKey) %>%
  select(customerId,
         transactionDateTime,
         merchantName,
         transactionAmount,
         transactionType,
         multi_swipe)

# Display an example of how multi-swipes are being labeled
head(multi_swipe_view, 8)

# Identify all multi-swipe transaction count and dollar amounts
# excluding the original transaction of the multi-swipe sequence
multi_swipes <- data2 %>%
  filter(multi_swipe == "1") %>%
  summarise(reversal_count = n(),
            pcnt_total_count = n() / nrow(data),
            reversal_dollars = sum(transactionAmount),
            pcnt_total_dollars = reversal_dollars / sum(data$transactionAmount))

print(multi_swipes)




# ======================================================
# ======================================================
# ======================================================
#                   MACHINE LEARNING
# ======================================================
# ======================================================
# ======================================================

# Print the split between the two labels in the output variable.
table(data2$isFraud)

# ======================================================
# Feature Engineering and Selection
# ======================================================

data2 <- data2 %>%
  arrange(customerId, transactionDateTime) %>%
# Identify the time between the last purchase and the current purchase
# for the same `customerId`
  mutate(time_from_previous = ifelse (dplyr::lag(customerId,
                                                 n = 1,
                                                 default = NA) == customerId,
                                      round(as.numeric(transactionDateTime - dplyr::lag(transactionDateTime,
                                                                                        n = 1,
                                                                                        default = NA))/60),
                                      NA)) %>%
  group_by(customerId) %>%
  mutate(total_trans_count = n(),
# For every "NA" `time_from_previous` value, populate it with the customer's
# mean `time_from_previous` value
         time_from_previous = ifelse (is.na(time_from_previous),
                                      mean(time_from_previous, na.rm = T),
                                      time_from_previous),
# Calculate the z-score for each member's `time_from_previous` to understand
# relative distributions of transaction timing for each member
         LogCustTime = ifelse(abs(transactionAmount) <= 1,
                              0,
                              sign(transactionAmount)*log10(abs(transactionAmount))),
# Calculate the dates
         transaction_date = as.Date(as.character(substr(transactionDateTime, 1, 10))),
# Calculate the time of day that the transactions occurred
         time_of_day = as.numeric(format(transactionDateTime, "%H")) +
                       as.numeric(format(transactionDateTime, "%M"))/60,
# Calculate the hour of day that the transaction occurred
         hour_of_day = ifelse(nchar(as.character(floor(time_of_day))) == 1,
                              paste0("0", as.character(floor(time_of_day))),
                              as.character(floor(time_of_day))),
# Calculate the log of each member's `transactionAmount` to understand
# relative distributions of transaction amounts for each member
         LogCustAmt = ifelse(abs(transactionAmount) <= 1,
                             0,
                             sign(transactionAmount)*log10(abs(transactionAmount))))


data2.by_member <- data2 %>%
 group_by(customerId) %>%
 summarise(total_trans = n(),
           total_dollars = sum(transactionAmount))

# Calculate the percent of transactions for each merchant category by member
data2.cat.a <- data2 %>%
  group_by(customerId, merchantCategoryCode) %>%
  summarise(category_trans = n(),
            category_dollars = sum(transactionAmount))

data2.cat.b <- dplyr::inner_join(data2.cat.a, data2.by_member, by = "customerId") %>%
  mutate(perc_cat_trans = category_trans / total_trans,
         perc_cat_dollars = category_dollars / total_dollars)

# Calculate the percent of transactions for each hour of the day by member
data2.hour.a <- data2 %>%
 group_by(customerId, hour_of_day) %>%
 summarise(hour_trans = n(),
           hour_dollars = sum(transactionAmount))

data2.hour.b <- dplyr::inner_join(data2.hour.a, data2.by_member, by = "customerId") %>%
 mutate(perc_hour_trans = hour_trans / total_trans,
        perc_hour_dollars = hour_dollars / total_dollars) %>%
  select(-total_trans, -total_dollars)

# Join the data2.cat.b and data2.hour.b to the transaction level data set
data3 <- data2 %>%
  dplyr::inner_join(data2.cat.b, by = c("customerId", "merchantCategoryCode")) %>%
  dplyr::inner_join(data2.hour.b, by = c("customerId", "hour_of_day"))

# Standardize the character and logical variables to factor variables
data3[sapply(data3, is.character)] <- lapply(data3[sapply(data3, is.character)],
                                             as.factor)
data3[sapply(data3, is.logical)] <- lapply(data3[sapply(data3, is.logical)],
                                            as.factor)


# ======================================================
# Explore Data
# ======================================================

# Create a plot to trend the non-fraud and fraud transactions over time
ts1 <- data3 %>%
  group_by(transaction_date, isFraud) %>%
  summarise(amounts = sum(transactionAmount),
            transactions = n())
ts2 <- ts1 %>%
  group_by(transaction_date) %>%
  summarise(total_daily_amounts = sum(amounts),
            total_daily_transactions = sum(transactions))
ts <- dplyr::left_join(ts1, ts2, by = "transaction_date") %>%
  mutate(perc_total_amount = amounts / total_daily_amounts,
         perc_total_transactions = transactions / total_daily_transactions)

ts_plot <- ggplot(data = ts, aes(x = transaction_date, y = transactions, color = isFraud)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(isFraud ~ ., scales = "free") +
  labs(title = "Non-Fraud and Fraud Transaction Counts over Time",
       x = "Transaction Date",
       y = "Transaction Count")
plot(ts_plot)


# Create a plot to visualize the percentage of fraud transactions
ts_perc_fraud <- ggplot(data = subset(ts, isFraud == TRUE),
                        aes(x = transaction_date, y = perc_total_transactions)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(title = "Percentage of Fraudulent Transactions over Time",
       x = "Transaction Date",
       y = "Fraud Percentage of Transactions")
plot(ts_perc_fraud)


# Create a plot to visualize the trends of non-fraud and fraud transactions by hour
hours1 <- subset(data3, !is.na(hour_of_day)) %>%
  group_by(hour_of_day, isFraud) %>%
  summarise(amounts = sum(transactionAmount),
            transactions = n())

hours2 <- hours1 %>%
  group_by(hour_of_day) %>%
  summarise(total_hourly_amounts = sum(amounts),
            total_hourly_transactions = sum(transactions))

hours <- dplyr::left_join(hours1, hours2, by = "hour_of_day") %>%
  mutate(perc_total_amount = amounts / total_hourly_amounts,
         perc_total_transactions = transactions / total_hourly_transactions)

hours_plot <- ggplot(data = hours, aes(x = hour_of_day, y = transactions, color = isFraud)) +
  geom_col() +
  facet_wrap(. ~ isFraud, scales = "free", ncol = 1) +
  labs(title = "Transaction Counts by Hour of Day",
       x = "Hour of Day",
       y = "Transaction Count")
plot(hours_plot)


# View distribution of fraud transactions by merchant category
merch_cat_trans <- data3 %>%
  filter(isFraud == "TRUE") %>%
  group_by(merchantCategoryCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pareto = cumsum(n) / sum(n))

merchant_category_count <- ggplot(data = merch_cat_trans, aes(x = as.factor(merchantCategoryCode), y = pareto)) +
  geom_col() +
  scale_x_discrete(limits = merch_cat_trans$merchantCategoryCode) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Pareto: Fraudulent Transactions by Merchant Category",
       x = "Merchant Category",
       y = "Fraudulent Transaction Percentage")
plot(merchant_category_count)

# View box-plot distributions of transactionAmount by merchant category and fraud label
merch_cat_dollar_dist <- ggplot(data = subset(data3, merchantCategoryCode %in% merch_cat_trans$merchantCategoryCode),
                                aes(x = as.factor(isFraud), y = transactionAmount)) +
  geom_boxplot() +
  facet_wrap(merchantCategoryCode ~ ., scale = "free") +
  labs(title = "Boxplots: Transaction Amounts by Merchant Category and Fraud Label",
       x = "Fraud Label",
       y = "Transaction Amount")
plot(merch_cat_dollar_dist)


# Plot box-plot distributions by fraud label
num_fields <- colnames(data3[sapply(data3, is.numeric)])

num_boxplots_df <- data.frame(value = numeric(),
                              label = factor(),
                              var = character())
for (i in num_fields) {
  value <- data3[, i]
  label <- data3[, c("isFraud")]
  ndf <- cbind(value, label)
  ndf$var <- colnames(data3[, i])
  colnames(ndf) <- c("value", "label", "var")
  num_boxplots_df <- rbind(num_boxplots_df, ndf)
}

num_value_dist <- ggplot(data = num_boxplots_df, aes(x = as.factor(label), y = value)) +
  geom_boxplot() +
  facet_wrap(var ~ ., scale = "free") +
  labs(title = "Boxplots: Transaction Amounts by Numeric Predictor and Fraud Label",
         x = "Fraud Label",
         y = "Value")
plot(num_value_dist)



# ======================================================
# Initial Feature Selection
# ======================================================

# Keep all the numeric predictor variables and
# all categorical variables with less than 50 categories but greater than 1 (to get rid of zero Variance)
num_data <- (data3[, num_fields])

cat_data <- data3[, !(colnames(data3) %in% num_fields)]
low_values <- sapply(cat_data, function(x) (length(unique(x)) < 51 & length(unique(x)) > 1))
cat_data_low <- cat_data[, low_values]

data4 <- cbind(num_data, cat_data_low)

str(data4)


# ======================================================
# Pre-Processsing
# ======================================================

# Ensure that there are no missing values in the outcome variable
percent_complete <- nrow(subset(data4, !is.na(isFraud))) / nrow(data4)
if (percent_complete == 1) {
  print("No missing values in the outcome variable.")
} else {
  data2 <- subset(data4, !is.na(isFraud))
  print(paste0(round(1 - percent_complete), 2),
               " of outcome variables are missing. They have been removed from scope.")
}


# Turn the outcome into a 0/1 label
data4$Class <- ifelse(data4$isFraud == "TRUE", "Fraud", "notFraud")
data4$isFraud <- NULL
table(data4$Class)


# Handle NA values
# Assuming that " " values are really NA values
data4[data4 == " "] <- NA
sapply(data4, function(x) sum(is.na(x)))


# One-hot encode the categorical variables
dummies <- dummyVars(" ~ .", data = data4[, -ncol(data4)])
model_data <- data.frame(predict(dummies, newdata = data4))
model_data$Class <- as.factor(data4$Class)
str(model_data)


# Split the data into training (70%) and testing (30%) sets
train_index <- createDataPartition(y = model_data$Class, p = 0.7, list = FALSE)
train.a <- model_data[train_index, ]
test.a <- model_data[!(as.numeric(rownames(model_data)) %in% train_index), ]

train_pred <- train.a %>% select(-Class)
test_pred <- test.a %>% select(-Class)


# Impute the values of numeric variables with the mean of the training set values
train_num <- train_pred[, 1:length(num_fields)]
test_num <- test_pred[, 1:length(num_fields)]

for(i in 1:ncol(train_num)) {
  train_num[is.na(train_num[,i]), i] <- mean(train_num[,i], na.rm = TRUE)
}

for(i in 1:ncol(test_num)) {
  test_num[is.na(test_num[,i]), i] <- mean(train_num[,i], na.rm = TRUE)
}


# Impute missing values of categorical variables with the mode of the training set values
train_cat <- train_pred[, (length(num_fields)+1):ncol(train_pred)]
test_cat <- test_pred[, (length(num_fields)+1):ncol(test_pred)]

Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

for(i in 1:ncol(train_cat)) {
  train_cat[is.na(train_cat[, i]), i] <- Mode(train_cat[, i])
}

for(i in 1:ncol(test_cat)) {
  test_cat[is.na(test_cat[, i]), i] <- Mode(train_cat[, i])
}


# Finalized data sets to be used in model development and testing
train_data <- cbind(train.a$Class, train_num, train_cat) %>%
  rename(Class = `train.a$Class`)

test_data <- cbind(test_num, test_cat)
test_label <- test.a$Class

# Reformat the column names to remove "."
tr_names <- colnames(train_data)
tr_names <- gsub("\\.", "", tr_names)
colnames(train_data) <- tr_names

te_names <- colnames(test_data)
te_names <- gsub("\\.", "", te_names)
colnames(test_data) <- te_names


# Remove all unnecessary objects from memory
rm(list=setdiff(ls(), c("data4",
                        "train_index",
                        "train_data",
                        "test_data",
                        "test_label",
                        "num_fields")))


# ======================================================
# Model Development - Random Forest
# ======================================================

# Train a Random Forest model with under-sampling during resample
fitControl <- trainControl(
    method = "cv",                     # k-fold cross validation
    number = 5,                        # Number of folds
    savePredictions = "final",         # Saves predictions for optimal tuning parameter
    classProbs = T,                    # Should class probabilities be returned
    summaryFunction = twoClassSummary, # Results summary function
    sampling = "down")                 # Under-sample the majority class w/in each cv split

set.seed(3219)
  rf_model_down <- train(Class ~ .,
                         data = train_data,
                         method ='rf',
                         tuneLength = 5,
                         trControl = fitControl,
                         metric = "ROC")


#Train a Random Forest model with up-sampling during resample
fitControl$sampling = "up"

set.seed(3219)
  rf_model_up<- train(Class ~ .,
                         data = train_data,
                         method ='rf',
                         tuneLength = 5,
                         trControl = fitControl,
                         metric = "ROC")


# Evaluate the Random Forest Models
models <- c("rf_model_down",
            "rf_model_up")
outcomes <- function(x) {
  model <- eval(parse(text = x))
  predictions <- predict(model, test_data) # Apply trained random forest model to test data
  sens <- sensitivity(predictions, test_label) # Calculate the sensitivity
  spec <- specificity(predictions, test_label) # Calculate the specificity
  binary_labels <- ifelse(test_label == "Fraud", 1, 0)
  binary_predictions <- ifelse(predictions == "Fraud", 1, 0)
  roc <- as.numeric(as.character(roc(binary_labels, binary_predictions)$auc)) # Calculate the AUC
  res <- data.frame(model = x,
                    auc = roc,
                    senstvty = sens,
                    specfcty = spec)
}
rf_results <- do.call("rbind", lapply(models, function(x) outcomes(x)))
print(rf_results)


# ======================================================
# Model Development - XGBoost
# ======================================================
xgb_imbalance_handling <- "upsample" # use either "downsample", "upsample", "smote"

train_pred <- data4[train_index, ]
test_pred <- data4[!as.numeric(rownames(data4)) %in% train_index, ]

train_pred$Class <- as.factor(train_pred$Class)
test_pred$Class <- as.factor(test_pred$Class)

# Impute the values of numeric variables with the mean of the training set values
train_num <- train_pred[, 1:length(num_fields)]
test_num <- test_pred[, 1:length(num_fields)]

for(i in 1:ncol(train_num)) {
train_num[is.na(train_num[,i]), i] <- mean(train_num[,i], na.rm = TRUE)
}

for(i in 1:ncol(test_num)) {
test_num[is.na(test_num[,i]), i] <- mean(train_num[,i], na.rm = TRUE)
}

train_data <- cbind(train_num, train_pred[, !colnames(train_pred) %in% num_fields])
test_data <- cbind(test_num, test_pred[, !colnames(test_pred) %in% num_fields])

if (xgb_imbalance_handling == "downsample") {

# Down-sample the majority class of the training data
set.seed(3219)
gb_train <- downSample(x = train_data %>% select(-Class),
                       y = train_data$Class)
table(gb_train$Class)

} else {

if (xgb_imbalance_handling == "upsample") {

# Down-sample the majority class of the training data
  set.seed(3219)
  gb_train <- upSample(x = train_data %>% select(-Class),
                       y = train_data$Class)
  table(gb_train$Class)

} else {

  if(xgb_imbalance_handling == "smote") {

# Use SMOTE method to synthetically balance the classes of training data
    set.seed(3219)
    gb_train <- SMOTE(Class ~ .,
                      train_data,
                      perc.over = 5000,
                      perc.under = 200)
    table(train_data$Class)
  }
}
}

gb_test <- test_data

# Create sparse matrix for numeric predictors
M.a <- sparse.model.matrix(~ creditLimit +
                           availableMoney +
                           transactionAmount +
                           currentBalance +
                           time_from_previous +
                           total_trans_count +
                           LogCustTime +
                           time_of_day +
                           LogCustAmt +
                           category_trans +
                           category_dollars +
                           total_trans +
                           total_dollars +
                           perc_cat_trans +
                           perc_cat_dollars +
                           hour_trans +
                           hour_dollars +
                           perc_hour_trans +
                           perc_hour_dollars -1,
                         data = rbind(gb_train[, num_fields],
                                      gb_test[, num_fields]))


# Create sparse matrix for categorical predictors
cats_train <- gb_train[, !(colnames(gb_train) %in% num_fields)] %>%
select(-Class)
cats_test <- gb_test[, !(colnames(gb_test) %in% num_fields)] %>%
select(-Class)

cats <- data.table(rbind(cats_train, cats_test))
cats$account <- c(1:nrow(cats))

# Identify unique categorical feature values for each account (record)
d1 <- cats[, list(account, acqCountry)]
d2 <- cats[, list(account, merchantCountryCode)]
d3 <- cats[, list(account, posEntryMode)]
d4 <- cats[, list(account, posConditionCode)]
d5 <- cats[, list(account, merchantCategoryCode)]
d6 <- cats[, list(account, transactionType)]
d7 <- cats[, list(account, cardPresent)]
d8 <- cats[, list(account, expirationDateKeyInMatch)]
d9 <- cats[, list(account, hour_of_day)]

d1[ , acqCountry:= paste0("acqCountry: ", acqCountry)]
d2[ , merchantCountryCode:= paste0("merchantCountryCode: ", merchantCountryCode)]
d3[ , posEntryMode:= paste0("posEntryMode: ", posEntryMode)]
d4[ , posConditionCode:= paste0("posConditionCode: ", posConditionCode)]
d5[ , merchantCategoryCode:= paste0("merchantCategoryCode: ", merchantCategoryCode)]
d6[ , transactionType:= paste0("transactionType: ", transactionType)]
d7[ , cardPresent:= paste0("cardPresent: ", cardPresent)]
d8[ , expirationDateKeyInMatch:= paste0("expirationDateKeyInMatch: ", expirationDateKeyInMatch)]
d9[ , hour_of_day:= paste0("hour_of_day: ", hour_of_day)]

names(d1) <- names(d2) <- names(d3) <- names(d4) <- names(d5) <- names(d6) <- names(d7) <- names(d8) <- c("account","feature_name")
d <- rbind(d1, d2, d3, d4, d5, d6, d7, d8)
rm(d1, d2, d3, d4, d5, d6, d7, d8)
d <- unique(d)
setkey(d, account)

# Creates a list of unique accounts (records)
ii <- as.character(unique(d$account))
# Creates a list of all unique feature_names
jj <- unique(d$feature_name)
# Creates a list the length of dd that gives each account a unique identifier from 1: the number of unique accounts
id_i <- match(d$account,ii)
# Same thing for feature_name
id_j <- match(d$feature_name,jj)
id_ij <- cbind(id_i,id_j)
# Creates a matrix frame that has the feature_names as column names and accounts as row names, and every point is blank
M.b <- Matrix(0,nrow=length(ii),ncol=length(jj),
            dimnames=list(ii,jj),sparse=T)
# If the account and feature_name are found together in the id_i data frame, then mark it as a 1 in the M.b matrix
M.b[id_ij] <- 1
rm(ii,jj,id_i,id_j,id_ij)

# Combine the numeric and categorical matrices
M <- cbind(M.a, M.b)


# Create xgb matrices for the xgboost model
gb_train_matrix <- M[1:nrow(gb_train), ]

set.seed(3219)
gbtrain_index <- createDataPartition(y = gb_train$Class, p = 0.95, list = FALSE)
trw_data <- gb_train_matrix[gbtrain_index[, 1], ]
tew_data <- gb_train_matrix[-gbtrain_index[, 1], ]

gb_test_matrix <- M[(nrow(gb_train)+1):nrow(M), ]

trw_label <- ifelse(gb_train[gbtrain_index[, 1], ]$Class == "Fraud", 1, 0)
tew_label <- ifelse(gb_train[-gbtrain_index[, 1], ]$Class == "Fraud", 1, 0)
test_label <- ifelse(gb_test$Class == "Fraud", 1, 0)

dtrain_tr <- xgb.DMatrix(data = trw_data, label = trw_label)
dtrain_te <- xgb.DMatrix(data = tew_data, label = tew_label)
dtest <- xgb.DMatrix(data = test_data, label = test_label)


# Train xgboost tree model
watchlist <- list(train = dtrain_tr, test = dtrain_te)

set.seed(3219)
bst <- xgb.train(data = dtrain_tr,
               watchlist = watchlist,
               eta = .1,
               nround = 400,
               max_depth = 4,
               booster = "gbtree",
               objective = "binary:logistic",
               eval_metric = "auc")

gb_pred <- as.factor(ifelse(predict(bst, gb_test_matrix) >= .5, "Fraud", "NotFraud")) #apply trained xgboost model to test data
test_label_comp <- as.factor(ifelse(test_label == 1, "Fraud", "NotFraud"))
sens <- sensitivity(gb_pred, test_label_comp)
spec <- specificity(gb_pred, test_label_comp)
gb_cm <- confusionMatrix(gb_pred, test_label_comp)
binary_predictions <- ifelse(gb_pred == "Fraud", 1, 0)
roc <- as.numeric(as.character(roc(test_label, binary_predictions)$auc)) # Calculate the AUC
gb_results <- data.frame(model = "xgboost upsample",
                 auc = roc,
                 senstvty = sens,
                 specfcty = spec)

print(gb_results)

xgb.importance(feature_names = NULL, model = bst)
