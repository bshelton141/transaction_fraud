# ======================================================
# Configure the environment
# ======================================================


# Install and load required packages
packages <- c("jsonlite",
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

# Identify how many values from each column are NULL
sapply(data, function(x) sum(is.na(x)))

# Identify how many values from each column are empty
sapply(data, function(x) sum(x == ""))

# Identify the distribution of each numeric field
num_fields <- colnames(data[sapply(data, is.numeric)])
summary(data[, num_fields])

cor(data[, num_fields], method = "pearson")

# Identify the unique elements of each non-numeric columns
non_num_data <- data[, !(colnames(data) %in% num_fields)]
unique_count <- sapply(non_num_data, function(x) length(unique(x)))
print(unique_count)

# Run a Chi-Squared Test for non-numeric values on columns where
# the number of unique values in the column is > 1 and < 51
low_values <- sapply(non_num_data, function(x) (length(unique(x)) < 51 & length(unique(x)) > 1))
chi2_data <- non_num_data[, low_values]
# #chi2_data <- chi2_data[, c(1, 4, 7)]
#
# start <- Sys.time()
# chi2_pvalues <- c()
# chi2_colnames <- c()
# chi2_rownames <- c()
# for (i in c(1:ncol(chi2_data))) {
#
#   chi2_col1 <- chi2_data[, i]
#   chi2_pvalues1 <- c()
#   chi2_rownames1 <- c()
#   for (j in c(1:ncol(chi2_data))) {
#     df_matrix <- as.data.frame.matrix(table(chi2_col1, chi2_data[, c(j)]))
#     chi2_values1 <- c(chisq.test(df_matrix, simulate.p.value = TRUE))
#     chi2_pvalues1 <- c(chi2_pvalues1, chi2_values1$p.value)
#   }
#   chi2_pvalues <- c(chi2_pvalues, chi2_pvalues1)
#   chi2_colnames <- c(, )
# }
# end <- Sys.time()
# end - start

# chi2_outer <- function(x) {
#   field1 <- chi2_data[, x]
#
#   chi2_inner <- function(y) {
#     field2 <- chi2_data[, y]
#     df_matrix <- as.data.frame.matrix(table(field1, field2))
#     chi2_values_y <- c(chisq.test(df_matrix, simulate.p.value = TRUE))
#     output1 <- data.frame(field1_name = colnames(chi2_data[1]),
#                          field2_name = colnames(chi2_data[y]),
#                          p_value = chi2_values_y$p.value)
#   }
#   output2 <- do.call("rbind", lapply(c(1:ncol(chi2_data)), function(y) chi2_inner(y)))
# }
# chi2_output <- do.call("rbind", lapply(c(1:ncol(chi2_data)), function(x) chi2_outer(x)))


# ======================================================
# Visualize the `transactionAmount`
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



# NEED TO STILL DESCRIBE THE DISTRIBUTION AND POSE A HYPOTHESIS


# ======================================================
# Data Wrangling: Removing Reversals
# It is assumed that the "PURCHASE" directly prior to the "REVERSAL" was not a real purchase,
# so both the "REVERSAL" and immediately prior "PURCHASE" for the same transaction are removed.
# ======================================================

data1 <- data %>%
  # create a key for identifying unique transactions
  mutate(transactionDate = substr(transactionDateTime, 1, 10),
         transactionKey = paste(accountNumber,
                                merchantName,
                                transactionDate,
                                transactionAmount, sep = "_")) %>%
  # order the data by unique transactions and time of swipe
  arrange(transactionKey, transactionDateTime) %>%
  # create a field to label a record with 1 when it either followed by a "REVERSAL" for the same transaction OR if it is a "REVERSAL" itself
  mutate(remove_id = ifelse((dplyr::lead(transactionKey,
                                         n = 1,
                                         default = "NA") == transactionKey
                             & dplyr::lead(transactionType) == "REVERSAL")
                            | transactionType == "REVERSAL",
                            1, 0)) %>%
  # remove the records that are immediately followed by a "REVERSAL" for the same transaction or are a "REVERSAL" themselves
  filter(remove_id != 1)

# count the number of transactions removed due to reversals
nrow(data) - nrow(data1)
# identify the percent of transactions removed due to reversals
(nrow(data1) - nrow(data)) / nrow(data)

# identify the dollar amount removed due to reversals
sum(data$transactionAmount) - sum(data1$transactionAmount)
# identify the percent of dollars removed due to reversals
(sum(data1$transactionAmount) - sum(data$transactionAmount)) / sum(data$transactionAmount)

# ======================================================
# Data Wrangling: Removing Multi-Swipes
# Because there could be legitimate multiple purchases for the same amount on the same day,
# (e.g., taking an Uber to and from a specific location), multi-swipes are being considered
# if the exact same transaction happens twice within 5 minutes or less.
# ======================================================

data2 <- data1 %>%
  mutate(transactionDateTime = as.POSIXct(strptime(gsub("T", " ", transactionDateTime), "%Y-%m-%d %H:%M:%OS")),
# Calculate the number of minutes between swipes for the same transactionKey
         time_from_previous = ifelse (dplyr::lag(transactionKey, n = 1, default = "NA") == transactionKey,
                                      round(as.numeric(transactionDateTime - dplyr::lag(transactionDateTime,
                                                                                        n = 1,
                                                                                        default = NA))/60),
                                      NA)) %>%
# Remove all records where the time of the duplicate transaction was within 5 minutes of the previous
  filter(time_from_previous >= 5 | is.na(time_from_previous)) %>%
  select(-remove_id, -time_from_previous)

# count the number of transactions removed due to multi-swipes
nrow(data1) - nrow(data2)
# identify the percent of transactions removed due to multi-swipes
(nrow(data2) - nrow(data1)) / nrow(data1)

# identify the dollar amount removed due to multi-swipes
sum(data1$transactionAmount) - sum(data2$transactionAmount)
# identify the percent of dollars removed due to multi-swipes
(sum(data2$transactionAmount) - sum(data1$transactionAmount)) / sum(data1$transactionAmount)

# FIGURE OUT WHAT TO DO WITH THE $0 AMOUNT TRANSACTIONS




# ======================================================
# ======================================================
# ======================================================
#                   MACHINE LEARNING
# ======================================================
# ======================================================
# ======================================================


# ======================================================
# Feature Engineering and Selection
# ======================================================


data2 <- data2 %>%
  arrange(customerId, transactionDateTime) %>%
# Identify the time between the last purchase and the current purchase
# for the same `customerId`
  mutate(time_from_previous = ifelse (dplyr::lag(customerId, n = 1, default = "NA") == customerId,
                                      round(as.numeric(transactionDateTime - dplyr::lag(transactionDateTime,
                                                                                        n = 1,
                                                                                        default = NA))/60),
                                      NA)) %>%
# Calculate the z-score for each member's time difference to understand
# relative distributions of transaction timing for each member
  group_by(customerId) %>%
  mutate(total_trans_count = n(),
         time_from_previous = ifelse (is.na(time_from_previous),
                                      mean(time_from_previous, na.rm = T),
                                      time_from_previous),
# Calculate the dates
         transaction_date = as.Date(as.character(substr(transactionDateTime, 1, 10))),
# Calculate the time of day that the transactions occurred
         time_of_day = as.numeric(format(transactionDateTime, "%H")) +
                      as.numeric(format(transactionDateTime, "%M"))/60,
         CustTimeZscore = (time_from_previous - mean(time_from_previous)) / sd(time_from_previous),
# Calculate the hour of day that the transaction occurred
         hour_of_day = ifelse(nchar(as.character(floor(time_of_day))) == 1,
                              paste0("0", as.character(floor(time_of_day))),
                              as.character(floor(time_of_day))),
# Calculate the z-score of each member's `transactionAmount` to understand
# relative distributions of transaction amounts for each member
         CustAmtZscore = ifelse(total_trans_count == 1,
                               0,
                               (transactionAmount - mean(transactionAmount)) / sd(transactionAmount)),
# Format the `merchantName2` field
         merchantName2 = gsub(" #.*", "", merchantName),
         merchantName2 = gsub("[[:punct:]]", "", merchantName2),
         merchantName2 = gsub(" ", "", merchantName2))


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

# ======================================================
# Explore Data
# ======================================================
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
  facet_wrap(. ~ isFraud, scales = "free", ncol = 1)

ts_perc_fraud <- ggplot(data = subset(ts, isFraud == TRUE),
                        aes(x = transaction_date, y = perc_total_transactions)) +
  geom_line()

hours1 <- data3 %>%
  group_by(hour_of_day, isFraud) %>%
  summarise(amounts = sum(transactionAmount),
            transactions = n())
hours2 <- hour1 %>%
  group_by(hour_of_day) %>%
  summarise(total_hourly_amounts = sum(amounts),
            total_hourly_transactions = sum(transactions))
hours <- dplyr::left_join(hours1, hours2, by = "hour_of_day") %>%
  mutate(perc_total_amount = amounts / total_hourly_amounts,
         perc_total_transactions = transactions / total_hourly_transactions)

hours_plot <- ggplot(data = hours, aes(x = hour_of_day, y = transactions, color = isFraud)) +
  geom_col() +
  facet_wrap(. ~ isFraud, scales = "free", ncol = 1)


ggplot(data = data3, aes(x = as.factor(isFraud), y = transactionAmount)) +
  geom_boxplot() + facet_wrap(. ~ merchantCategoryCode, scale = "free")






# ======================================================
# Initial Feature Selection
# ======================================================

# Keep all the numeric predictor variables and
# all categorical variables with less than 50 categories but greater than 1 (to get rid of zero Variance)
num_fields <- colnames(data3[sapply(data3, is.numeric)])
num_data <- (data3[, num_fields])

cat_data <- data2[, !(colnames(data2) %in% num_fields)]
low_values <- sapply(cat_data, function(x) (length(unique(x)) < 51 & length(unique(x)) > 1))
#cat_data_low <- cbind(cat_data[, low_values], cat_data[, c("merchantName2")])
cat_data_low <- cat_data[, low_values]

data4 <- cbind(num_data, cat_data_low)


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

# Handle NA values
# Assuming that " " values are really NA values
data4[data4 == " "] <- NA
sapply(data4, function(x) sum(is.na(x)))

# Turn the outcome into a 0/1 label
data4$Class <- ifelse(data4$isFraud == "TRUE", "Fraud", "notFraud")
data4$isFraud <- NULL

data4[sapply(data4, is.character)] <- lapply(data4[sapply(data4, is.character)],
                                             as.factor)
data4[sapply(data4, is.logical)] <- lapply(data4[sapply(data4, is.logical)],
                                            as.factor)

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

# Perform standardization on all values based on training set distributions
# scale_process <- preProcess(train_pred, method = c("center", "scale"))
# stand_train <- predict(scale_process, train_pred)
# stand_test <- predict(scale_process, test_pred)
stand_train <- train_pred
stand_test <- test_pred

#Impute the values of numeric variables with the mean of the training set values
train_num <- stand_train[, 1:length(num_fields)]
test_num <- stand_test[, 1:length(num_fields)]

for(i in 1:ncol(train_num)) {
  train_num[is.na(train_num[,i]), i] <- mean(train_num[,i], na.rm = TRUE)
}

for(i in 1:ncol(test_num)) {
  test_num[is.na(test_num[,i]), i] <- mean(train_num[,i], na.rm = TRUE)
}


# Impute missing values of categorical variables with the mode of the training set values
train_cat <- stand_train[, (length(num_fields)+1):ncol(stand_train)]
test_cat <- stand_test[, (length(num_fields)+1):ncol(stand_test)]

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


#transform predictor variables to principle components
set.seed(3219)
pre_pca <- preProcess(cbind(train_num, train_cat), method = "pca")
train_pca <- predict(pre_pca, cbind(train_num, train_cat))
test_pca <- predict(pre_pca, cbind(test_num, test_cat))



# Finalized data sets to be used in model development and testing
train_data <- cbind(train.a$Class, train_pca) %>%
  rename(Class = `train.a$Class`)

test_data <- test_pca
test_label <- test.a$Class

# Remove all unnecessary objects from memory
rm(list=setdiff(ls(), c("data4",
                        "train_index",
                        "train_data",
                        "test_data",
                        "test_label")))


# ======================================================
# Model Development - Random Forest
# ======================================================

# Train a Random Forest model with under-sampling during resample
set.seed(3219)
fitControl <- trainControl(
    method = "cv",                     # k-fold cross validation
    number = 5,                        # number of folds
    savePredictions = "final",         # saves predictions for optimal tuning parameter
    classProbs = T,                    # should class probabilities be returned
    summaryFunction = twoClassSummary, # results summary function
    sampling = "down")                 # under-sample the majority class w/in each cv split

# system.time(
#   rf_model_down <- train(Class ~ .,
#                          data = train_data,
#                          method ='rf',
#                          tuneLength = 5,
#                          trControl = fitControl,
#                          metric = "ROC")
# )

# Train a Random Forest model with SMOTE during resample
# set.seed(3219)
# fitControl$sampling = "smote"
#
# system.time(
#   rf_model_smote <- train(Class ~ .,
#                          data = train_data,
#                          method ='rf',
#                          tuneLength = 5,
#                          trControl = fitControl,
#                          metric = "ROC")
# )

# # Train a Random Forest model with ROSE during resample
set.seed(3219)
fitControl$sampling = "rose"

system.time(
  rf_model_rose <- train(Class ~ .,
                         data = train_data,
                         method ='rf',
                         tuneLength = 5,
                         trControl = fitControl,
                         metric = "ROC")
)




print(rf_model_rose) #print the trained model summary
train_plot <- plot(rf_model_rose) #plot the Kappa scores for the different ntree and mtry combinations
print(train_plot)

rf_pred <- predict(rf_model_rose, test_data) #apply trained random forest model to test data
rf_cm <- confusionMatrix(rf_pred, test_label)
rf_sens <- sensitivity(rf_pred, test_label)
rf_spec <- specificity(rf_pred, test_label)

rf_labels <- ifelse(test_label == "Fraud", 1, 0)
rf_predictions <- ifelse(rf_pred == "Fraud", 1, 0)
rf_roc <- roc(rf_labels, rf_predictions)
rf_auc <- auc(rf_roc)
print(rf_cm)
print(rf_auc)

#plot the top 20 most importance variables in the RF model
var_imp_plot <- plot(varImp(rf_model, scale = FALSE), top = 20)
print(var_imp_plot)


# ======================================================
# Model Development - XGBoost
# ======================================================
xgb_imblance_handling <- "smote" # use either "smote", "rose", "under", "over"

# Split the data4 data frame into train and test sets
gb_train1 <- data4[train_index, ]
gb_test1 <- data4[!(as.numeric(rownames(data4)) %in% train_index), ]

# isolate the numeric variables of both the train and test sets
num_fields <- colnames(gb_train1[sapply(gb_train1, is.numeric)])
gb_train_num1 <- gb_train1[, num_fields]
gb_test_num1 <- gb_test1[, num_fields]

#Impute the values of numeric variables with the mean of the training set values
for(i in 1:ncol(gb_train_num1)) {
  gb_train_num1[is.na(gb_train_num1[, i]), i] <- mean(gb_train_num1[,i], na.rm = TRUE)
}

for(i in 1:ncol(gb_test_num1)) {
  gb_test_num1[is.na(gb_test_num1[, i]), i] <- mean(gb_train_num1[,i], na.rm = TRUE)
}

# Center and scale both the train and test sets,
# based on the center and scale of the train set
scale_process <- preProcess(gb_train_num1, method = c("center", "scale"))
stand_train1 <- predict(scale_process, gb_train_num1)
stand_test1 <- predict(scale_process, gb_test_num1)

# Combine the newly-scaled numeric and categorical training variables back together
gb_train2 <- cbind(gb_train_num1, gb_train1[, !(colnames(gb_train1) %in% num_fields)])

# Down-sample the majority class of the training data


# Use SMOTE to synthetically balance the two classes in the training data
gb_train <- SMOTE(Class ~ .,
                  gb_train2,
                  perc.over = 500,
                  perc.under = 200)
table(gb_train$Class)

# Combine the newly-scaled numeric and categorical testing variables back together
gb_test <- cbind(gb_test_num1, gb_test1[, !(colnames(gb_test1) %in% num_fields)])

#create sparse matrix for numeric predictors
M.a <- sparse.model.matrix(~ creditLimit +
                             availableMoney +
                             transactionAmount +
                             currentBalance +
                             time_from_previous +
                             total_trans_count +
                             time_of_day +
                             CustTimeZscore +
                             CustAmtZscore +
                             category_trans +
                             category_dollars +
                             total_dollars +
                             perc_cat_trans +
                             perc_cat_dollars +
                             hour_trans +
                             hour_dollars +
                             perc_hour_trans +
                             perc_hour_dollars -1,
                           data = rbind(gb_train[, num_fields],
                                        gb_test[, num_fields]))

#create sparse matrix for categorical predictors
#################################################
cats_train <- gb_train[, !(colnames(gb_train) %in% num_fields)] %>%
  select(-Class)
cats_test <- gb_test[, !(colnames(gb_test) %in% num_fields)] %>%
  select(-Class)

cats <- data.table(rbind(cats_train, cats_test))
cats$account <- c(1:nrow(cats))

#identify unique categorical feature values for each account (record)
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
rm(d1, d2, d3, d4, d5, d6, d7, d8); gc()
d <- unique(d)
setkey(d, account)

#creates a list of unique accounts (records)
ii <- as.character(unique(d$account))
#creates a list of all unique feature_names
jj <- unique(d$feature_name)
#creates a list the length of dd that gives each account a unique identifier from 1: the number of unique accounts
id_i <- match(d$account,ii)
#same thing for feature_name
id_j <- match(d$feature_name,jj)
id_ij <- cbind(id_i,id_j)
#creates a matrix frame that has the feature_names as column names and accounts as row names, and every point is blank
M.b <- Matrix(0,nrow=length(ii),ncol=length(jj),
              dimnames=list(ii,jj),sparse=T)
#if the account and feature_name are found together in the id_i data frame, then mark it as a 1 in the M.b matrix
M.b[id_ij] <- 1
rm(ii,jj,id_i,id_j,id_ij);gc()

#combine the numeric and categorical matrices
M <- cbind(M.a, M.b)

#create xgb matrices for the xgboost model
#################################################
train_data <- M[1:nrow(gb_train), ]
trwr <- sample(1:nrow(train_data), round(.85*nrow(train_data), 0), replace = FALSE)
trw_data <- train_data[trwr, ]
tew_data <- train_data[-trwr, ]

test_data <- M[(nrow(gb_train)+1):nrow(M), ]

trw_label <- ifelse(gb_train[trwr, ]$Class == "Fraud", 1, 0)
tew_label <- ifelse(gb_train[-trwr, ]$Class == "Fraud", 1, 0)
test_label <- ifelse(gb_test$Class == "Fraud", 1, 0)

dtrain_tr <- xgb.DMatrix(data = trw_data, label = trw_label)
dtrain_te <- xgb.DMatrix(data = tew_data, label = tew_label)
dtest <- xgb.DMatrix(data = test_data, label = test_label)

#train xgboost tree model
##############################

watchlist <- list(train = dtrain_tr, test = dtrain_te)
bst <- xgb.train(data = dtrain_tr,
                 #watchlist = watchlist,
                 eta = .1,
                 nround = 200,
                 booster = "gbtree",
                 objective = "binary:logistic",
                 eval_metric = "auc")


gb_pred <- as.factor(ifelse(predict(bst, tew_data) >= .5, "Fraud", "NotFraud")) #apply trained xgboost model to test data
test_label_comp <- as.factor(ifelse(tew_label == 1, "Fraud", "NotFraud"))
gb_cm <- confusionMatrix(gb_pred, test_label_comp)

gb_predictions <- ifelse(gb_pred == "Fraud", 1, 0)
gb_roc <- roc(tew_label, gb_predictions)
gb_auc <- auc(gb_roc)
print(gb_auc)


#perform prediction
gb_pred <- as.factor(ifelse(predict(bst, test_data) >= .5, "Fraud", "NotFraud")) #apply trained xgboost model to test data
test_label_comp <- as.factor(ifelse(test_label == 1, "Fraud", "NotFraud"))
gb_cm <- confusionMatrix(gb_pred, test_label_comp)
print(gb_cm)
gb_sens <- sensitivity(gb_pred, test_label_comp)
gb_spec <- specificity(gb_pred, test_label_comp)

gb_predictions <- ifelse(gb_pred == "Fraud", 1, 0)
gb_roc <- roc(test_label, gb_predictions)
gb_auc <- auc(gb_roc)
print(gb_auc)



# IF I HAD MORE TIME:
# -- Recursive Feature Elimination
# -- Trying to improve performance by rebalancing the training set


t <- data.frame(a = c("Brandon", "Brandon", "Brandon", NA,
                      "Kyler", NA, "Kyler", "Kyler",
                      "Trent", "Trent", "Trent", "Trent"),
                b = c(1, 5, 3, 2,
                      13.2, 4.1, 13.2, 14.5,
                      26.2, 58.5, NA, 46.5))

s <- c(1, 4, 4, 2, 5, 3, 4, 5, 1, 1, 1, 1)
