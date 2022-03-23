
# take home final R Markdown

# load the data set:
fraud <- read.csv("../ECON 621/cc_fraud.csv", stringsAsFactors = TRUE)

# getting the dimension:
dim(fraud)

# getting a look at the data:
head(fraud)
colnames(fraud)

# data types:
str(fraud)

# review of the data set details:
summary(fraud)
# it looks like there are some columns at the end that are worth dropping
# there are some variables that have missing values that should be replaced with na

# taking a closer look at of the columns
unique(fraud$merchantCity)
# this column doesn't contain any formation so it is worth dropping

# there are some missing values to fix:
# there are some missing values to fix:
# assign columns with missing values to a new object
na_cols <- c("acqCountry", "merchantCountryCode", "transactionType")

# we want to convert proxy NA values to nas
nas <- c("", "n/a", "N/A")

# assign NA as the new name for the missing values
fraud[, na_cols] <- sapply(fraud[, na_cols], function(x){
  x[x %in% nas] <- NA
  return(x)
})

table(apply(fraud, 1, function(x) {any(is.na(x))}))

# checking to make sure replacing na was successful: 
summary(fraud)


fraud <- fraud[, !(colnames(fraud) %in% c("merchantCity", "merchantState", "merchantZip","posOnPremises", "recurringAuthInd", "echoBuffer"))]
colnames(fraud)

# looking at the distribution of the categorical variables:
cat_cols <- c("merchantName", "acqCountry", "merchantCountryCode", "merchantCategoryCode", "transactionType")
sapply(fraud[, cat_cols], function(x) {
  c(head(sort(table(x)/length(x), decreasing = T)), n_unique_vals = length(unique(x)))
})

# histograms for the numeric variables:
summary(fraud)
hist(fraud$creditLimit, main = "Distribution of Credit Limit", xlab = "Credit Limit")
hist(fraud$availableMoney, main = "Distribution of Available Money", xlab = "Available Money")
# something identifiable here is that the data is skewed strongly to the right, with the most observations at the low end of the amount of money available
hist(fraud$transactionAmount, main = "Distribution of Transaction Amounts", xlab = "Transaction Amounts")
hist(fraud$currentBalance, main = "Distribution of Current Balances", xlab = "Current Balance")
# the current balance amounts are consistent with the available money distribution
# this isn't too surprising since the two terms are in most cases synonymous 

# numeric variables that I didn't make into a histogram: accountNumber, CustomerId, posEntryMode, posConditionCode, cardCVV, enteredCVV, CardLast4Digits
# Because it is not very explanatory to consider their distribution, they are mostly just identifier used for people/cards

# examining the covariance between variables:
num_cols <- c("creditLimit", "availableMoney","transactionAmount", "currentBalance")
cor(fraud[, num_cols])
# it makes sense that we see availableMoney and CreditLimit strongly correlated, someone with a lot of money in the bank is likely to receive better credit offers like higher credit limits
# I'm surprised to see that availableMoney and currentBalance aren't more strongly correlated




