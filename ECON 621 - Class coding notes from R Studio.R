econ621_heights_cm <- c(186, 178, 188, 173, 158, 174, 165, 165, 157, 162.5, 181, 178.5, 177.8)

# add up all the heights: 
sum(econ621_heights_cm)

# find the shortest person in class:
min(econ621_heights_cm)

# find the tallest person in class:
max(econ621_heights_cm)

# find the number of heights listed:
length(econ621_heights_cm)

# find all the unique values:
unique(econ621_heights_cm)

# count how many unique values in list:
length(unique(econ621_heights_cm))

# class of the vector:
class(econ621_heights_cm)


#find the average height:
econ621_avg_height <- sum(econ621_heights_cm)/length(econ621_heights_cm)
econ621_avg_height

#or use:
mean(econ621_heights_cm)

# create a sequence, default interval of 1
seq(1,10)
#create a sequence, specify interval of 0.5
seq(1, 10, by = 0.5)

#--------------------------------------------


#--------------------------------------------

?seq
??seq

#--------------------------------------------

# Making a list
x <- list(1, "a", TRUE, c(4, 5, 6))
x

# Making a matrix
m <- matrix(1:6, nrow = 2, ncol = 3)
m

# Columns and rows can be named in the matrix
rownames(m) <- c("row1", "row2")
colnames(m) <- c("a", "b", "c")
m

# Using cbind & rbind
z <- 1:3
y <- 10:12
cbind(z, y)

#-------------------------------------------
x <- c(2.1, 4.2, 3.3, 5.4)

# pull out the first value
x[1]

# subset by pulling out the elements in index position 1 & 3
x[c(1,3)]

# gives index position of smalled to largest values
order(x)

# use the results to sort the values
x[order(x)]
# round about way of running the sort function

# subset again to pull our the lowest value
x[order(x)][1]

# negative integers omit elements at specific positions
x[-1]

# conditional vector sub-setting
x[x >3]
x[x <=2]
x[x >3 ]

# sub-setting matrices
a <- matrix(1:9, nrow=3)
colnames(a) <- c("a","b","c")
a

# selects first row, and the first and third columns
a[1,-2]

# how do we select multiple rows and columns? specify those rows/columns in sub-setting code
a[c(1,2), c(1,3)]

# select firs two rows and all columns
a[c(1, 2),]

# sub-setting data frames
wine_data <- read.csv("../ECON 621/wine_components.csv")

# get the first row and first column
wine_data[1,2]

#print all the data for the first wine
wine_data[1,]

# print all the alcohol content for all wines
wine_data[,2]

# print the type of wine for the first 5 wines
wine_data[1:5, 1]

# conditional sub-setting for data frames
# Suppose we only want to analyze wine with an alcohol content greater than 14%

# extract the alcohol content column
alcohol_content <- wine_data$alcohol
alcohol_content

# then find elements greater than 14%
alcohol_content > 14

# now we can extract all the wines with an alcohol content >14%
wine_data[alcohol_content>14, ]

# make a new object that only indludes strong wines
very_strong_wines <- wine_data[alcohol_content>14, ]
very_strong_wines

# we want to replace the term "Rose" with "Rosado" in the wine_data data frame
# first, check which wines are of the Rose type:
wine_data$wine_type == "Rose"

# replace
wine_data$wine_type[wine_data$wine_type == "Rose"] <- "Rosado"
summary(wine_data)

# shows frequency of each wine type
table(wine_data$wine_type)

wine_data

# find the dimensions of the data frame
dim(wine_data)

# more indepth overview of the data frame
str(wine_data)

# only column names
colnames(wine_data)

# is ask one of the variables in your data set?
"ash" %in% colnames(wine_data)

# are each of the columns in your data set named "ash"?
colnames(wine_data) %in% "ash"

# all function to return whether multiple columns are in a data frame
required_cols <- c("color_intensity", "hue", "price")
all(required_cols %in% colnames(wine_data))
# we get a false return because price is not included in the column names

# subset the vector of columns were looking for by the conditional response to 
# the %in% operator
required_cols[!required_cols %in% colnames(wine_data)]

# change a column name
colnames(wine_data)[colnames(wine_data) == "OD280_OD315_of_diluted_wines"] <- "protein content"
colnames(wine_data)

# how do missing variables/NAs affect analysis?
x <- c(1, 2, NA, 4)
max(x)
min(x)
mean(x)

# na breaks conditional statements too
NA >1
NA < 100
NA == 11

# test for the location of the missing values
is.na(x)

# test whether there is an NA value in any of the vectors
any(is.na(x))

# one option is to ignore the NA vlue and perform analysis on all of the other numerical values
max(x, na.rm = T)

sum(x, na.rm = T)

# assinging new values to the NA/missing values
x[is.na(x)] <- 0
x
# note: generally, this procedure should only be performed if there is strong evidence that all of the NAs/missing values should be zeros instead

# exploratory summary for one vector
summary(wine_data$color_intensity)
sd(wine_data$color_intensity)

table(wine_data$wine_type)
table(wine_data$malic_acid)
#table is the most useful for categorical/qualitative vectors, integer vectors, or vectors with fewer unique values
# we can use a conditional statement to return a more useful summary of this vector
table(wine_data$malic_acid > 2)
table(wine_data$alcohol >14)

unique(wine_data$wine_type)
duplicated(wine_data$malic_acid)

# what if we want to find the wines with magnesium level > 100 based on the wine_type
table(wine_data$wine_type, wine_data$magnesium >100)

# ____________________________________________
# changing variable classes
df<- data.frame(user = c(letters[1:5]), x = c("3", "1", "5", "3", "8"), stringsAsFactors = F)
class(df$x)

df$x_as_num <- as.numeric(df$x)
df

df<- data.frame(user = c(letters[1:5]), x = c("3", "1", "5", "3", "8"), stringsAsFactors = T)
class(df$x)

# we want to split this data by wine type
split(wine_data[, 1:5], wine_data$wine_type)

# use lapply to find the number of rows of each type of wine
lapply(split(wine_data[, 1:5], wine_data$wine_type), nrow)

# making a custom function
times2 <- function(x) {x*2}
times2((14))
times2(100.5)

# this function counts the rows in the data set based on the different wine types then finds the average value for malic acid and ask, and returns everything as a data frame
function(x) {
  data.frame(count = nrow(x), avg_magnesium = mean(x$malic_acid), avg_hue = mean(x$ash))
}
# lapply(split(wine_data[, 1:5], wine_data$wine_type), function(x) {
#   data.frame(count = nrow(x), avg_mag)}

  
# -----------------------------------------------------
# constructing an EDA (with assigning new values to the NAs_)

#strings as factos
surveys <- read.csv("../ECON 621/student_surveys.csv", stringsAsFactors = FALSE)
summary(surveys)
summary(as.factor((surveys$Gender)))

#make sure to set strings/character vectors as factors so R returns useful information in the summary
surveys <- read.csv("../ECON 621/student_surveys.csv", stringsAsFactors = TRUE)
summary(surveys)

#what are all the unique values in each column?
sapply(surveys[, na_cols], unique)
#in the output we can find the missing/na values that we want to address and clean up

#assign columns with missing values to a new object
na_cols <- c("Horoscope", "Subject", "SpendTime1", "SpendTime2", "Self1", "Self2", "Career", "Superpower")

#we want to convert proxy NA values to nas
nas <- c("", "n/a", "N/A")

#assign NA as the new name for the missing values
surveys[, na_cols] <- sapply(surveys[, na_cols], function(x){
  x[x %in% nas] <- NA
  return(x)
})

library(plyr)
#we need to reassemble the columns
ldply(sapply(surveys, function(x){table(is.na(x))}), rbind)

# ----------------------

#replace missing values in PhysActive with the median
surveys$PhysActive <- ifelse(is.na(surveys$PhysActive), median(surveys$PhysActive, na.rm = T), surveys$PhysActive)
# remove the missing value while calculating the median otherise will return an error (na.rm = T)

# ----------------------

#students in the career category responded with "I Don't Know" as a reponse. Convert NAs to "idk"
surveys$Career <- ifelse(is.na(surveys$Career), "idk", surveys$Careers)

#for other character variables, we'll convert 
surveys[, c("Subject", "Self1", "Self2", "Superpower")] <- sapply(surveys[, c("Subject", "Self1", "Self2", "Superpower")], function(x){
  x <- ifelse(is.na(x), "no response", x)
  return(x)
})

#check our conversions
sapply(surveys[, na_cols], unique)

#after our conversions, what is the number of observations with NAs
table(apply(surveys, 1, function(x) {any(is.na(x))}))

#what if we want to remove variables with a significant portion of NAs?
surveys <- surveys[!apply(surveys, 1, function(x) {any(is.na(x))}), ]

# ---------------------

#what if we want to convert a numeric vector into z-scores
surveys$ScreenTime_z <- scale(surveys$ScreenTime)
summary(surveys$ScreenTime)
summary(surveys$ScreenTime_z)

#what if we want to convert to percentile values
surveys$ScreenTime_perc <- ecdf(surveys$ScreenTime)(surveys$ScreenTime)
summary(surveys$ScreenTime_perc)

# _-------------------------

#we need to potentially account for zeros in our data frames as well. Check for the proportion of the observations that are zero. 
num_cols <- c("ScreenTime", "Sleep", "PhysActive", "HrsHomework")
ldply(sapply(surveys[, num_cols], function(x){table(x == 0)/length(x)}), rbind)

#finding the distribution of categorical and numeric values
#categorical variables first
cat_cols <- c("Gender", "Grade", "Horoscope", "Subject", "IntExt", "OptPest", "SpendTime1", "SpendTime2", "Self1","Self2", "Career", "Superpower")

#find frequency counts, proportions, and number of unique values
sapply(surveys[, cat_cols], function(x) {
  c(head(sort(table(x)/length(x), decreasing = T)), n_unique_values = length(unique(x)))
})

# examining relationships between variables
cor(surveys[ ,num_cols])

hist(wine_data$alcohol)

barplot(table(wine_data$wine_type))

#customizing a bar chart 
barplot(table(wine_data$wine_type), main = "Type of Wine", width = 0.75,
        col= c("firebrick4", "mistyrose1", "lemonchiffon"), space = 0.1,
        legend = row.names(table(wine_data$wine_type)), names.arg = F,
        xlim = c(0, 10), ylim = c(0,20))

airports <- read.csv("../ECON 621/airports.csv", stringsAsFactors = T)
summary(airports)

hist(airports$altitude)

# we can modify the number of bins in the histogram with the bins argument 
hist(airports$altitude, breaks=100)

# we can add lines to the graph to denote certain altitudes. we want to add a red line to represent the avg alt./airports and a blue
# line for altitude of 1 mile/5280ft
hist(airports$altitude, breaks=100)
abline(v = mean(airports$altitude), col = "red")
abline(v = 5280, col = "blue")

# scatterplot that shows the relationship between wine hue and color intensity
plot(wine_data$color_intensity, wine_data$hue)

# customize this graph
plot(wine_data$color_intensity, wine_data$hue, xlab = "Color Intensity", ylab = "Hue", main = "Why do a sctterplot with so few values?",
     cex = 2, pch  = 15, col = "firebrick")
abline(v = mean(wine_data$color_intensity), h=1, lty = 2)

library(ggplot2)
ggplot(data = wine_data, aes(x = wine_type)) + geom_bar()
ggplot(data = wine_data, aes(x = wine_type, fill = wine_type)) + geom_bar()

# ----------------------------

#building an automated process
pythagorean <- function(a, b) {
  hypotenuse <- sqrt(a^2 + b^2)
  return(hypotenuse)
}

pythagorean(a = 3, b = 4)

# ----------------------------

# age ifelse function that diverts code based on the reported age of an individual
age <- 22
if (age<21) {
  print("I'm sorry, I can't let you in here.")
} else {
  print("I have to check everyone. thanks")
}

# we nest an ifelse inside another ifelse
age <- 18
if (age == 21) {
  print("Do you have any other forms of identification?")
} else if (age < 21) {
   print("I'm sorry, I can't let you in here")
} else {
  print("I have to check everyone. thanks")
}

#---------------------
wine_data <- read.csv("../ECON 621/wine_components.csv")

# we want to determine whether a wine has less than 14% alcohol
ifelse(wine_data$alcohol < 14, 1, 0)

# we can add this wine content vector to the data frame as a new variable
wine_data$low_alc <- ifelse(wine_data$alcohol < 14, 1, 0)
head(wine_data)

# nesting ifelse statements inside functions
wine_data$alcohol_level <- ifelse(wine_data$alcohol <13, "Less than 13%",
                                  ifelse(wine_data$alcohol <14,
                                         "Between 13% and 14%", "MOre than 14%"))
head(wine_data[, c("wine_type", "alcohol", "alcohol_level")])

# --------------------------------

#our more robust R function
data_clean <- function(csv) {
  data <- wine_data <- read.csv("../ECON 621/wine_components.csv")
  print(all(c("wine_type", "alcohol", "magnesium", "color_intensity", "hue") %in% colnames(data)))
  colnames(data)[which(colnames(data == "OD280_OD315_of_diluted_wine"))] <- "protein_content"
  data[, sapply(data, is.factor)] <- sapply(data[, sapply(data, is.factor)], as.character)
  numeric_cols <- c("alcohol", "malic_acid", "ash", "alcalinity_of_ask", "magnesium")
  data[, numeric_cols] <- sapply(data[, numeric_cols], as.numeric)
  data$z_score_malic_acid <- scale(data$malic_acid)
  data[, c("wine_type", "alcohol", "magnesium", "color_intesity", "hue","z_score_malic_acid")]
}

# function report back
data_clean <- function(csv) {
  data <- wine_data <- read.csv("../ECON 621/wine_components.csv")
  if(all(c("wine_type", "alcohol", "magnesium", "color_intensity", "hue") %in% colnames(data)))
  {print("All required columns found")
  } else {
      print("Columns missing")
  }
  print("Converting columns")
  colnames(data)[which(colnames(data == "OD280_OD315_of_diluted_wine"))] <- "protein_content"
  if (any(sapply(data, is.factor))) {
    data[, sapply(data, is.factor)] <- sapply(data[ , sapply(data, is.factor)], as.character)
  }
  data[, sapply(data, is.factor)] <- sapply(data[, sapply(data, is.factor)], as.character)
  numeric_cols <- c("alcohol", "malic_acid", "ash", "alcalinity_of_ask", "magnesium")
  data[, numeric_cols] <- sapply(data[, numeric_cols], as.numeric)
  data$z_score_malic_acid <- scale(data$malic_acid)
  data[, c("wine_type", "alcohol", "magnesium", "color_intesity", "hue","z_score_malic_acid")]
}
