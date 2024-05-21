#Q1. Reading the csv file
Retest_data <- read.csv('C://Users//HP//OneDrive - Atlantic TU//Documents//Week-8//Retest(1).csv')
#Structure of the dataset
str(Retest_data)
#Viewing the Data
View(Retest_data)
# Starting 15 rows of the data 
head(Retest_data, 15)
# no of rows in data  = 88875
nrow(Retest_data)
 
#Q2. Changing the date column in the dataset to month, date, year
Retest_data$datetime <- as.Date(Retest_data$dateandtime, format="%m/%d/%Y")
Retest_data$datetime
# Display the structure to confirm the change
str(Retest_data$datetime)
str(Retest_data)

#Q3. Changing the names of few column
names(Retest_data)[names(Retest_data) == 'dateandtime'] <- 'DateTime'
names(Retest_data)[names(Retest_data) == 'duration..hours.min.'] <- 'TotalDuration'
names(Retest_data)[names(Retest_data) == 'duration..seconds.'] <- 'DurationSeconds'
names(Retest_data)[names(Retest_data) == 'value1'] <- 'MeanValues_1'
names(Retest_data)[names(Retest_data) == 'value2'] <- 'MeanValues_2'
# Display the names to confirm changes
names(Retest_data)
str(Retest_data)

#Q4. Converting the MeanValue_2 char to numeric data type
Retest_data$MeanValue_2 <- as.numeric(Retest_data$MeanValue_2)
# Show the new structure of the data frame
str(Retest_data)

#Q5. Checking the max,sum,percent,plotting nissing values from the dataset
library(mice)
library(VIM)
missing_data <- md.pattern(Retest_data, rotate.names = FALSE)
# Number of records with no missing data
no_missing_values<- sum(complete.cases(Retest_data))
# Number of variables with missing DateTime records
missing_datatime_var<- sum(is.na(Retest_data$DateTime))
# Variable with the largest number of missing data points
max_missing_data <- (Retest_data)[which.max(colSums(is.na(Retest_data))
# Percent of data available without missing points
percent_complete_data <- (no_missing_data_records / nrow(Retest_data)) * 100
# Display the missing data pattern
plotting <- aggr(Retest_data, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Retest_data), gap=3, ylab=c("Missing data","Pattern"))

#Q6.Removing the na values 
Retest_data <-na.omit(Retest_data) - nrow((Retest_data))
Retest_data
#Deleting the data from rows
delete_data <- nrow(Retest_data)
delete_data

#Q7
mean_values <- tapply(Retest_data$MeanValue_1, Retest_data$ID, mean, na.rm=TRUE)
barplot(mean_values, 
        main="Summary of Retest Information", 
        xlab="Retest ID", 
        ylab="Mean Value",
        col = "red", "blue",
        legend(rownames((mean_values),besied = TRUE)))
# Display which Retest ID has the highest mean values
max_mean_id <- names(which.max(mean_values))
# Display which area has the lowest mean values
min_mean_id <- names(which.min(mean_values))


#Q9 where country == gd and shape = disk
Retest_subset <- subset(Retest_data, country == "gb" & shape == "disk")
#no of subset rows 
nrow(Retest_subset)

# Q8: Sorting the data of shape and disk
Retest_data_sorted <- Retest_data[order(Retest_data$shape, Retest_data$city), ]
sorted_Retest_data <- Retest_data_sorted[, c("DateTime", "city", "country", "shape")]
# Starting 15 rows of the data 
head(sorted_Retest_data, 15)
#Structure of the dataset
str(sorted_Retest_data)







