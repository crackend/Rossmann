# Load the data
require(data.table)
require(Matrix)
training <- data.frame(fread("data/train.csv"))
test <- data.frame(fread("data/test.csv"))
store <- data.frame(fread("data/store.csv"))

# Store table preprocessing
facNames <- c("StoreType", "Promo2", "PromoInterval")
store[, facNames] <- data.frame(sapply(store[, facNames], as.factor))
store$Assortment <- factor(store$Assortment, levels = c("a", "b", "c"), ordered = TRUE)
store$CompetitionDistance <- as.numeric(store$CompetitionDistance)
store$CompetitionDistance [is.na(store$CompetitionDistance)] <- 0
levels(store$PromoInterval) <- 0:3
store <- store[, -7]

# Training and test table preprocessing
training$DayOfWeek <- factor(training$DayOfWeek, ordered = TRUE)
test$DayOfWeek <- factor(test$DayOfWeek, ordered = TRUE)
facNames <- c("Open", "Promo", "StateHoliday", "SchoolHoliday")
training[, facNames] <- data.frame(sapply(training[, facNames], as.factor))
test[, facNames] <- data.frame(sapply(test[, facNames], as.factor))
levels(test$StateHoliday) <- levels(training$StateHoliday)
training$Date <- as.Date(training$Date)
test$Date <- as.Date(test$Date)
training$Year <- as.numeric(format(training$Date, "%y"))
training$Month <- as.numeric(format(training$Date, "%m"))
training$Day <- as.numeric(format(training$Date, "%d"))
test$Year <- as.numeric(format(test$Date, "%y"))
test$Month <- as.numeric(format(test$Date, "%m"))
test$Day <- as.numeric(format(test$Date, "%d"))
levels(training$Open) <- c("Closed", "Open")
levels(test$Open) <- c("Closed", "Open")

# Join tables
training <- merge(training, store)
test <- merge(test, store)
training$Store <- as.factor(training$Store)
test$Store <- as.factor(test$Store)

# Remove zero sales
training <- training[training$Sales > 0, !names(training) %in% "Open"]
test <- test[, !names(test) %in% "Open"]

# Training set features
trainTime <- training[, c("Date", "Year", "Month", "Day", 
                          "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", 
                          "Promo2SinceWeek", "Promo2SinceYear")]
trainTime$CompetitionOpenDays <- 0
ind <- !is.na(trainTime[, 6])
trainTime$CompetitionOpenDays[ind] <- trainTime$Date[ind] - as.Date(paste(trainTime[ind, 6], 
                                                                          trainTime[ind, 5], 
                                                                          1, sep = "-"))
trainTime$CompetitionOpenDays[trainTime$CompetitionOpenDays < 0] <- 0
trainTime$Promo2Weeks <- 0
ind <- !is.na(trainTime[, 7])
trainTime$Promo2Weeks[ind] <- as.numeric(format(trainTime$Date[ind], "%W")) - trainTime$Promo2SinceWeek[ind] +
  (as.numeric(format(trainTime$Date[ind], "%Y")) - trainTime$Promo2SinceYear[ind]) * 52
trainTime$Promo2Weeks[trainTime$Promo2Weeks < 0] <- 0
trainTime <- trainTime[, c("Year", "Month", "Day", "CompetitionOpenDays", "Promo2Weeks")]
trainStore <- training[, c("Store", "DayOfWeek", "Promo", "StateHoliday", "SchoolHoliday", "StoreType",
                           "Assortment", "CompetitionDistance", "PromoInterval")]
trainTarget <- data.frame(Sales = as.numeric(training[, "Sales"]))
training <- cbind(trainStore, trainTime, trainTarget)

# Test set features
testTime <- test[, c("Date", "Year", "Month", "Day", 
                     "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", 
                     "Promo2SinceWeek", "Promo2SinceYear")]
testTime$CompetitionOpenDays <- 0
ind <- !is.na(testTime[, 6])
testTime$CompetitionOpenDays[ind] <- testTime$Date[ind] - as.Date(paste(testTime[ind, 6], 
                                                                        testTime[ind, 5], 
                                                                        1, sep = "-"))
testTime$Promo2Weeks <- 0
ind <- !is.na(testTime[, 7])
testTime$Promo2Weeks[ind] <- as.numeric(format(testTime$Date[ind], "%W")) - testTime$Promo2SinceWeek[ind] + 
  (as.numeric(format(testTime$Date[ind], "%Y")) - testTime$Promo2SinceYear[ind]) * 52
testTime <- testTime[, c("Year", "Month", "Day", "CompetitionOpenDays", "Promo2Weeks")]
testStore <- test[, c("Store", "DayOfWeek", "Promo", "StateHoliday", "SchoolHoliday", "StoreType", 
                      "Assortment", "CompetitionDistance", "PromoInterval")]
testId <- data.frame(Id = as.numeric(test[, "Id"]))
test <- cbind(testStore, testTime, testId)

# Convert variables to numeric
for (featName in c("Store", "DayOfWeek", "Promo", "SchoolHoliday", "PromoInterval")) {
  training[, featName] <- as.numeric(as.character(training[, featName]))
  test[, featName] <- as.numeric(as.character(test[, featName]))
}
for (featName in c("StateHoliday", "StoreType", "Assortment")) {
  levels(training[, featName]) <- seq_along(levels(training[, featName]))
  levels(test[, featName]) <- seq_along(levels(test[, featName]))
  training[, featName] <- as.numeric(training[, featName])
  test[, featName] <- as.numeric(test[, featName])
}
rm(list = ls()[!ls() %in% c("training", "test")])