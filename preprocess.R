# Load the data
require(data.table)
training <- data.frame(fread("data/train.csv"))
test <- data.frame(fread("data/test.csv"))
store <- data.frame(fread("data/store.csv"))

# Store table preprocessing
facNames <- c("StoreType", "Assortment", "Promo2", "PromoInterval")
store[, facNames] <- data.frame(sapply(store[, facNames], as.factor))
store$CompetitionDistance <- as.numeric(store$CompetitionDistance)
levels(store$PromoInterval) <- 0:3
store <- store[, -7]

# Training and test table preprocessing
training$DayOfWeek <- as.numeric(training$DayOfWeek)
test$DayOfWeek <- as.numeric(test$DayOfWeek)
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
trainTime$CompetitionOpen <- "NoCompetition"
ind <- trainTime$CompetitionOpenDays > 0
trainTime$CompetitionOpen[ind] <- as.character(cut(trainTime$CompetitionOpenDays[ind], 
                                                   c(0, 3 * 365, 6 * 365, 9 * 365, 12 * 365, max(trainTime$CompetitionOpenDays[ind])),
                                                   c("0-3Years", "3-6Years", "6-9Years", "9-12Years", ">12Years")))
trainTime$CompetitionOpen <- factor(trainTime$CompetitionOpen)
trainTime$Promo2Weeks <- 0
ind <- !is.na(trainTime[, 7])
trainTime$Promo2Weeks[ind] <- as.numeric(format(trainTime$Date[ind], "%W")) - trainTime$Promo2SinceWeek[ind] +
  (as.numeric(format(trainTime$Date[ind], "%Y")) - trainTime$Promo2SinceYear[ind]) * 52

trainTime$Promo2 <- "NoPromo2"
ind <- trainTime$Promo2Weeks > 0
trainTime$Promo2[ind] <- as.character(cut(trainTime$Promo2Week[ind], 
                                          c(0, 80, 160, 240, max(trainTime$Promo2Week[ind])),
                                          c("0-80Weeks", "80-160Weeks", "160-240Weeks", ">240Weeks")))
trainTime$Promo2 <- factor(trainTime$Promo2)
trainTime <- trainTime[, c("Year", "Month", "Day", "CompetitionOpen", "Promo2")]
trainStore <- training[, c("Store", "DayOfWeek", "Promo", "StateHoliday", "SchoolHoliday", "StoreType",
                           "Assortment", "CompetitionDistance", "PromoInterval")]
trainCustomers <- as.integer(training[, "Customers"])
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
testTime$CompetitionOpen <- "NoCompetition"
ind <- testTime$CompetitionOpenDays > 0
testTime$CompetitionOpen[ind] <- as.character(cut(testTime$CompetitionOpenDays[ind], 
                                                  c(0, 3 * 365, 6 * 365, 9 * 365, 12 * 365, max(testTime$CompetitionOpenDays[ind])), 
                                                  c("0-3Years", "3-6Years", "6-9Years", "9-12Years", ">12Years")))
testTime$CompetitionOpen <- factor(testTime$CompetitionOpen)
testTime$Promo2Weeks <- 0
ind <- !is.na(testTime[, 7])
testTime$Promo2Weeks[ind] <- as.numeric(format(testTime$Date[ind], "%W")) - testTime$Promo2SinceWeek[ind] + 
  (as.numeric(format(testTime$Date[ind], "%Y")) - testTime$Promo2SinceYear[ind]) * 52

testTime$Promo2 <- "NoPromo2"
ind <- testTime$Promo2Weeks > 0
testTime$Promo2[ind] <- as.character(cut(testTime$Promo2Week[ind], 
                                         c(0, 80, 160, 240, max(testTime$Promo2Week[ind])),
                                         c("0-80Weeks", "80-160Weeks", "160-240Weeks", ">240Weeks")))
testTime$Promo2 <- factor(testTime$Promo2)
testTime <- testTime[, c("Year", "Month", "Day", "CompetitionOpen", "Promo2")]
testStore <- test[, c("Store", "DayOfWeek", "Promo", "StateHoliday", "SchoolHoliday", "StoreType", 
                      "Assortment", "CompetitionDistance", "PromoInterval")]
modelCustom <- lm(Customers ~ ., data = cbind(Customers = trainCustomers, trainTime))
testCustomers <- predict(modelCustom, testTime)
testId <- data.frame(Id = as.numeric(test[, "Id"]))
test <- cbind(testStore, testTime, testId)
rm(list = ls()[!ls() %in% c("training", "test", "trainCustomers", "testCustomers")])
