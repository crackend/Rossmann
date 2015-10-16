# Load the data
training <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
store <- read.csv("data/store.csv")

# Store table preprocessing
notMissInd <- !is.na(store$CompetitionOpenSinceYear)
store$CompetitionOpenSinceDate <- NA
store$CompetitionOpenSinceDate[notMissInd] <- as.Date(paste("1", 
                                                            store$CompetitionOpenSinceMonth[notMissInd], 
                                                            store$CompetitionOpenSinceYear[notMissInd], 
                                                            sep = "/"), 
                                                      "%d/%m/%Y")
store$Promo2 <- factor(store$Promo2)
notMissInd <- store$Promo2 == 1
store$Promo2SinceDate <- NA
store$Promo2SinceDate[notMissInd] <- as.Date(store$Promo2SinceWeek[notMissInd] * 7, 
                                             origin = as.Date(as.character(store$Promo2SinceYear[notMissInd]), "%Y"))
levels(store$PromoInterval) <- c(0, 1:3)
store$CompetitionDistance <- as.numeric(store$CompetitionDistance)
store <- store[, -(5:9)]

# training and test table preprocessing
facNames <- c("DayOfWeek", "Open", "Promo", "StateHoliday", "SchoolHoliday")
training[, facNames] <- data.frame(apply(training[, facNames], 2, as.factor))
test[, facNames] <- data.frame(apply(test[, facNames], 2, as.factor))
levels(test$StateHoliday) <- levels(training$StateHoliday)
training$Date <- as.numeric(as.Date(training$Date, "%Y-%m-%d"))
test$Date <- as.numeric(as.Date(test$Date, "%Y-%m-%d"))
levels(training$Open) <- c("Closed", "Open")
levels(test$Open) <- c("Closed", "Open")

# Join tables
training <- merge(training, store)[, -1]
test <- merge(test, store)[, -1]

# Split predictors
trainTime <- training[, c(2, 13, 14)]
trainTime <- data.frame(cbind(Date = trainTime[, 1], 
                              CompetitionOpenDateDiff = trainTime[, 1] - trainTime[, 2], 
                              Promo2DateDiff = trainTime[, 1] - trainTime[, 3]))
trainTime[is.na(trainTime)] <- -1e+4
trainCross <- training[, -c(2:4, 13, 14)]
trainCustomers <- data.frame(Customers = as.integer(training[, 4]))
trainTarget <- data.frame(Sales = as.numeric(training[, 3]))
testTime <- test[, c(3, 12, 13)]
testTime <- data.frame(cbind(Date = testTime[, 1], 
                              CompetitionOpenDateDiff = testTime[, 1] - testTime[, 2], 
                              Promo2DateDiff = testTime[, 1] - testTime[, 3]))
testTime[is.na(testTime)] <- -1e+4
testCross <- test[, -c(1, 3, 12, 13)]
testID <- data.frame(ID = as.integer(test[, 1]))

# Customers feature test set
modelCustom <- lm(Customers ~ ., data = cbind(trainCustomers, trainTime))
testCustomers <- data.frame(Customers = predict(modelCustom, testTime))
rm(list = c("store", "facNames", "notMissInd", "modelCustom", "training", "test"))
