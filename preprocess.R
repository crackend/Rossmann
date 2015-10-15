# Load the data
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
store <- read.csv("data/store.csv")

# Store table preprocessing
notMissInd <- !is.na(store$CompetitionOpenSinceYear)
store$CompetitionOpenSinceDate <-NA
store$CompetitionOpenSinceDate[notMissInd] <- as.Date(paste("1", 
                                                            store$CompetitionOpenSinceMonth[notMissInd], 
                                                            store$CompetitionOpenSinceYear[notMissInd], 
                                                            sep = "/"), 
                                                      "%d/%m/%Y")
store$Promo2 <- factor(store$Promo2)
notMissInd <- store$Promo2 == 1
store$Promo2SinceDate <-NA
store$Promo2SinceDate[notMissInd] <- as.Date(store$Promo2SinceWeek[notMissInd] * 7, 
                                             origin = as.Date(as.character(store$Promo2SinceYear[notMissInd]), "%Y"))
levels(store$PromoInterval) <- c(NA, 1:3)
store <- store[, -c(5, 6, 8, 9)]

# Train and test table preprocessing
facNames <- c("DayOfWeek", "Open", "Promo", "StateHoliday", "SchoolHoliday")
train[, facNames] <- data.frame(apply(train[, facNames], 2, as.factor))
test[, facNames] <- data.frame(apply(test[, facNames], 2, as.factor))
levels(test$StateHoliday) <- levels(train$StateHoliday)
train$Date <- as.Date(train$Date, "%Y-%m-%d")
test$Date <- as.Date(test$Date, "%Y-%m-%d")

# Join tables
train <- merge(train, store)[, -1]
test <- merge(test, store)[, -1]
rm(list = c("store", "facNames", "notMissInd"))
