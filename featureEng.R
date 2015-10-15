# Feature extraction
train$CompetitionOpenSinceDateDiff <- train$Date - train$CompetitionOpenSinceDate
test$CompetitionOpenSinceDateDiff <- test$Date - test$CompetitionOpenSinceDate
train$Promo2SinceDateDiff <- train$Date - train$Promo2SinceDate
test$Promo2SinceDateDiff <- test$Date - test$Promo2SinceDate
excludeNames <- c("CompetitionOpenSinceDate", "Promo2SinceDate")
train <- train[, !names(train) %in% excludeNames]
test <- test[, !names(test) %in% excludeNames]
