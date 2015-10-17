# Split train and validation sets
require(caret)
require(xgboost)
ind <- createDataPartition(training$Sales, p = 0.9, list = FALSE)
valX <- training[-ind, -ncol(training)]
valY <- training[-ind, ncol(training)]
trainX <- training[ind, -ncol(training)]
trainY <- training[ind, ncol(training)]
trainMatrix <- xgb.DMatrix(data.matrix(trainX), label = trainY, missing = NA)
valMatrix <- xgb.DMatrix(data.matrix(valX), label = valY, missing = NA)

# Model training
evalMetric <- function(preds, dtrain) {
  return(list(metric = "RMSPE", value = rmspe(preds, getinfo(dtrain, "label"))))
}
rmspeObj <- function(preds, dtrain) {
  obs <- getinfo(dtrain, "label")
  grad <-  (-1 / obs + preds / (obs ^ 2))
  hess <- (1 / (obs ^ 2))
  return(list(grad = grad, hess = hess))
}
param <- list(max.depth = 8, eta = 0.25, obj = rmspeObj, eval_metric = evalMetric, 
              subsample = 0.7, colsample_bytree = 0.7)
watchlist <- list(eval = valMatrix, train = trainMatrix)
modelXgb <- xgb.train(param, data = trainMatrix, nround = 700, 
                      watchlist, early.stop.round = 50, maximize = FALSE)

# Submission
testPreds <- predict(modelXgb, data.matrix(test[, -ncol(test)]), missing = NA)
submission <- data.frame(Id = test$Id, Sales = testPreds)
write.csv(submission, "Data/submissionXgb.csv", row.names = FALSE)
