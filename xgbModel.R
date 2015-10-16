# Split train and validation sets
require(caret)
ind <- createDataPartition(trainTarget$Sales, p = 0.6, list = FALSE)
valX <- cbind(trainCross[-ind, ], trainTime[-ind, ])
valY <- trainTarget$Sales[-ind]
trainX <- cbind(trainCross[ind, ], trainTime[ind, ])
trainY <- trainTarget$Sales[ind]
trainMatrix <- xgb.DMatrix(data.matrix(trainX), label = trainY, missing = NA)
valMatrix <- xgb.DMatrix(data.matrix(valX), label = valY, missing = NA)

# Model training
param <- list(max.depth = 7, eta = 1, objective = "reg:linear")
modelXgb <- xgboost(param, data = trainMatrix, nround = 100, verbose = 1)

# RMSPE
trainPreds <- predict(modelXgb, data.matrix(trainX), missing = NA)
rmspe(trainPreds, getinfo(trainMatrix, "label"))
valPreds <- predict(modelXgb, data.matrix(valX), missing = NA)
rmspe(valPreds, getinfo(valMatrix, "label"))
