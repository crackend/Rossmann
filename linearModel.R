# Split train and validation sets
require(caret)
ind <- createDataPartition(trainTarget$Sales, p = 0.8, list = FALSE)
val <- cbind(trainCross[-ind, ], trainTime[-ind, ], Sales = trainTarget[-ind, ])
training <- cbind(trainCross[ind, ], trainTime[ind, ], Sales = trainTarget[ind, ])

# Model training
modelLinear <- lm(Sales ~ ., data = training)

# Training and validation RMSPE
trainPreds <- predict(modelLinear, training)
rmspe(trainPreds, training$Sales)
valPreds <- predict(modelLinear, val)
rmspe(valPreds, val$Sales)
