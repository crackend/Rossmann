# Split train and validation sets
require(caret)
training$LogSales <- log(training$Sales)
ind <- createDataPartition(training$Sales, p = 0.9, list = FALSE)
val <- training[-ind, ]
training <- training[ind, ]

# Model training
modelLinear <- lm(LogSales ~ ., data = training)

# Validation RMSPE
valPreds <- predict(modelLinear, val)
valPreds <- exp(valPreds)
rmspe(valPreds, val$Sales)

# Submission
testPreds <- predict(modelLinear, test)
testPreds <- exp(testPreds)
submission <- data.frame(Id = test$Id, Sales = testPreds)
write.csv(submission, "data/submissionLinear.csv",row.names=F)
