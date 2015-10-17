# Split train and validation sets
require(caret)
library(h2o)
training$LogSales <- log(training$Sales)
ind <- createDataPartition(training$Sales, p = 0.9, list = FALSE)
val <- training[-ind, ]
training <- training[ind, ]
trainH2o <- as.h2o(training)
valH2o <- as.h2o(val)
testH2o <- as.h2o(test)

# H2O model training
h2o.init(nthreads = -1 ,max_mem_size = '4G')
featInd <- names(training)[!(names(training) %in% c("Sales", "LogSales"))]
modelRf <- h2o.randomForest(x = featInd, y = "LogSales", 
                            ntrees = 100, max_depth = 30, 
                            nbins_cats = 1115, training_frame = trainH2o)

# Validation RMSPE
valPreds <- as.data.frame(h2o.predict(modelRf, valH2o))
valPreds <- exp(valPreds[, 1])
rmspe(valPreds, val$Sales)

# Submission
testPreds <- as.data.frame(h2o.predict(modelRf, testH2o))
testPreds <- exp(testPreds[, 1])
submission <- data.frame(Id = test$Id, Sales = testPreds)
write.csv(submission, "data/submission2.csv",row.names=F)
