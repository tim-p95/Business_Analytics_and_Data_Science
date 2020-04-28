#install.packages("randomForest")
library(ranger)

test$test_id = seq(1:nrow(test))

test$user_dob = NULL
train$user_dob = NULL
test$user_yob = NULL
train$user_yob = NULL

trainc = train[which(complete.cases(train)), ]
testc = test[which(complete.cases(test)), ]

trainc$order_item_id = NULL
#trainc$order_date = NULL
trainc$delivery_date_missing = NULL
trainc$delivery_time_negative = NULL
trainc$user_dob_missing = NULL
trainc$item_size = NULL
#trainc$item_size2 = NULL
trainc$item_color = NULL
trainc$female = NULL
trainc$user_yob = NULL
trainc$user_dob = NULL
#trainc$user_reg_date = NULL
#trainc$order_quarter = NULL
#trainc$reg_quarter = NULL
#trainc$delivery_quarter = NULL
#trainc$east = NULL
#trainc$order_year = NULL
#trainc$delivery_date = NULL

rf <- ranger(return ~ ., data = trainc, importance = "impurity", num.trees = 2000, mtry = 3, alpha = 0.001)
var_imp = data.frame("vars" = as.character(rownames(rf$variable.importance)), "importance" = rf$variable.importance)
barplot(var_imp$rf.variable.importance)
rownames(var_imp)

testc$prediction = predict(dt, newdata = testc, type = "prob")[,2]
head(pred.dt,10)

testc$prediction_class = sapply(testc$prediction, FUN = function(x) ifelse(x > 0.5, 1, 0))

test$prediction_class = 0

j = 1
for(i in testc$test_id){
  test$prediction_class[i] = testc$prediction_class[j]
  j = j + 1
}

sum(as.numeric(as.character(test$prediction_class)))/nrow(test)
sum(as.numeric(as.character(test$return)))/nrow(test)
sum(as.numeric(as.character(testc$return)))/nrow(testc)

sum(as.numeric(as.character(train$return)))/nrow(train)
sum(as.numeric(as.character(trainc$return)))/nrow(trainc)

accuracy = vector() #accuracy = correct predictions/total observations
accuracy["Model"] = sum(test$prediction_class == test$return)/length(test$prediction_class)
accuracy

#parameter tuning
# Load package {mlr}
library("mlr")

task <- makeClassifTask(data = train_small, target = "return", positive = "1")

View(listLearners())

rf <- makeLearner("classif.ranger", importance = "impurity", num.trees = 2000, mtry = 3)

## Tuning
# Hyperparameter setting
# Set the scale/range for your parameters
rf.parms <- makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = 6), # Number of features selected at each node, smaller -> faster
  makeIntegerParam("num.trees", lower = 300, upper = 2000) # Number of tree, smaller -> faster
) 

# Sampling strategy
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)

# How dense should the parameters be selected from the ranges?
tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)

# Start tuning with the defined options
tuning <- tuneParams(rf, task = task, resampling = rdesc, par.set = rf.parms, control = tuneControl, measures = mlr::acc)

tuning$x
#------------------------------------------------------------------------------------

rf <- ranger(return ~ ., data = known1, importance = "impurity",num.trees = 1000)

pred.rf = predict(rf, data = unknown1, predict.all = FALSE, num.trees = rf$num.trees, type = "response")

prediction_rf = data.frame(unknown1$order_item_id)
head(prediction_rf, 20)

#head(pred.rf,10)
prediction_rf$prediction_class = as.numeric(as.numeric(pred.rf$predictions))
#levels(pred.rf$predictions)

#conversion = function(x){ifesle(x == 1, 0, 1)}

prediction_rf$return = as.factor(prediction_rf$prediction_class - 1)
prediction_rf$prediction_class = NULL

colnames(prediction_rf) = c("order_item_id", "return")
head(prediction_rf, 50)

# Save predictions
write.csv(prediction_rf, file = "prediction_rf.csv", row.names = FALSE)
