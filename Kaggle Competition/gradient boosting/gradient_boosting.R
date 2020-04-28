library(caret)
library(mlr)

test$test_id = seq(1:nrow(test))

test$user_dob = NULL
train$user_dob = NULL
test$user_yob = NULL
train$user_yob = NULL

trainc = train[which(complete.cases(train)), ]
testc = test[which(complete.cases(test)), ]

trainc$order_item_id = NULL
#trainc$order_date = NULL
trainc$user_title = NULL
trainc$delivery_date_missing = NULL
trainc$delivery_time_negative = NULL
trainc$user_dob_missing = NULL
trainc$item_size = NULL
trainc$item_size2 = NULL
trainc$item_color = NULL
#trainc$female = NULL
trainc$user_yob = NULL
trainc$user_dob = NULL
#trainc$user_reg_date = NULL
trainc$order_quarter = NULL
trainc$reg_quarter = NULL
trainc$delivery_quarter = NULL
trainc$east = NULL
trainc$order_year = NULL
trainc$delivery_year = NULL
trainc$yob_cat = NULL
trainc$reg_year = NULL

trainc$order_date = unclass(trainc$order_date)
trainc$delivery_date = unclass(trainc$delivery_date)
trainc$user_reg_date = unclass(trainc$user_reg_date)



testc$order_item_id = NULL
#testc$order_date = NULL
testc$user_title = NULL
testc$delivery_date_missing = NULL
testc$delivery_time_negative = NULL
testc$user_dob_missing = NULL
testc$item_size = NULL
testc$item_size2 = NULL
testc$item_color = NULL
#testc$female = NULL
testc$user_yob = NULL
testc$user_dob = NULL
#testc$user_reg_date = NULL
testc$order_quarter = NULL
testc$reg_quarter = NULL
testc$delivery_quarter = NULL
testc$east = NULL
testc$order_year = NULL
testc$delivery_year = NULL
testc$yob_cat = NULL
testc$reg_year = NULL

testc$order_date = unclass(testc$order_date)
testc$delivery_date = unclass(testc$delivery_date)
testc$user_reg_date = unclass(testc$user_reg_date)

# Prepare the mlr task
# Xgboost doesn't take categorical variables as input
train_dummy <- mlr::createDummyFeatures(trainc, target="return")
test_dummy <- mlr::createDummyFeatures(testc, target="return")

cbind(colnames(train_dummy), colnames(test_dummy))

test_dummy$test_id = NULL

task <- makeClassifTask(data = train_dummy, target = "return", positive = "1")

#install.packages("xgboost")
library(xgboost)

xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", # prediction type needs to be specified 
                           par.vals = list("verbose" = 0,
                           "early_stopping_rounds"=10)) # early stopping when no improvement xgb.learner
xgb.learner

# Set tuning parameters
xgb.parms <- makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.1),
  makeIntegerParam("nrounds", lower=40, upper=300),
  makeIntegerParam("max_depth", lower=2, upper=6),
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 1),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 0.9)
)

# How dense should the parameters be selected from the ranges?
tuneControl <- makeTuneControlRandom(maxit=2, tune.threshold = FALSE)

# We do 3-fold cross-validation, given the small data more folds might be better
rdesc <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)

library("parallelMap")
library(parallel)

parallelStartSocket(3, level = "mlr.tuneParams")
set.seed(123)

RNGkind("L'Ecuyer-CMRG")

clusterSetRNGStream(iseed = 1234567)
# Tune parameters as before
xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)
parallelStop()

# Extract optimal parameter values after tuning
xgb.tuning$x

# Update the learner to the optimal hyperparameters
xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
xgb.learner

#fixed
xgb.learner <- setHyperPars(xgb.learner, `eta` = 0.08794438, nrounds =  296, max_depth = 6, gamma = 0, colsample_bytree = 1,
                            min_child_weight = 1, subsample = 0.9)

# Train the model on the full training data (not only a CV-fold)
xgb = mlr::train(xgb.learner, task = task)



#make predictions
pred.xgb = predict(xgb, newdata = test_dummy, simplify=FALSE)
testc$prediction_class = as.numeric(as.character(pred.xgb$data$response))
head(testc$prediction_class)

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


performance(pred.xgb, auc)
performance(pred.xgb, acc)


##prediction for submission

known_small = data.frame(known1_1[, c(1, 2, 3, 4, 6, 7, 8, 9, 10, 16, 19, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31)])
unknown_small = data.frame(unknown1_1[, c(1, 2, 3, 5, 6, 7, 8, 9, 15, 18, 19, 22, 23, 24, 25, 26, 27, 28, 29, 30)])

#c(1, 2, 3, 4, 6, 7, 8, 9, 10, 16, 19, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31)-1

colnames(known_small)
colnames(unknown_small)

unknown_small$return = 0

# Prepare the mlr task
# Xgboost doesn't take categorical variables as input
known_dummy <- mlr::createDummyFeatures(known_small, target="return")
unknown_dummy <- mlr::createDummyFeatures(unknown_small, target="return")

known_dummy$delivery_date = NULL
unknown_dummy$delivery_date = NULL

task <- makeClassifTask(data = known_dummy, target = "return", positive = "1")

#install.packages("xgboost")
library(mlr)
library(xgboost)

xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", # prediction type needs to be specified 
                           par.vals = list("verbose" = 0,
                           "early_stopping_rounds"=10)) # early stopping when no improvement xgb.learner
xgb.learner

# Set tuning parameters
xgb.parms <- makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.1),
  makeIntegerParam("nrounds", lower=40, upper=300),
  makeIntegerParam("max_depth", lower=2, upper=6),
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 1),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 0.9)
)

# How dense should the parameters be selected from the ranges?
#tuneControl <- makeTuneControlGrid(resolution=100, tune.threshold = FALSE)
tuneControl <- makeTuneControlRandom(maxit = 5, tune.threshold = FALSE)

# We do 3-fold cross-validation, given the small data more folds might be better
rdesc <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)

library("parallelMap")
library(parallel)

parallelStartSocket(3, level = "mlr.tuneParams")
set.seed(123)

RNGkind("L'Ecuyer-CMRG")

clusterSetRNGStream(iseed = 1234567)

# Tune parameters as before
xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)
parallelStop()

# Extract optimal parameter values after tuning
xgb.tuning$x

# Update the learner to the optimal hyperparameters
xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
xgb.learner

#fixed
xgb.learner <- setHyperPars(xgb.learner, `eta` = 0.0473418, nrounds = 256, max_depth = 4, gamma = 0, colsample_bytree = 1,
                           min_child_weight = 1, subsample = 0.9)

# Train the model on the full training data (not only a CV-fold)
system.time(
xgb = mlr::train(xgb.learner, task = task)
)

#make predictions

pred.xgb = predict(xgb, newdata = unknown_dummy, simplify=FALSE)


prediction_gb = data.frame("order_item_id" = unknown$order_item_id, "return" = pred.xgb$data$response)

prediction_gb$return[which(is.na(unknown$delivery_date))] = 0

#sum(is.na(unknown1$delivery_date))


write.csv(prediction_gb, file = "prediction_xgb4.csv", row.names = FALSE)


