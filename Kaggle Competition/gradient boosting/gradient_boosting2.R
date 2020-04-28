library(caret)
library(mlr)

#prepare data
known1$user_dob = NULL
known1$user_yob = NULL
known1$yob_cat = NULL

unknown1$user_dob = NULL
unknown1$user_yob = NULL
unknown1$yob_cat = NULL

trainc = known1[which(complete.cases(known1)), ]
testc = unknown1[which(complete.cases(unknown1)), ]

trainc$order_item_id = NULL
#trainc$order_date = NULL
#trainc$user_title = NULL
trainc$delivery_date_missing = NULL
trainc$delivery_time_negative = NULL
trainc$user_dob_missing = NULL
trainc$item_size = NULL
#trainc$item_size2 = NULL
trainc$item_color = NULL
#trainc$female = NULL
trainc$user_yob = NULL
trainc$user_dob = NULL
#trainc$user_reg_date = NULL
#trainc$order_quarter = NULL
trainc$reg_quarter = NULL
trainc$delivery_quarter = NULL
trainc$east = NULL
trainc$order_year = NULL
trainc$delivery_year = NULL
trainc$yob_cat = NULL
trainc$reg_year = NULL
trainc$delivery_month = NULL
trainc$female = NULL
trainc$reg_month = NULL
trainc$item_size_freq = NULL
trainc$item_color_freq = NULL


trainc$order_date = unclass(trainc$order_date)
trainc$delivery_date = unclass(trainc$delivery_date)
trainc$user_reg_date = unclass(trainc$user_reg_date)

unknown_ids = testc$order_item_id
testc$order_item_id = NULL
#testc$order_date = NULL
#testc$user_title = NULL
testc$delivery_date_missing = NULL
testc$delivery_time_negative = NULL
testc$user_dob_missing = NULL
testc$item_size = NULL
#testc$item_size2 = NULL
testc$item_color = NULL
#testc$female = NULL
testc$user_yob = NULL
testc$user_dob = NULL
#testc$user_reg_date = NULL
#testc$order_quarter = NULL
testc$reg_quarter = NULL
testc$delivery_quarter = NULL
testc$east = NULL
testc$order_year = NULL
testc$delivery_year = NULL
testc$yob_cat = NULL
testc$reg_year = NULL
testc$delivery_month = NULL
testc$female = NULL
testc$reg_month = NULL
testc$item_size_freq = NULL
testc$item_color_freq = NULL

testc$order_date = unclass(testc$order_date)
testc$delivery_date = unclass(testc$delivery_date)
testc$user_reg_date = unclass(testc$user_reg_date)


# Prepare the mlr task
# Xgboost doesn't take categorical variables as input
train_dummy <- mlr::createDummyFeatures(trainc, target="return")
test_dummy <- mlr::createDummyFeatures(testc)

task <- makeClassifTask(data = train_dummy, target = "return", positive = "1")
library("xgboost")

xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", # prediction type needs to be specified 
                           par.vals = list("verbose" = 0,
                           "early_stopping_rounds"=10)) 

# early stopping when no improvement xgb.learner
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
tuneControl <- makeTuneControlRandom(maxit=10, tune.threshold = FALSE)

# We do 3-fold cross-validation, given the small data more folds might be better
rdesc <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)
library("parallelMap")
library(parallel)
parallelStartSocket(3, level = "mlr.tuneParams")
set.seed(123) 
# Set seed for the local random number generator, e.g. the CV samples
# Set seed for the clusters. Select a random number generator for parallel computing
# and set the seed for the registered cluster
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

# Train the model on the full training data (not only a CV-fold)
model_library <- mlr::train(xgb.learner, task = task)

# Make prediction on test data
pred <- predict(model_library, newdata = test_dummy, simplify=FALSE)

sum(as.numeric(as.character(pred$data$response)))/length(pred$data$response)

prediction = data.frame("prediction" = pred$data$prob.1, "id" = unknown_ids)

unknown1$prediction_class = 0

j = 1
for(i in prediction$id){
  unknown1$prediction_class[i] = as.numeric(as.character(prediction$prediction[j]))
  j = j + 1
}

sum(as.numeric(as.character(unknown1$prediction_class)))/nrow(unknown1)

head(unknown_ids,30)
head(prediction$prediction,30)
head(unknown1$prediction_class,30)


prediction_xgb = data.frame("order_item_id" = unknown$order_item_id, "return" = unknown1$prediction_class)

write.csv(prediction_xgb, file = "prediction_xgb6.csv", row.names = FALSE)
