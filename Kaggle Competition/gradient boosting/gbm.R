library(caret)
library(mlr)

train_small = data.frame(train[, c(1, 2, 4, 6, 8, 9, 14, 15, 16, 17, 18)])
test_small = data.frame(test[, c(1, 2, 4, 6, 8, 9, 14, 15, 16, 17, 18)])


# Prepare the mlr task
# gbm doesn't take categorical variables as input
train_dummy <- mlr::createDummyFeatures(train_small, target="return")
test_dummy <- mlr::createDummyFeatures(test_small, target="return")

task <- makeClassifTask(data = train_dummy, target = "return", positive = "1")

#load GBM
#install.packages("gbm")
library(gbm)

getParamSet("classif.gbm")
g.gbm <- makeLearner("classif.gbm", predict.type = "response", distribution = "bernoulli", n.trees = 600, 
                     interaction.depth = 5, n.minobsinnode = 25, shrinkage = 0.5)

#specify tuning method
rancontrol <- makeTuneControlRandom(maxit = 5L)

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#parameters
gbm_par<- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)

#tune parameters
tune_gbm <- tuneParams(learner = g.gbm, task = task ,resampling = set_cv ,measures = acc, par.set = gbm_par, control = rancontrol)

#check CV accuracy
tune_gbm$y

#set parameters
final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

#train
gbm_model <- train(g.gbm, task)

prediction_gbm = predict(gbm_model, newdata = test_dummy)

test_small$prediction_class = prediction_gbm$data$response

accuracy = vector() #accuracy = correct predictions/total observations
accuracy["Model"] = sum(test_small$prediction_class == test_small$return)/length(test_small$prediction_class)
accuracy


##for unknown data
known_small = data.frame(known1_1[, c(1, 2, 4, 6, 8, 9, 14, 15, 16, 17, 18)])
unknown_small = data.frame(unknown1_1[, c(1, 3, 5, 7, 8, 13, 14, 15, 16, 17)])

colnames(known_small)
colnames(unknown_small)


# Prepare the mlr task
# Xgboost doesn't take categorical variables as input
train_dummy <- mlr::createDummyFeatures(known_small, target="return")
test_dummy <- mlr::createDummyFeatures(unknown_small)

train_dummy$return = factor(known1$return)

task <- makeClassifTask(data = train_dummy, target = "return", positive = "1")

#load GBM
#install.packages("gbm")
library(gbm)

getParamSet("classif.gbm")
g.gbm <- makeLearner("classif.gbm", predict.type = "response", distribution = "bernoulli", n.trees = 600, 
                     interaction.depth = 5, n.minobsinnode = 25, shrinkage = 0.5)

#train
gbm_model <- train(g.gbm, task)

prediction_gbm = predict(gbm_model, newdata = test_dummy)

prediction_gbm = data.frame("order_item_id" = unknown$order_item_id, "return" = prediction_gbm$data$response)

write.csv(prediction_gbm, file = "prediction_gbm.csv", row.names = FALSE)
