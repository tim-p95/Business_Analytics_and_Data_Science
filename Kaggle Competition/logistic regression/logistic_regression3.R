## lr with lasso and ridge

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
trainc$item_size2 = NULL
trainc$item_color = NULL
#trainc$female = NULL
trainc$user_yob = NULL
trainc$user_dob = NULL
#trainc$user_reg_date = NULL
#trainc$reg_quarter = NULL
#trainc$delivery_quarter = NULL
#trainc$east = NULL
#trainc$order_year = NULL
#trainc$delivery_year = NULL
#trainc$order_date = NULL
#trainc$delivery_time = NULL
#trainc$delivery_month = NULL
#trainc$item_color_freq = NULL
#trainc$yob_cat = NULL
#trainc$reg_month = NULL
#trainc$reg_year = NULL
#trainc$user_title = NULL
#trainc$order_quarter = NULL


library("LiblineaR")

train_numeric <- model.matrix(return~.-1, trainc) # training set
head(train_numeric)

library("caret")
scaling_information <- preProcess(train_numeric, method=c("center","scale"), na.remove = FALSE)
scaling_information$mean

# Now apply it to train and test dataset
X_train <- train_numeric
X_test<- model.matrix(return~.-1, testc)

y_train <- as.numeric(trainc$return == "1")
y_test <- as.numeric(testc$return == "1")

X_train <- predict(scaling_information, newdata=X_train)
X_test <- predict(scaling_information, newdata=X_test)

# Now let's try the L1 and L2
# Setting the penalty parameter lambda (refer to board)
lambda = 2
lasso <- LiblineaR(X_train, y_train, type=6, cost = 1/lambda, epsilon=0.0001)
ridge <- LiblineaR(X_train, y_train, type=0, cost = 1/lambda, epsilon=0.0001)

# Check out the models
str(lasso)
lasso$W

# Compare coefficients of standard and regularized logit models
# Benchmark (needs some tricks to work)
logit <- glm(return~., data= data.frame(cbind("return" = y_train, X_train)), family = binomial(link="logit"))

cbind("no_penalty" = round(coef(logit), 2)[2:107],"lasso" = -1*round(lasso$W,2)[1:106],
      "ridge" = -1*round(ridge$W,2)[1:106])


# Now let's make predictions and evaluate the performance.
yhat_full <- predict(logit, newdata = data.frame(X_test), type="response")
yhat_lasso <- predict(lasso, X_test, proba = TRUE, decisionValues = FALSE)
yhat_ridge <- predict(ridge, X_test, proba = TRUE, decisionValues = FALSE)

# make sure to extract the right values for evaluation
library("hmeasure")
lr.roc <- data.frame("no_regularization" = yhat_full, "Lasso" = yhat_lasso$probabilities[,2], "Ridge" = yhat_ridge$probabilities[,2] )
h <- HMeasure(true.class = y_test, scores = lr.roc)
plotROC(h, which = 1)

#Let screate the vector to compare the AUCs
auc_compare <- h$metrics["AUC"]


lr = glm(return ~ ., data = trainc, family = binomial(link = "logit"))
summary(lr)

testc$prediction = predict(lr, newdata = testc, type = "response")
testc$prediction_class = sapply(testc$prediction, FUN = function(x) ifelse(x > 0.5, 1, 0))

test$prediction_class = 0

j = 1
for(i in testc$test_id){
  test$prediction_class[i] = testc$prediction_class[j]
  j = j + 1
}

sum(as.numeric(as.character(test$prediction_class)))/nrow(test)

accuracy = vector() #accuracy = correct predictions/total observations
accuracy["Model"] = sum(test$prediction_class == test$return)/length(test$prediction_class)
accuracy

#last try acc = 0.625875 with brand and size woe

#summary(lr)


#confuion matrix
library(hmeasure)
library(caret)

test_small$prediction_class = as.factor(test_small$prediction_class)
confusionMatrix(data = test_small$prediction_class, reference = test_small$return)

#roc curve
predictions.roc <- data.frame(LR = test_small$prediction)

h = HMeasure(true.class = as.numeric(test_small$return == "1") , scores = predictions.roc)
plotROC(h, which = 1)
h$metrics["AUC"]


#prediction for unknown data:
knownc = known1[which(complete.cases(known1)), ]
unknownc = unknown1[which(complete.cases(unknown1)), ]

knownc$order_item_id = NULL
knownc$delivery_date_missing = NULL
knownc$delivery_time_negative = NULL
knownc$user_dob_missing = NULL
knownc$item_size = NULL
knownc$item_color = NULL
knownc$female = NULL
knownc$user_yob = NULL

str(knownc)

lr = glm(return ~ ., data = knownc, family = binomial(link = "logit"))
#test$prediction = predict(lr, newdata = unknown, type = "response")
#unknown$prediction_class = ifelse(test$prediction > 0.5, "1", "0")
summary(lr)

pred_unknown = predict(lr, newdata = unknownc, type = "response")
unknownc$prediction = ifelse(pred_unknown > 0.5, "1", "0")

sum(as.numeric(unknownc$prediction))/nrow(unknownc)

head(pred_unknown)

unknown_pred = data.frame("id" = seq(1:50000))
unknown_pred$pred = 0

j = 1
for(i in unknownc$order_item_id){
  unknown_pred$pred[i] = unknownc$prediction[j]
  j = j + 1
}

sum(as.numeric(as.character(unknown_pred$pred)))/50000
sum(as.numeric(as.character(known1$return)))/100000

prediction_lr = data.frame(unknown$order_item_id, as.factor(unknown_pred$pred))
colnames(prediction_lr) = c("order_item_id", "return")
head(prediction_lr, 20)

# Save predictions
write.csv(prediction_lr, file = "prediction_lr8.csv", row.names = FALSE)



