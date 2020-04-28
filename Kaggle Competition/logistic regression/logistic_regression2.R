library("LiblineaR")
library(caret)

train_small = data.frame(train[, c(1:4, 6, 8:10, 14:15)])
test_small = data.frame(test[, c(1:4, 6, 8:10, 14:15)])

train_numeric <- model.matrix(return~.-1, train_small)
head(train_numeric)

scaling_information <- preProcess(train_numeric, method=c("center","scale"), na.remove = FALSE)
scaling_information$mean

X_train <- train_numeric
X_test<- model.matrix(return~.-1, test)

y_train <- as.numeric(train$return == "1")
y_test <- as.numeric(test$return == "1")

X_train <- predict(scaling_information, newdata=X_train)
X_test <- predict(scaling_information, newdata=X_test)

lambda = 2
lasso <- LiblineaR(X_train, y_train, type=6, cost = 1/lambda, epsilon=0.0001)
ridge <- LiblineaR(X_train, y_train, type=0, cost = 1/lambda, epsilon=0.0001)

# Check out the models
str(lasso)
lasso$W

lr = glm(return~., data= data.frame(cbind("return" = y_train, X_train)), family = binomial(link="logit"))

cbind("no_penalty" = round(coef(lr), 2)[2:38], "lasso" = -1*round(lasso$W,2)[1:37], "ridge" = -1*round(ridge$W,2)[1:37])

yhat_full <- predict(lr, newdata = data.frame(X_test), type="response")
yhat_lasso <- predict(lasso, X_test, proba = TRUE, decisionValues = FALSE)
yhat_ridge <- predict(ridge, X_test, proba = TRUE, decisionValues = FALSE)

lr.roc = data.frame("no_regularization" = yhat_full, "Lasso" = yhat_lasso$probabilities[,2], "Ridge" = yhat_ridge$probabilities[,2])

h = HMeasure(true.class = y_test, scores = lr.roc)
plotROC(h, which = 1)

auc_compare <- h$metrics["AUC"]



#prediction for unknown data:
known_small = data.frame(known1_1[, c(1, 4, 6, 14, 15)])
unknown_small = data.frame(unknown1_1[, c(3, 5, 13, 14)])

head(known_small)
head(unknown_small)

lr = glm(return ~ ., data = known_small, family = binomial(link = "logit"))
#test$prediction = predict(lr, newdata = unknown, type = "response")
#unknown$prediction_class = ifelse(test$prediction > 0.5, "1", "0")

pred_unknown = predict(lr, newdata = unknown_small, type = "response")
pred_unknown1 = ifelse(pred_unknown > 0.5, "1", "0")
head(pred_unknown)
head(pred_unknown1)

prediction_lr = data.frame(unknown1$order_item_id, as.factor(pred_unknown1))
colnames(prediction_lr) = c("order_item_id", "return")
head(prediction_lr, 20)

# Save predictions
write.csv(prediction_lr, file = "prediction_lr4.csv", row.names = FALSE)



