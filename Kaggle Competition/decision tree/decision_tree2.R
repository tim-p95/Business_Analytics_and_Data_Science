#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

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
trainc$order_quarter = NULL
#trainc$reg_quarter = NULL
#trainc$delivery_quarter = NULL
#trainc$east = NULL
#trainc$order_year = NULL

dt = rpart(return~., trainc, method = "class", control = rpart.control(minsplit = 5, cp = 0.001)) #decision tree
testc$prediction = predict(dt, newdata = testc, type = "prob")[,2]
head(pred.dt,10)

testc$prediction_class = sapply(testc$prediction, FUN = function(x) ifelse(x > 0.50, 1, 0))

test$prediction_class = 0

j = 1
for(i in testc$test_id){
  test$prediction_class[i] = testc$prediction_class[j]
  j = j + 1
}

sum(as.numeric(as.character(test$prediction_class)))/nrow(test)
sum(as.numeric(as.character(train$return)))/nrow(train)

accuracy = vector() #accuracy = correct predictions/total observations
accuracy["Model"] = sum(test$prediction_class == test$return)/length(test$prediction_class)
accuracy

prp(dt,extra = 104, border.col = 0, box.palette = "auto")

#confusion matrix
library(hmeasure)
library(caret)

test$prediction_class = as.factor(test$prediction_class)
confusionMatrix(data = test$prediction_class, reference = test$return)

#roc curve
predictions.roc <- data.frame(DT = test$prediction_class)

h = HMeasure(true.class = as.numeric(test$return == "1") , scores = predictions.roc)
plotROC(h, which = 1)
h$metrics["AUC"]

#prediction for unknown data:
dt = rpart(return~item_price+item_return_prob+delivery_time+delivery_month, known1_1, method = "class") #decision tree
#test$prediction = predict(lr, newdata = unknown, type = "response")
#unknown$prediction_class = ifelse(test$prediction > 0.5, "1", "0")

pred.dt = predict(dt, newdata = unknown1_1, type = "prob")[,2]
head(pred.dt,10)
pred_unknown1_dt = ifelse(pred.dt > 0.5, "1", "0")

head(pred_unknown1_dt)

prediction_dt = data.frame(unknown1$order_item_id, pred_unknown1_dt)
colnames(prediction_dt) = c("order_item_id", "return")
head(prediction_dt, 20)

# Save predictions
write.csv(prediction_dt, file = "prediction_dt_3.csv", row.names = FALSE)



