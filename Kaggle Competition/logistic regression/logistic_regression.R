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
trainc$reg_quarter = NULL
trainc$delivery_quarter = NULL
trainc$east = NULL
trainc$order_year = NULL
trainc$delivery_year = NULL
trainc$order_date = NULL
#trainc$delivery_time = NULL
trainc$delivery_month = NULL
trainc$item_color_freq = NULL
trainc$yob_cat = NULL
trainc$reg_month = NULL
trainc$reg_year = NULL
trainc$user_title = NULL
trainc$order_quarter = NULL


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
known1$user_dob = NULL
unknown1$user_dob = NULL
known1$user_yob = NULL
unknown1$user_yob = NULL

trainc = known1[which(complete.cases(known1)), ]
testc = unknown1[which(complete.cases(unknown1)), ]

trainc$order_item_id = NULL
#trainc$order_date = NULL

trainc$delivery_date_missing = NULL
trainc$delivery_time_negative = NULL
trainc$delivery_month = NULL
trainc$delivery_quarter = NULL
trainc$delivery_year = NULL
trainc$delivery_time = NULL

trainc$user_dob_missing = NULL
trainc$user_yob = NULL
trainc$user_dob = NULL
trainc$yob_cat = NULL

trainc$item_size = NULL
#trainc$item_size2 = NULL

trainc$item_color = NULL

#trainc$female = NULL
trainc$user_title = NULL

#trainc$user_reg_date = NULL
trainc$reg_quarter = NULL
trainc$reg_month = NULL
trainc$reg_year = NULL

trainc$east = NULL

trainc$order_year = NULL
#trainc$order_date = NULL
#trainc$order_quarter = NULL

trainc$item_color_freq = NULL
trainc$item_size_freq = NULL


#comparing sizes in trainc and unknown1
size_levels_k = data.frame("levels" = as.character(unique(trainc$item_size2)))
size_levels_u = data.frame("levels" = as.character(unique(unknown1$item_size2)))

size_levels_k$in_unknown = size_levels_k$levels %in% size_levels_u$levels
size_levels_u$in_known = size_levels_u$levels %in% size_levels_k$levels


#build regression model
lr = glm(return ~ ., data = trainc, family = binomial(link = "logit"))
summary(lr)

unknown1$prediction = predict(lr, newdata = unknown1, type = "response")
unknown1$prediction_class = sapply(unknown1$prediction, FUN = function(x) ifelse(x > 0.524, 1, 0))
unknown1$prediction_class[which(is.na(unknown1$prediction_class))] = 0

sum(as.numeric(as.character(unknown1$prediction_class)))/nrow(unknown1)

prediction_lr = data.frame(unknown$order_item_id, as.factor(unknown1$prediction_class))
colnames(prediction_lr) = c("order_item_id", "return")
head(prediction_lr, 20)

# Save predictions
write.csv(prediction_lr, file = "prediction_lr9.csv", row.names = FALSE)



