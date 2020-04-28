library(randomForest)
set.seed(21)

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
trainc$female = NULL
trainc$user_yob = NULL
trainc$user_dob = NULL
#trainc$user_reg_date = NULL
trainc$order_quarter = NULL
#trainc$reg_quarter = NULL
#trainc$delivery_quarter = NULL
#trainc$east = NULL
#trainc$order_year = NULL
trainc$delivery_date = NULL

fit <- randomForest(return ~ .,
                    data=trainc, 
                    importance=TRUE, 
                    ntree=2000,
                    mtry = 3,
                    sampsize = 5000,
                    replacement = TRUE)

varImpPlot(fit)


pred.rf = data.frame("respone" = predict(fit, newdata = test, type = "response"))
head(pred.rf)
#pred.rf$prediction = sapply(pred.rf$respone, FUN = function(x){if(x < 0.5){return(0)}else{return(1)}})


test$prediction_class = pred.rf$respone

accuracy = vector() #accuracy = correct predictions/total observations
accuracy = sum(test$prediction_class == test$return)/length(test$prediction_class)
accuracy


#predict unknown data
known_small = data.frame(known1_1[, c(1, 4, 5, 6, 14, 15, 16)])

fit2 <- randomForest(return ~ .,
                    data=known_small, 
                    importance=TRUE, 
                    ntree=1000,
                    mtry = 5,
                    sampsize = 500,
                    replacement = TRUE, 
                    nodesize = 5)

varImpPlot(fit2)

pred.rf = data.frame("respone" = predict(fit2, newdata = unknown1_1, type = "response"))
head(pred.rf)
pred.rf$prediction = sapply(pred.rf$respone, FUN = function(x){if(x < 0.5){return(0)}else{return(1)}})


rf_pred = data.frame("order_item_id" = unknown$order_item_id, "return" = pred.rf$prediction)
write.csv(rf_pred, file = "prediction_rf_5.csv", row.names = FALSE)
