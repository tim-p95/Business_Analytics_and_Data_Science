library(rpart)
library(rpart.plot)

dt = rpart(return ~ user_title + user_state, train, method = "class") #decision tree
test$prediction = predict(dt, newdata = test, type = "prob")[,2]

rpart.plot(dt)

#accuracy = correct predictions/total observations
accuracy = sum(as.numeric(test$prediction) == as.numeric(test$return))/length(test$prediction)
accuracy

str(test)

dt = rpart(return~., train, method = "class") #decision tree
pred.dt = predict(dt, newdata = test, type = "prob")[,2]
head(pred.dt,10)

levels(loans$BAD)

Accuracy2 = function(prediction, class, threshold = 0.5){
  predClass = ifelse(prediction > threshold, levels(class)[2],levels(class)[1])
  acc = sum(predClass == class)/length(class)
  return(acc)
}

acc.dt = Accuracy2(pred.dt, loans$BAD, 0.5)
prp(dt)
prp(dt,extra = 104, border.col = 0, box.palette = "auto")

