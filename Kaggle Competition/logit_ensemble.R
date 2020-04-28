#train models with different training and feature sets

logit = list(1, 2, 3)
k = 1

for(i in 1:3) { 
  for(j in 1:3){
    train_set = train[[i]][features[[j]]]
    logit[k] = assign(paste("logit", i, ".", j, sep = ""), glm(return ~ ., data = train_set, family = binomial(link = "logit")))
    k = k + 1
  }
}


for(i in 1:3) { 
    train_set = train[[1]][features[[i]]]
    logit[k] = assign(paste("logit", i, ".", j, sep = ""), glm(return ~ ., data = train_set, family = binomial(link = "logit")))
    k = k + 1
}




