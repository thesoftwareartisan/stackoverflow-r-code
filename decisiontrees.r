
decisionTree <- function (data) {
  require(randomForest)
   
  #generate initial forests
  nb <- randomForest(classData ~ . , data = training)
  an <- randomForest(as.factor(isAnswered) ~ ., data = training)
}