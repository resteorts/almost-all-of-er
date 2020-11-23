calcPrecision <- function(predictions, labels) {
  predictions <- as.logical(predictions)
  labels <- as.logical(labels)
  TP <- sum(predictions & labels)       
  PP <- sum(predictions)
  return(1.0*TP/(PP))
}

calcRecall <- function(predictions, labels) {
  predictions <- as.logical(predictions)
  labels <- as.logical(labels)
  TP <- sum(predictions & labels) 
  P <- sum(labels)
  return(1.0*TP/P)
}

calcF1 <- function(predictions, labels) {
  predictions <- as.logical(predictions)
  labels <- as.logical(labels)
  TP <- sum(predictions & labels)       
  FP <- sum(predictions & !labels)
  FN <- sum(!predictions & labels)
  return((2.0*TP)/(2.0*TP+FP+FN))
}