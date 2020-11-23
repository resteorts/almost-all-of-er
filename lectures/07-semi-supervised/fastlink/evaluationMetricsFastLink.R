calcPrecision <- function(posterior, threshold = 0.85) {
  TP <- sum(posterior * ifelse(posterior >= threshold, 1, 0))
  FP <- B <- sum(ifelse(posterior >= threshold, 1, 0)) - TP
  print(return(1.0*TP/(TP + FP)))
}

calcRecall <- function(posterior, nobs.a, nobs.b, threshold = 0.85) {
TP <- sum(posterior * ifelse(posterior >= threshold, 1, 0))
FP <- sum(ifelse(posterior >= threshold, 1, 0)) - TP
A.1 <- sum((1 - posterior) * ifelse(posterior < threshold, 1, 0))
TNM <- A.1 + (min(nobs.a, nobs.b) - TP - FP - A.1) * (1 - 0.001)
FN <-  (min(nobs.a, nobs.b) - TP - FP) - TNM
print(return(1.0*TP/(TP + FN)))
}

calcF1 <- function(posterior, nobs.a, nobs.b, threshold = 0.85) {
TP <- sum(posterior * ifelse(posterior >= threshold, 1, 0))
FP <- sum(ifelse(posterior >= threshold, 1, 0)) - TP
A.1 <- sum((1 - posterior) * ifelse(posterior < threshold, 1, 0))
TNM <- A.1 + (min(nobs.a, nobs.b) - TP - FP - A.1) * (1 - 0.001)
FN <-  (min(nobs.a, nobs.b) - TP - FP) - TNM
print(return((2.0*TP)/(2.0*TP+FP+FN)))
}