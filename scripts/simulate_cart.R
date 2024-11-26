
library(randomForest)
library(rpart)


# Regression tree
temp <- round(rnorm(10, 5, 3), 2)
prec <- round(rnorm(10, 200, 50), 2)

generate_ab <- function(temp, prec) {
  if (temp < 5) {
    round(rnorm(1,30,3))
  } else {
    if(prec < 200) {
      round(rnorm(1,50,3)) 
    } else round(rnorm(1,70,3)) 
  } 
}

abundance <- c()
for (i in 1:length(temp)) {
  abundance[i] <- generate_ab(temp[i], prec[i])
}

res <- rpart(abundance ~ temp + prec, control = rpart.control(minsplit = 5))
res

# Classification tree
generate_occ <- function(temp, prec) {
  if (temp < 5) {
    rbinom(1, 1, 0.2)
  } else {
    if(prec < 200) {
      rbinom(1, 1, 0.6) 
    } else rbinom(1, 1, 0.9) 
  } 
}

occ <- c()
for (i in 1:length(temp)) {
  occ[i] <- generate_occ(temp[i], prec[i])
}
occ <- as.factor(occ)

res2 <- rpart(occ ~ temp + prec, control = rpart.control(minsplit = 5))
res2
