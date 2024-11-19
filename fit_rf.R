
library(randomForest)
library(caret)

# data
data_ev <- read.csv("data_ev.csv")
data_ev$Presence <- as.factor(data_ev$Presence)

# Inital forest
tuneRF(data_ev[,-1], y = data_ev[,1])
res_rf <- randomForest(Presence~., 
                       data = data_ev, 
                       ntree = 2000, 
                       mtry = 8,
                       importance = T)
rfStats(res_rf)
imp <- importance(res_rf)
imp

# Forest with selected variables
idx <- which(imp[,3] > 0) + 1

data_ev_sel <- data_ev[,c(1,idx)]
data_ev_sel

tuneRF(data_ev_sel[,-1], y = data_ev_sel[,1])
res_rf2 <- randomForest(Presence~., 
                        data = data_ev_sel, 
                        ntree = 8000, 
                        mtry = 2,
                        importance = T)
rfStats(res_rf2)
importance(res_rf2)


