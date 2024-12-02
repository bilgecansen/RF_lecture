
library(randomForest)
library(caret)
library(pdp)
library(permimp)


# Nesting habitat of Egyptian Vultures ------------------------------------

# data
data_ev <- read.csv("data/data_ev.csv")
data_ev$Presence <- as.factor(data_ev$Presence)

# Initial forest
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

# Partial dependence plots
par_el <- partial(res_rf2, pred.var = c("Elevation"), 
                  rug = T, progress = "text", prob = T)

theme_set(theme_bw())
ggplot() +
  geom_line(data = par_el, 
            aes(x = Elevation, y = yhat),
            color = "#95d6dc", linewidth = 2) +
  labs(x = "Elevation (m))", 
       y =  "Nesting Probability") +
  theme(panel.border = element_blank(),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0.35, 0.7))

par_nnd <- partial(res_rf2, pred.var = c("NND"), 
                  rug = T, progress = "text", prob = T)

theme_set(theme_bw())
ggplot() +
  geom_line(data = par_nnd, 
            aes(x = NND, y = yhat),
            color = "#95d6dc", linewidth = 2) +
  labs(x = "NND (m))", 
       y =  "Nesting Probability") +
  theme(panel.border = element_blank(),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank())


# Emperor penguins --------------------------------------------------------

# data
data_em <- readRDS("data/data_em.rds")
data_em
data_em <- as.data.frame(data_em[,-1])
data_em$N

tuneRF(data_em[,-33], y = data_em[,33])
res_rf_em <- randomForest(N ~., 
                          data = data_em, 
                          ntree = 8000, 
                          mtry = 20,
                          importance = T,
                          keep.inbag = T,
                          keep.forest = T)

rfStats(res_rf_em)

varimp_rf <- permimp(res_rf_em, conditional = F, progressBar = T)
varimp_rf <- varimp_rf$values[order(varimp_rf$values, decreasing = T)]

ggplot(mapping = aes(x = factor(names(varimp_rf), 
                                levels = names(varimp_rf)),
                     y = varimp_rf)) +
  geom_col(fill = "darkblue") +
  labs(y = "Variable Importance") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10))

par_ice <- partial(res_rf_em, pred.var = c("aice_nonbreed"), 
                   rug = T, progress = "text")
ggplot() +
  geom_line(data = par_ice, 
            aes(x = aice_nonbreed, y = yhat),
            color = "#95d6dc", linewidth = 2) +
  labs(x = "Sea-Ice Concentration", 
       y =  "Average Colony Abundance") +
  theme(panel.border = element_blank(),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank())

par_fice <- partial(res_rf_em, pred.var = c("fdice_rearing"), 
                   rug = T, progress = "text")
ggplot() +
  geom_line(data = par_fice, 
            aes(x = fdice_rearing, y = yhat),
            color = "#95d6dc", linewidth = 2) +
  labs(x = "Distance to nearest fast-ice edge", 
       y =  "Average Colony Abundance") +
  theme(panel.border = element_blank(),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank())

