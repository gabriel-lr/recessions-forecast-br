# Rodando Modelo!!

library(tidyverse)
library(EWS)
library(glmnet)


# Merge nos dados

df.train <- my_data |> 
  drop_na()


attach(df.train)

# Definindo Variaveis

trainy <- as.vector(dummy)
trainx <- df.train |> 
  select(-data,-dummy) |> 
  as.matrix()

## Summary
summary(trainx)
summary(trainy)

# Modelo Estatico
estat <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                             Nb_Id = 1, Lag = 1, type_model = 1)
estat$Estimation

# Dynamic Model lag Binaria
dyn_bin <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 2)

dyn_bin$Estimation

# Dynamic Model lag index (Autoregressivo?)
dyn_ind <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 3)
dyn_ind$Estimation

# Dinamico Autoregressivo
dyn_aut <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 4)
dyn_aut$Estimation

# Plot Modelos

x1_range <- df.train$data
plot(x1_range[-1], trainy[-1], 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Resultado", main="Probabilidade")
# Add line para modelo estat
lines(x1_range[-1], estat$prob, 
      type="l", 
      col="blue")
# Add line para modelo dyn bin
lines(x1_range[-1], dyn_bin$prob, 
      type="l", 
      lwd=2,
      col="turquoise2")
# Add line modelo dyn ind
lines(x1_range[-1], dyn_ind$prob, 
      type="l", 
      lwd=2, 
      col="orangered")
# Add line modelo dyn aut
lines(x1_range[-1], dyn_aut$prob, 
      type="l", 
      lwd=2, 
      col="green")
# add a horizontal line at p=.5
abline(h=.5, lty=2)
