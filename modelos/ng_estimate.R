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
estat$R2
estat$LogLik
estat$AIC
estat$BIC
# Dynamic Model lag Binaria
dyn_bin <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 2)

dyn_bin$Estimation
dyn_bin$R2
dyn_bin$LogLik
dyn_bin$AIC
dyn_bin$BIC
# Dynamic Model lag index (Autoregressivo?)
dyn_ind <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 3)
dyn_ind$Estimation
dyn_ind$R2
dyn_ind$LogLik
dyn_ind$AIC
dyn_ind$BIC
# Dinamico Autoregressivo
dyn_aut <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 4)
dyn_aut$Estimation
dyn_aut$R2
dyn_aut$LogLik
dyn_aut$AIC
dyn_aut$BIC
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

## Previsão

a <- Simul_GIRF(Dicho_Y = trainy,
           Exp_X = trainx,
           t_mod = 1,
           Int = TRUE,
           n_simul = 10,
           horizon = 3,
           Lag = 1,
           centile_shock = 0.95,
           OC = "AM") 
  
a

##------------Testando só com swap ou só com tcb-----------#

swap.train <- df.train |> 
  select(swap) |> 
  as.matrix()

# Modelo Estatico
estat.swap <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = swap.train, Intercept = TRUE,
                             Nb_Id = 1, Lag = 1, type_model = 1)
estat$Estimation
estat$R2

# Dynamic Model lag Binaria
dyn_bin.swap <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = swap.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 2)

dyn_bin.swap$Estimation
dyn_bin.swap$R2
dyn_bin.swap$LogLik
dyn_bin.swap$AIC
dyn_bin.swap$BIC
# Dynamic Model lag index (Autoregressivo?)
dyn_ind.swap <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = swap.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 3)
dyn_ind$Estimation
dyn_ind$R2
# Dinamico Autoregressivo
dyn_aut.swap <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = swap.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 4)
dyn_aut.swap$Estimation
dyn_aut.swap$R2
dyn_aut.swap$LogLik
dyn_aut.swap$AIC
dyn_aut.swap$BIC
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


#####--------------tcb--------------###

tcb.train <- df.train |> 
  select(tcb) |> 
  as.matrix()

# Modelo Estatico
estat.tcb <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = tcb.train, Intercept = TRUE,
                             Nb_Id = 1, Lag = 1, type_model = 1)
estat.tcb$Estimation
estat.tcb$R2

# Dynamic Model lag Binaria
dyn_bin.tcb <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = tcb.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 2)

dyn_bin.tcb$Estimation
dyn_bin.tcb$R2

# Dynamic Model lag index (Autoregressivo?)
dyn_ind.tcb <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = tcb.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 3)
dyn_ind.tcb$Estimation
dyn_ind.tcb$R2
# Dinamico Autoregressivo
dyn_aut.tcb <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = tcb.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 4)
dyn_aut.tcb$Estimation
dyn_aut.tcb$R2
# Plot Modelos

x1_range <- df.train$data
plot(x1_range[-1], trainy[-1], 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Resultado", main="Probabilidade")
# Add line para modelo estat
lines(x1_range[-1], estat.tcb$prob, 
      type="l", 
      col="blue")
# Add line para modelo dyn bin
lines(x1_range[-1], dyn_bin.tcb$prob, 
      type="l", 
      lwd=2,
      col="turquoise2")
# Add line modelo dyn ind
lines(x1_range[-1], dyn_ind.tcb$prob, 
      type="l", 
      lwd=2, 
      col="orangered")
# Add line modelo dyn aut
lines(x1_range[-1], dyn_aut.tcb$prob, 
      type="l", 
      lwd=2, 
      col="green")
# add a horizontal line at p=.5
abline(h=.5, lty=2)

#### sem fipezap

nfp.train <- df.train |> 
  select(-fipezap, -data, -dummy) |> 
  as.matrix()

# Modelo Estatico
estatnfp <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = nfp.train, Intercept = TRUE,
                             Nb_Id = 1, Lag = 1, type_model = 1)
estatnfp$Estimation
estatnfp$R2


# Dynamic Model lag Binaria
dyn_binnfp <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = nfp.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 2)

dyn_binnfp$Estimation
dyn_binnfp$R2
dyn_binnfp$LogLik
dyn_binnfp$BIC
dyn_binnfp$AIC

# Dynamic Model lag index (Autoregressivo?)
dyn_indnfp <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = nfp.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 3)
dyn_indnfp$Estimation
dyn_indnfp$R2
# Dinamico Autoregressivo
dyn_autnfp <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = nfp.train, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 4)
dyn_autnfp$Estimation
dyn_autnfp$R2
dyn_autnfp$LogLik
dyn_autnfp$AIC
dyn_autnfp$BIC
# Plot Modelos

x1_range <- df.train$data
plot(x1_range[-1], trainy[-1], 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Resultado", main="Probabilidade")
# Add line para modelo estat
lines(x1_range[-1], estatnfp$prob, 
      type="l", 
      col="blue")
# Add line para modelo dyn bin
lines(x1_range[-1], dyn_binnfp$prob, 
      type="l", 
      lwd=2,
      col="turquoise2")
# Add line modelo dyn ind
lines(x1_range[-1], dyn_indnfp$prob, 
      type="l", 
      lwd=2, 
      col="orangered")
# Add line modelo dyn aut
lines(x1_range[-1], dyn_autnfp$prob, 
      type="l", 
      lwd=2, 
      col="green")
# add a horizontal line at p=.5
abline(h=.5, lty=2)

## Conclusão modelo dinâmico geral supera todos!

## Previsão

predict(df.train, dyn_bin)

