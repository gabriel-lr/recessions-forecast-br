# Estatico Model One Variable Differente lags periods

# Avalição Modelos

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


#Swap: 1
lags.swap <- function(y){
swap.est <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx[,1], Intercept = TRUE,
                             Nb_Id = 1, Lag = y, type_model = 1)
swap.est$R2
}
Tx_Swap <- lapply(seq(1:18), lags.swap)|> 
  unlist()
#ibov
lags.ibov <- function(y){
  ibov.est <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx[,2], Intercept = TRUE,
                                  Nb_Id = 1, Lag = y, type_model = 1)
  ibov.est$R2
}
Ibovespa <- lapply(seq(1:18), lags.ibov)|> 
  unlist()
#Fipezap
lags.fp <- function(y){
  ibov.fp <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx[,3], Intercept = TRUE,
                                  Nb_Id = 1, Lag = y, type_model = 1)
  ibov.fp$R2
}
Fipezap <- lapply(seq(1:18), lags.fp)|> 
  unlist()
#il
lags.il <- function(y){
  il.est <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx[,4], Intercept = TRUE,
                                  Nb_Id = 1, Lag = y, type_model = 1)
  il.est$R2
}
Ind_Liquidez <- lapply(seq(1:18), lags.il)|> 
  unlist()
#tcb
lags.tcb <- function(y){
  tcb.est <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx[,5], Intercept = TRUE,
                                Nb_Id = 1, Lag = y, type_model = 1)
  tcb.est$R2
}
Ind_Deterioração <- lapply(seq(1:18), lags.tcb) |> 
  unlist()

r2.tbl <- tibble(Tx_Swap, Ibovespa, Fipezap, Ind_Liquidez, Ind_Deterioração)
r2.tbl.1 <- as_tibble(cbind('Váriaveis Explicativas' = names(r2.tbl), t(r2.tbl)))


gt::gt(r2.tbl.1)

