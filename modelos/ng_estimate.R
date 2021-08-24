# Rodando Modelo!!

library(EWS)
library(glmnet)


# Merge nos dados

df.train <- df |> 
  inner_join(index_tcb, by = "data") |> 
  inner_join(codace, by = "data") |> 
  drop_na()


attach(df.train)

# Definindo Variaveis

trainy <- as.vector(dummy)
trainx <- as.matrix(swap, ibov, fipezap, il, tcb)

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
