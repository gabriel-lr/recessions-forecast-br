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


# Modelos Estaticos Diferentes Lags

estat <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                             Nb_Id = 1, Lag = 2, type_model = 1)

estat$Estimation
estat$R2
estat$LogLik      ##PIOR QUE 1 LAG
estat$AIC
estat$BIC



# Dynamic Model lag Binaria

dyn_bin <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 2)

dyn_bin$Estimation
dyn_bin$R2
dyn_bin$LogLik        ##PIOR QUE 1 LAG
dyn_bin$AIC
dyn_bin$BIC

# Dynamic Model lag index (Autoregressivo)
dyn_ind <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 2, type_model = 3)
dyn_ind$Estimation
dyn_ind$R2
dyn_ind$LogLik           ##PIOR QUE 1 LAG
dyn_ind$AIC
dyn_ind$BIC

# Dinamico Autoregressivo
dyn_aut <- Logistic_Estimation(Dicho_Y = trainy, Exp_X = trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 2, type_model = 4)
dyn_aut$Estimation
dyn_aut$R2
dyn_aut$LogLik           ##PIOR QUE 1 LAG
dyn_aut$AIC
dyn_aut$BIC

######-------------------Testando Diferentes Lags nas variaveis ------#######



trainx.ibo <- lag(ibov, 3) |> 
  na.omit() |> 
  cbind(trainx[4:111,])
trainy.ibo <- trainy[4:111]

ibo.dyn_bin <- Logistic_Estimation(Dicho_Y = trainy.ibo, Exp_X = trainx.ibo, Intercept = TRUE,
                                   Nb_Id = 1, Lag = 1, type_model = 2)

ibo.dyn_bin$Estimation
ibo.dyn_bin$R2
ibo.dyn_bin$LogLik
ibo.dyn_bin$AIC
ibo.dyn_bin$BIC




####-------------------testar estacionarizando------------------------####


dif.trainx <- diff(trainx)
dif.trainy <- lag(trainy) |> 
  na.omit()


#Estatico
dif.estat <- Logistic_Estimation(Dicho_Y = dif.trainy, Exp_X = dif.trainx, Intercept = TRUE,
                             Nb_Id = 1, Lag = 1, type_model = 1)

dif.estat$Estimation
dif.estat$R2
dif.estat$LogLik
dif.estat$AIC
dif.estat$BIC

# Dinâmico
dif.dyn_bin <- Logistic_Estimation(Dicho_Y = dif.trainy, Exp_X = dif.trainx, Intercept = TRUE,
                              Nb_Id = 1, Lag = 1, type_model = 2)

dif.dyn_bin$Estimation
dif.dyn_bin$R2
dif.dyn_bin$LogLik
dif.dyn_bin$AIC
dif.dyn_bin$BIC

#Autoreg
dif.dyn_ind <- Logistic_Estimation(Dicho_Y = dif.trainy, Exp_X = dif.trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 3)
dif.dyn_ind$Estimation
dif.dyn_ind$R2
dif.dyn_ind$LogLik           
dif.dyn_ind$AIC
dif.dyn_ind$BIC

# Dinamico Autoregressivo
dif.dyn_aut <- Logistic_Estimation(Dicho_Y = dif.trainy, Exp_X = dif.trainx, Intercept = TRUE,
                               Nb_Id = 1, Lag = 1, type_model = 4)
dif.dyn_aut$Estimation
dif.dyn_aut$R2
dif.dyn_aut$LogLik           
dif.dyn_aut$AIC
dif.dyn_aut$BIC


# Estacionario
df.train |> 
  mutate(diff_swap = c(NA, diff(swap))) |> 
  ggplot(aes(x = data, y = diff_swap)) +
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(dummy == 1, max(diff_swap),min(diff_swap)), 
                  ymin = min(diff_swap)))+
  ggtitle("Tx. Swap DI/Pré Fixada 360 dias")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")




df.train |> 
  mutate(diff_ibov = c(NA, diff(ibov))) |> 
  ggplot(aes(x = data, y = diff_ibov)) +
  geom_line()

df.train |> 
  mutate(diff_il = c(NA, diff(il))) |> 
  ggplot(aes(x = data, y = diff_il)) +
  geom_line()

df.train |> 
  mutate(diff_fp = c(NA, diff(fipezap))) |> 
  ggplot(aes(x = data, y = diff_fp)) +
  geom_line()

  mutate(diff_tcb = c(NA, diff(tcb))) |> 
  ggplot(aes(x = data, y = diff_tcb)) +
  geom_line()
