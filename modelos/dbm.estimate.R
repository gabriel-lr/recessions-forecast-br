library(dbm)
library(tidyverse)
library(xts)
library(TTR)
data(recession)
dbm(recession, x.vars = c("T10Y", "TB3M"))
glimpse(recession)

#----------------- Tratamento dados ----------------------#

teste <- my_data |> 
  drop_na() |> 
  transmute(data = as.POSIXlt(data, format = "%Y-%m-%d", tz = "GMT"),
            codace = as.numeric(as.character(dummy)),
            swap = swap - SMA(swap, 6),
            ibov = ibov - SMA(ibov, 6),
            fipezap = fipezap - SMA(fipezap, 6),
            il = il - SMA(il, 6),
            tcb = tcb - SMA(tcb, 6)) |> 
  drop_na() 
  
teste.xts <- xts(teste[,-1],
             order.by = teste$data)

#----------------------estimation------------------------------#


# estatico
estat <- dbm(teste.xts, x.vars = c("swap", "tcb", "il"),
               arp = 0, arq = 0)
estat
predict(estat, n.ahead = 4)
estat$fit$fitted.values

# dinamico
dyn_bin <- dbm(teste.xts, x.vars = c("swap", "tcb","il"),
               arp = 0, arq = 1)
dyn_bin
predict(dyn_bin, n.ahead = 4)

# autoregressivo
aut <- dbm(teste.xts, x.vars = c("swap", "tcb", "il"),
               arp = 1, arq = 0)
aut
predict(aut, n.ahead = 4)

## Dinâmico Autoregressivo
dyn_aut <- dbm(teste.xts, x.vars = c("swap", "tcb", "il"),
               arp = 1, arq = 1)
dyn_aut
predict(dyn_aut, n.ahead = 4)

#----------------------- Plots -----------------------------------------#

# Estatico

x1_range <- teste$data
plot(x1_range, teste.xts$codace, 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Probabilidade", main="Probit Estático")
# Add line para modelo estat
lines(x1_range, estat$fit$fitted.values, 
      type="l", 
      col="blue")



#Dinamico
x1_range <- teste$data
plot(x1_range, teste.xts$codace, 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Probabilidade", main="Probit Dinâmico")
lines(x1_range, dyn_bin$fit$fitted.values, 
      type="l",
      col="blue")


# Autoregressivo
x1_range <- teste$data
plot(x1_range, teste.xts$codace, 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Probabilidade", main="Probit Autoregressivo")
lines(x1_range, aut$fit$fitted.values, 
      type="l", 
      col="blue")

# Autoregressivo Dinâmico
x1_range <- teste$data
plot(x1_range, teste.xts$codace, 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Probabilidade", main="Probit Autoregressivo Dinâmico")
lines(x1_range, dyn_aut$fit$fitted.values, 
      type="l", 
      col="blue")

#--------------- Definindo lag ideal-------------------------#

#swap
lag.swap <- dbm(teste.xts, x.vars = c("swap"),
             arp = 0, arq = 0, x.lags = 12)
lag.swap

#tcb
lag.tcb <- dbm(teste.xts, x.vars = c("tcb"),
                arp = 0, arq = 0, x.lags = 1)
lag.tcb

#il
lag.il <- dbm(teste.xts, x.vars = c("il"),
               arp = 0, arq = 0, x.lags = 5)
lag.il

