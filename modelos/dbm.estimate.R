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
predict(estat, n.ahead = 2)
estat$fit$fitted.values

# dinamico
dyn_bin <- dbm(teste.xts, x.vars = c("swap", "tcb","il"),
               arp = 0, arq = 1)
dyn_bin
predict(dyn_bin, n.ahead = 2)

# autoregressivo
aut <- dbm(teste.xts, x.vars = c("swap", "tcb", "il"),
               arp = 1, arq = 0)
aut
predict(aut, n.ahead = 2)

## Dinâmico Autoregressivo
dyn_aut <- dbm(teste.xts, x.vars = c("swap", "tcb", "il"),
               arp = 1, arq = 1)
dyn_aut
predict(dyn_aut, n.ahead = 2)

#----------------------- Plots -----------------------------------------#

# Plot Modelos

x1_range <- teste$data
plot(x1_range, teste.xts$codace, 
     ylim=c(0,1),
     pch = "+", 
     xlab="Data", ylab="Resultado", main="Probabilidade")
# Add line para modelo estat
lines(x1_range, estat$fit$fitted.values, 
      type="l", 
      col="blue")
# Add line para modelo dyn bin
lines(x1_range, dyn_bin$fit$fitted.values, 
      type="l", 
      lwd=2,
      col="turquoise2")
# Add line modelo dyn ind
lines(x1_range, aut$fit$fitted.values, 
      type="l", 
      lwd=2, 
      col="orangered")
# Add line modelo dyn aut
lines(x1_range, dyn_aut$fit$fitted.values, 
      type="l", 
      lwd=2, 
      col="green")
# add a horizontal line at p=.5
abline(h=.5, lty=2)


