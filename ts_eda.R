# Time Series Analysis
library(tidyverse)
library(xts)
library(forecast)
library(kableExtra)

glimpse(my_data)

# Análise Exploratória

# Estatísticas Descritivas
  my_data |> 
  dlookr::describe() |> 
    select(-c(10:26)) |> 
kbl(booktabs = T, caption = "Estatísticas Descritivas") |> 
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)
  
# Missing Values

plot.miss <- function(z){
  attach(my_data)
  ggplot(data = my_data,
         aes(x = data,
             y = z)) +
  naniar::geom_miss_point()
}

pmiss.il <- plot.miss(il)
pmiss.tcb <- plot.miss(tcb)
pmiss.fz <- plot.miss(fipezap)
pmiss.ib <- plot.miss(ibov)
pmiss.swap <- plot.miss(swap)
pmiss.dm <- plot.miss(dummy)

cowplot::plot_grid(pmiss.il, pmiss.tcb, pmiss.fz, pmiss.ib, pmiss.swap, pmiss.dm,
                   ncol = 3, nrow = 2)

# Correlation
my_data |>
  select(-data, -dummy) |>
  drop_na() |> 
  cor() |> 
  corrplot::corrplot(method = 'color', order = 'alphabet')

## Analise TS

# Transformar em TS
xts_data <- xts(my_data[,-1], order.by = my_data$data)

# Acf

# Checar tendência, sazonalidade e ciclicidade
decompose(xts_data[,2]) |> 
  plot()

#sazon != cicl.
plot(aggregate(my_data,FUN=mean))

# Estacionariedade
xts_data |> 
  tseries::adf.test()

## Caso não sejam estacionárias plotar em log abaixo

#Falta qqplot e histograma, botar o histograma dividindo tela
#swap
swap.qq <- qplot(sample = swap, data = my_data)+
  labs(title="QQ Plot")+
  theme_classic()
swap.hist <- my_data |> 
  ggplot() +
  geom_histogram(mapping = aes(x = swap), binwidth = 0.5)+
  labs(title="Histograma")+
  theme_classic()
cowplot::plot_grid(swap.qq, swap.hist, ncol = 2, nrow = 1)
#ibov
ibov.qq <- qplot(sample = ibov, data = my_data)+
  labs(title="IBOVESPA")+
  theme_classic()
ibov.hist <- my_data |> 
  ggplot() +
  geom_histogram(mapping = aes(x = ibov), binwidth = 0.5)+
  labs(title="Histograma")+
  theme_classic()
cowplot::plot_grid(ibov.qq, ibov.hist, ncol = 2, nrow = 1)
#fipezap
fp.qq <- qplot(sample = fipezap, data = my_data)+
  labs(title="Fipezap Index")+
  theme_classic()
fp.hist <- my_data |> 
  ggplot() +
  geom_histogram(mapping = aes(x = fipezap), binwidth = 0.5)+
  labs(title="Histograma")+
  theme_classic()
cowplot::plot_grid(fp.qq, fp.hist, ncol = 2, nrow = 1)
#il
il.qq <- qplot(sample = il, data = my_data)+
  labs(title="Índice de Liquidez BACEN")+
  theme_classic()
il.hist <- my_data |> 
  ggplot() +
  geom_histogram(mapping = aes(x = il), binwidth = 0.5)+
  labs(title="Histograma")+
  theme_classic()
cowplot::plot_grid(il.qq, il.hist, ncol = 2, nrow = 1)
#tcb
tcb.qq <- qplot(sample = tcb, data = my_data)+
  labs(title="Index Deterioração Macro.")+
  theme_classic()
tcb.hist <- my_data |> 
  ggplot() +
  geom_histogram(mapping = aes(x = tcb), binwidth = 0.5)+
  labs(title="Histograma")+
  theme_classic()
cowplot::plot_grid(tcb.qq, tcb.hist, ncol = 2, nrow = 1)

# Normality
my_data |> 
dlookr::normality() |> 
kbl(booktabs = T, caption = "Teste Shapiro-Walk") |> 
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)
## teste adf
st <- as.numeric(min(my_data$data))
fin <- as.numeric(max(my_data$data))
values <- my_data[,-1]

ts.data <- ts(values, start = st, end = fin, frequency = 12)
ts.data
ts.data[,2] |>
tseries::adf.test()

## trablhando por série


## swap
xts.swap <- my_data |> 
   select(swap) |> 
        xts(order.by = my_data$data)

xts.swap <- xts.swap[complete.cases(xts.swap[,1]),]

autoplot(acf(xts.swap, plot = FALSE))
autoplot(pacf(xts.swap, plot = FALSE))
tseries::adf.test(xts.swap, alternative="stationary", k=0)
autoplot(xts.swap)
autoplot(diff(xts.swap))

## il 
xts.il <- my_data |> 
  select(il) |> 
  xts(order.by = my_data$data)

xts.il <- xts.il[complete.cases(xts.il[,1]),]

autoplot(acf(xts.il, plot = FALSE))
autoplot(pacf(xts.il, plot = FALSE))
tseries::adf.test(xts.il, alternative="stationary", k=0)
autoplot(xts.il)
autoplot(diff(xts.il))

## fipezap

xts.fipezap <- my_data |> 
  select(fipezap) |> 
  xts(order.by = my_data$data)

xts.fipezap <- xts.fipezap[complete.cases(xts.fipezap[,1]),]

autoplot(acf(xts.fipezap, plot = FALSE))
autoplot(pacf(xts.fipezap, plot = FALSE))
tseries::adf.test(xts.fipezap, alternative="stationary", k=0)
autoplot(xts.fipezap)
autoplot(diff(xts.fipezap, lag = 2))

## tcb

xts.tcb <- my_data |> 
  select(tcb) |> 
  xts(order.by = my_data$data)

xts.tcb <- xts.tcb[complete.cases(xts.tcb[,1]),]

autoplot(acf(xts.tcb, plot = FALSE))
autoplot(pacf(xts.tcb, plot = FALSE))
tseries::adf.test(xts.tcb, alternative="stationary", k=0)
autoplot(xts.tcb)
autoplot(diff(xts.tcb))


## ibov com funcao

ts.eda <- function(x){
  xts.x <- my_data |> 
    select(x) |> 
    xts(order.by = my_data$data)
  
  xts.x <- xts.x[complete.cases(xts.x[,1]),]
  
  autoplot(acf(xts.x, plot = FALSE))
  autoplot(pacf(xts.x, plot = FALSE))
  tseries::adf.test(xts.x, alternative="stationary", k=0)
  autoplot(xts.x)
  autoplot(diff(xts.x))
}


ts.eda("ibov")



ndiffs(xts.swap)
nsdiffs(xts.swap)

ndiffs(xts.tcb)
nsdiffs(xts.tcb)

ndiffs(xts.il)
nsdiffs(xts.il)

ndiffs(xts.fipezap)
nsdiffs(xts.fipezap)
