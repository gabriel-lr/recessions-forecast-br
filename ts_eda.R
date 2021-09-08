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
ts_data |> 
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






