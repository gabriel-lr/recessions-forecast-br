# Time Series Analysis
library(tidyverse)
library(xts)
library(forecast)

glimpse(my_data)

# Análise Exploratória

# Estatísticas Descritivas
  my_data |> 
  dlookr::describe() |> 
kbl(booktabs = T, caption = "Visualização Geral") |> 
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)
  
# Missing Values
naniar::gg_miss_var(my_data) + labs(y = "Look at all the missing ones")


#Falta qqplot e histograma, botar o histograma dividindo tela

swap.qq <- qplot(sample = swap, data = my_data)+
  labs(title="QQ Plot")+
  theme_classic()
swap.hist <- my_data |> 
  ggplot() +
  geom_histogram(mapping = aes(x = swap), binwidth = 0.5)+
  labs(title="Histograma")+
  theme_classic()
cowplot::plot_grid(swap.qq, swap.hist, ncol = 2, nrow = 1)

qplot(sample = ibov, data = my_data)+
  labs(title="IBOVESPA")+
  theme_classic()
qplot(sample = fipezap, data = my_data)+
  labs(title="Fipezap Index")+
  theme_classic()
qplot(sample = il, data = my_data)+
  labs(title="Índice de Liquidez BACEN")+
  theme_classic()
qplot(sample = tcb, data = my_data)+
  labs(title="Index Deterioração Macro.")+
  theme_classic()

# Normality
my_data |> 
dlookr::normality() |> 
kbl(booktabs = T, caption = "Teste Shapiro-Walk") |> 
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)

# Correlation
my_data[,-1] |> 
corrplot::corrplot(method = 'color', order = 'alphabet')


## Analise TS

# Transformar em TS
xts_data <- xts(my_data[,-1], order.by = my_data$data)

# Acf

# Checar tendência, sazonalidade e ciclicidade
decompose(ts_data[,2]) |> 
  plot()

#sazon != cicl.
plot(aggregate(my_data,FUN=mean))

# Estacionariedade
ts_data |> 
tseries::adf.test()

