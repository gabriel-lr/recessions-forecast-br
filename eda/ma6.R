library(tidyverse)

#------------ Análise Visual Médias Moveis 6 meses ----------------#

# tratando dados
df.plot <- my_data |> 
  drop_na() |> 
  transmute(data = data,
            codace = as.numeric(as.character(dummy)),
            swap = swap - SMA(swap, 6),
            ibov = ibov - SMA(ibov, 6),
            fipezap = fipezap - SMA(fipezap, 6),
            il = il - SMA(il, 6),
            tcb = tcb - SMA(tcb, 6)) |> 
  drop_na() 

## Swap

swap.plot <- df.plot |>
  dplyr::select(data, swap, codace)

p <- ggplot(swap.plot,
            aes(x = data, y = swap))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(codace == 1, max(swap),min(swap)), 
                  ymin = min(swap)),
              alpha = 0.5)+
  ggtitle("Tx. Swap DI/Pré Fixada 360 dias")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")
p

## ibov

ibov.plot <- df.plot |>
  dplyr::select(data, ibov, codace)

p <- ggplot(ibov.plot,
            aes(x = data, y = ibov))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(codace == 1, max(ibov),min(ibov)), 
                  ymin = min(ibov)),
              alpha=0.5)+
  ggtitle("Índice IBOVESPA")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")
p

#il
il.plot <- df.plot |>
  dplyr::select(data, il, codace)

p <- ggplot(il.plot,
            aes(x = data, y = il))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(codace == 1, max(il),min(il)), 
                  ymin = min(il)),
              alpha=0.5)+
  ggtitle("Índice de Liquidez")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")
p

#fipezap
fp.plot <- df.plot |>
  dplyr::select(data, fipezap, codace)

p <- ggplot(fp.plot,
            aes(x = data, y = fipezap))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(codace == 1, max(fipezap),min(fipezap)), 
                  ymin = min(fipezap)),
              alpha=0.5)+
  ggtitle("Índice Fipezap")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")
p

#tcb

tcb.plot <- df.plot |>
  dplyr::select(data, tcb, codace)

p <- ggplot(tcb.plot,
            aes(x = data, y = tcb))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(codace == 1, max(tcb),min(tcb)), 
                  ymin = min(tcb)),
              alpha=0.5)+
  ggtitle("Índice de Deterioração Macroeconômica")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")
p
