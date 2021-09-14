# Analise Exploratória Dados.

## Plotando

df.plot <- my_data

## Indice Deterioração Macro

tcb.plot <- df.plot |>
  dplyr::select(data, tcb, dummy) |> 
  tidyr::drop_na()

tcb.plot$tcb <- log(tcb.plot$tcb)
p <- ggplot(tcb.plot,
       aes(x = data, y = tcb))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(dummy == 1, max(tcb),min(tcb)), 
                  ymin = min(tcb), 
                  alpha=0.5))+
  ggtitle("Índice de Deterioração Macroeconômica")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")
  
plotly::ggplotly(p)

## Indice de Liquidez
il.plot <- df.plot |>
  dplyr::select(data, il, dummy) |> 
  tidyr::drop_na()

il.plot$il <- log(il.plot$il)
p <- ggplot(il.plot,
            aes(x = data, y = il))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(dummy == 1, max(il),min(il)), 
                  ymin = min(il), 
                  alpha=0.5))+
  ggtitle("Índice de Liquidez")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")

plotly::ggplotly(p)


 ## FIPEZAP

fp.plot <- df.plot |>
  dplyr::select(data, fipezap, dummy) |> 
  tidyr::drop_na()


p <- ggplot(fp.plot,
            aes(x = data, y = log(fp)))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(dummy == 1, max(log(fp)),min(log(fp))), 
                  ymin = min(log(fp)), 
                  alpha=0.5))+
  ggtitle("Índice Fipezap")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")

plotly::ggplotly(p)

## Ibovespa

ibov.plot <- df.plot |>
  dplyr::select(data, ibov, dummy) |> 
  tidyr::drop_na()

p <- ggplot(ibov.plot,
            aes(x = data, y = ibov))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(dummy == 1, max(ibov),min(ibov)), 
                  ymin = min(ibov), 
                  alpha=0.5))+
  ggtitle("Índice IBOVESPA")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")

plotly::ggplotly(p)

## Swap

swap.plot <- df.plot |>
  dplyr::select(data, swap, dummy) |> 
  tidyr::drop_na()

p <- ggplot(swap.plot,
            aes(x = data, y = log(swap)))+
  geom_line(size = 0.7)+
  geom_ribbon(aes(x = data,
                  ymax = ifelse(dummy == 1, max(log(swap)),min(log(swap))), 
                  ymin = min(log(swap)), 
                  alpha=0.5))+
  ggtitle("Tx. Swap DI/Pré Fixada 360 dias")+
  ggplot2::xlab("Ano") + ggplot2::ylab("Index")

plotly::ggplotly(p)
