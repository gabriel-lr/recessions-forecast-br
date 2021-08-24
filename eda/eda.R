# Analise Exploratória Dados.

## Plotando

df.plot <- df |> 
  full_join(index_tcb, by = "data") |> 
  inner_join(codace, by = "data") |> 
  drop_na()

# Função Plot
attach(df.plot)

rec_plot <- function(y, titulo, cor){
  df.plot |> 
    ggplot(aes(x = data, y = y))+
    geom_line(colour = cor, size = 0.7)+
    geom_ribbon(
      aes(x = data,
          ymax = ifelse(dummy == 0, max(y),min(y)), 
          ymin = min(y), 
          alpha=0.5))+
    ylim(min(y),max(y))+
    ggtitle(titulo) +
    xlab("Ano") + ylab("Index")+
    theme(
      plot.title = element_text(color="black", size=10, face="bold"),
      axis.title.x = element_text(color="gray", size=10, face="bold"),
      axis.title.y = element_text(color="gray", size=10, face="bold"),
      legend.position = "none"
      )
    
}


## Indice Deterioração Macro
ind_det <- rec_plot(y = tcb, titulo = "Índice Deterioração Macro", cor ="blue")
ggplotly(ind_det)

## Indice de Liquidez

il_plot <- rec_plot(y = il, titulo = "Indice de Liquidez Bacen", cor = "orange")
ggplotly(il_plot)

## FIPEZAP
fipe_plot <- rec_plot(y = fipezap, cor = "red", titulo = "FIPEZAP")
ggplotly(fipe_plot)

## Ibovespa

ibov_plot <- rec_plot(y = ibov, cor = "green", titulo = "IBOVESPA")
ggplotly(ibov_plot)

## Swap

swap_plot <- rec_plot(y = swap, cor = "skyblue", titulo = "Swap DI 360")

ggplotly(swap_plot)
