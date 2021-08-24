## API Request

library(httr)
library(jsonlite)
library(tidyverse)


##------Solicitando os dados via api-----------#

# Fun??o
## Posteriormente trabalhar a efici?ncia do c?digo, est? mto pesado.

ipea.api <- function(cod, ap = ap){
  require(httr)
  require(jsonlite)
  require(tidyverse)
  res <- GET(paste("http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='",cod,"')", sep =""))
  df <- fromJSON(rawToChar(res$content))
  
  ap <- df[[2]] |> 
    select(VALDATA, VALVALOR)
  return(ap)
}

# C?digos API IPEA
## SWAP DI 360 m?dia: BMF12_SWAPDI360F12
## IBOVESPA FECHAMENTO % MENSAL: ANBIMA12_IBVSP12
## FIPEZAP VPRE?O VENDA BRASIL: FIPE12_VENBR12

codigos <- c("BMF12_SWAPDI360F12", "ANBIMA12_IBVSP12", "FIPE12_VENBR12" )

# Query

df <- lapply(codigos, ipea.api)

# Organizando dados
df <- df |> 
  reduce(full_join, by = "VALDATA")
# Renomeando cols
df <- df |> 
  rename(fipezap = VALVALOR,
         swap = VALVALOR.x,
         ibov = VALVALOR.y,
         data = VALDATA)
# Transformando em data
df$data <- str_remove(df$data, pattern = "T.*$") |> 
  as.Date(format = "%Y-%m-%d")

# Buscando Series no BCB SGS

bcb.api <- function(cod){
  require(httr)
  require(jsonlite)
  require(tidyverse)
  res <- GET(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.", cod, "/dados?formato=json", sep = ""))
  df <- fromJSON(rawToChar(res$content))
}

## IL SGS: 28448

il <- bcb.api(28448)

# Organizando dados

il$data <- as.Date(il$data, format = "%d/%m/%Y")


# Dando merge

df <- df |>
  full_join(il, by = "data")
rm(il)

# Transformando ile em numerico

df <- df |> 
  mutate(il = as.numeric(valor)) |> 
  select(-valor) |> 
  arrange(data)


### Pipe de importa??o e tratamento dos Dados conclu?da com sucesso!!!!!


##--------------------- DADOS TCB -----------------------##


# Códigos

#IPEA API

## Horas Trabalhadas Industria Dessaz: CNI12_HTRABD12
## Exped Papel Ondulado QTD: ABPO12_PAPEL12
## Prod Industria Geral Ind Base fix dessaz: PIMPFN12_QIIGSNAS12
## Index conf. empresario industrial geral: CNI12_ICEIGER12
## Indicador IPEA FBCF Const Civil Dessaz: GAC12_INDFBCFCCDESSAZ12
## M1 Média Período: BM12_M1MN12
## Ind. Expec Consumidor IEC: FCESP12_IICF12

## Total Demissões
### até 2019.12: CAGED12_DESLIG
### apartir 2020.01: CAGED12_DESLIGN12

tcb <- c("CNI12_HTRABD12", "ABPO12_PAPEL12", "PIMPFN12_QIIGSNAS12", 
         "CNI12_ICEIGER12", "GAC12_INDFBCFCCDESSAZ12", "BM12_M1MN12", 
         "FCESP12_IICF12", "CAGED12_DESLIG", "CAGED12_DESLIGN12")

nomes.tcb <- c("trab_ind", "exp_papel", "prod_ind", "icei", "fbcf_cc", "m1", 
               "iec", "dem_desc", "dem_att")
# Iterando
df.tcb <- lapply(tcb, ipea.api)

# Renomeando listas

for(i in 1:length(nomes.tcb)){
colnames(df.tcb[[i]])[2] <- nomes.tcb[i]
}

# Unlist Joint

df.tcb <- df.tcb |> 
  reduce(full_join, by = "VALDATA") |>
  rename(data = VALDATA) |> 
  arrange(data)

# Organizando datas

df.tcb$data <- str_remove(df.tcb$data, pattern = "T.*$") |> 
  as.Date(format = "%Y-%m-%d")
