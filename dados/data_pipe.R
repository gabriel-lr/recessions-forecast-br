## API Request

library(dplyr)
library(purrr)

##------Solicitando os dados via api-----------#

# Funcao

ipea.api <- function(cod, ap = ap){
  res <- httr::GET(paste("http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='",cod,"')", sep =""))
  df <- jsonlite::fromJSON(rawToChar(res$content))
  
  ap <- df[[2]] |> 
    dplyr::select(VALDATA, VALVALOR)
  return(ap)
}

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
df$data <- stringr::str_remove(df$data, pattern = "T.*$") |> 
  as.Date(format = "%Y-%m-%d")

# Buscando Series no BCB SGS

bcb.api <- function(cod){
  res <- httr::GET(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.", cod, "/dados?formato=json", sep = ""))
  df <- jsonlite::fromJSON(rawToChar(res$content))
}


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
  dplyr::select(-valor) |> 
  arrange(data)


##--------------------- DADOS TCB -----------------------##


# Códigos

tcb.cod <- c("CNI12_HTRABD12", "ABPO12_PAPEL12", "PIMPFN12_QIIGSNAS12", 
             "CNI12_ICEIGER12", "GAC12_INDFBCFCCDESSAZ12", "BM12_M1MN12", 
             "FCESP12_IICF12", "CAGED12_DESLIG", "CAGED12_DESLIGN12")

nomes.tcb <- c("trab_ind", "exp_papel", "prod_ind", "icei", "fbcf_cc", "m1", 
               "iec", "dem_desc", "dem_att")
# Iterando
df.tcb <- lapply(tcb.cod, ipea.api)

# Renomeando listas

for(i in 1:length(nomes.tcb)){
  colnames(df.tcb[[i]])[2] <- nomes.tcb[i]
}

# Unlist Joint

df.tcb <- df.tcb |> 
  reduce(dplyr::full_join, by = "VALDATA") |>
  rename(data = VALDATA) |> 
  arrange(data)

# Organizando datas

df.tcb$data <- stringr::str_remove(df.tcb$data, pattern = "T.*$") |> 
  as.Date(format = "%Y-%m-%d")

##---------------Cálculo TCB-----------------##

# Todas as séries são ou indice ou valor abs

# Cálculo:

df.teste <- df.tcb |> 
  mutate(dem_cons = dplyr::coalesce(dem_desc,dem_att)) |> # encadeando séries de demissões
  dplyr::select(-dem_desc, -dem_att) |> 
  tidyr::drop_na()

# Cáculo Média Simétrica da variação mensal de cada série
mensal <- 200 * ((df.teste[,-1] - lag(df.teste[,-1])) / (df.teste[,-1]+lag(df.teste[,-1])))
mensal <- mensal[-1,]

# Cáluclo da Média de Variação ajustada pelo desvio padrão
c <- matrix(nrow = 1, ncol = ncol(mensal))
for(i in 1:ncol(mensal)){
  c[,i] <- sd(mensal[,i])
}

k <- sum(c)
j <- k * c
rm(c)
rm(k)

# Calculo série diferenças ajustada
m <- matrix(nrow = nrow(mensal), ncol = ncol(mensal))
for(i in 1: ncol(mensal)){
  m[,i] <- mensal[,i] / j[i]
}
rm(j)
# Calculo soma das diferenças mensais ajustadas
f <- matrix(nrow = nrow(m), ncol = 1)
for(i in 1:nrow(m)){
  f[i,] <- sum(m[i,])
}
rm(m)
## Calculo do Indice:
index <- numeric(nrow(f))
for(i in 1:nrow(f)){
  if(i == 1){
    index[i] <- (200 + f[i,]) / (200 - f[i,])
  }else{
    index[i] <- ((200 + f[i,]) / (200 - f[i,])) * index[i-1]
  }
}
rm(f)
rm(mensal)

index.data <- cbind.data.frame(data = df.teste[-1,1], index)
rm(index)

# Padronizando para 2014 = 100

index_tcb <- index.data |> 
 dplyr::transmute(data = data,
            tcb = (index * 100) / mean(index.data[
              which(index.data$data == "2014-01-01"):which(index.data$data == "2014-12-01"),2
              ]))


rm(index.data)
rm(df.teste)
rm(df.tcb)

# Gerando dados Recessao

# Coleta e tratamento de dados
pib <- sidrar::get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202") |> 
  dplyr::select("date" = `Trimestre (Código)`, "value" = `Valor`)  |> 
  mutate(value = value, date = lubridate::yq(date)) |> 
  as_tibble()


# Obter datacao de ciclo de negocios
bc_dates <- pib |> 
  timetk::tk_ts(select = value, start = c(1996, 1), frequency = 4) |> 
  BCDating::BBQ(name = "Ciclo de Negócios do PIB do Brasil")

# ------- Gerar codigo para estabelecer os valores mensais --------- ###

# Atribuindo Tres meses para cada trimestre
codace <- TSstudio::ts_reshape(bc_dates@states, type = "long") |> 
  mutate(ntimes = 3)

leng.hp <- max(pib$date) |> 
  lubridate::interval(as.Date("1996/01/01")) |> 
  lubridate::as.period(unit = "months")

if(lubridate::month(max(pib$date)) != 12){
codace <- as.data.frame(lapply(codace, rep, codace$ntimes)) |> 
  dplyr::select(-quarter, -ntimes) |> 
  dplyr::group_by(year) |> 
  dplyr::transmute(ano = year,
            mes = ifelse(year < lubridate::year(max(pib$date)), seq(1:12), seq(1:(lubridate::month(max(pib$date))))),
            dummy = gsub("1", "0", value)) |>
  dplyr::ungroup() |> 
  dplyr::select(-year) |> 
  dplyr::transmute(data = seq(as.Date("1996/01/01"), by = "month", length.out = (3+abs(lubridate::month(leng.hp)))),
            dummy = gsub("-0", "1", dummy))
}else{
  codace <- as.data.frame(lapply(codace, rep, codace$ntimes)) |> 
    dplyr::select(-quarter, -ntimes) |> 
    dplyr::group_by(year) |> 
    dplyr::transmute(ano = year,
                     mes = seq(1:12),
                     dummy = gsub("1", "0", value)) |>
    dplyr::ungroup() |> 
    dplyr::select(-year) |> 
    dplyr::transmute(data = seq(as.Date("1996/01/01"), by = "month", length.out = lubridate::month(leng.hp)),
                     dummy = gsub("-0", "1", dummy)) 
}

codace$dummy <- as.factor(codace$dummy)

rm(pib)

data.final <- df |> 
  full_join(index_tcb, by = "data") |> 
  full_join(codace, by = "data") |> 
  arrange(data)

saveRDS(data.final, file = "my_data.rds")
