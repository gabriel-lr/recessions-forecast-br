## Cálculo TCB

# Todas as séries são ou indice ou valor abs

# Cálculo:

df.teste <- df.tcb |> 
  mutate(dem_cons = coalesce(dem_desc,dem_att)) |> # encadeando séries de demissões
  select(-dem_desc, -dem_att) |> 
  drop_na()

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


index

index.data <- cbind.data.frame(data = df.teste[-1,1], index)
rm(index)

# Padronizando para 2014 = 100

which(index.data$data == "2014-01-01")
which(index.data$data == "2014-12-01")

index_tcb <- index.data |> 
  transmute(data = data,
            tcb = (index * 100)/mean(index.data[48:59,2]))
rm(index.data)
rm(df.teste)
rm(df.tcb)

summary(index_tcb)
