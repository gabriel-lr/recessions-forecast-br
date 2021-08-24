# Gerando dados CODACE
## Script do Blog Analise Macro

# Instalar/carregar pacotes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  "magrittr",
  "BCDating",
  "sidrar",
  "dplyr",
  "lubridate",
  "timetk",
  "zoo",
  "ggplot2",
  "ggthemes"
)

# Coleta e tratamento de dados
pib <- sidrar::get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202") |> 
  dplyr::select("date" = `Trimestre (Código)`, "value" = `Valor`)  |> 
  dplyr::mutate(value = value, date = lubridate::yq(date)) |> 
  dplyr::as_tibble()


# Obter datação de ciclo de negócios
bc_dates <- pib %>%
  timetk::tk_ts(select = value, start = c(1996, 1), frequency = 4) |> 
  BCDating::BBQ(name = "Ciclo de Negócios do PIB do Brasil")

# Exibir resultados
show(bc_dates)

# ------- Gerar codigo para estabelecer os valores mensais --------- ###


codace <- TSstudio::ts_reshape(bc_dates@states, type = "long") |> 
  mutate(ntimes = 3)

codace <- as.data.frame(lapply(codace, rep, codace$ntimes)) |> 
  select(-quarter, -ntimes) |> 
  group_by(year) |> 
   transmute(ano = year,
             mes = ifelse(year < 2021, seq(1:12), seq(1:3)),
             dummy = gsub("-1", "0", value)) |>
  ungroup() |> 
  select(-year) |> 
  transmute(data = seq(as.Date("1996/01/01"), by = "month", length.out = 303),
            dummy = dummy)

codace$dummy <- as.numeric(codace$dummy)


rm(pib)


  