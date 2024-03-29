# Harding Pagan vs CODACE

# Coleta e tratamento de dados
pib <- sidrar::get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202") |>
  janitor::clean_names() |> 
  dplyr::select("date" = trimestre_codigo, "value" = valor)  |> 
  dplyr::mutate(value = value, date = lubridate::yq(date)) |> 
  dplyr::as_tibble()


# Obter datação de ciclo de negócios
bc_dates <- pib |> 
  timetk::tk_ts(select = value, start = c(1996, 1), frequency = 4) |> 
  BCDating::BBQ(name = "Ciclo de Negocios do PIB do Brasil")

# Exibir resultados
a <- as.data.frame(show(bc_dates))

#---------- CODACE -----------#

rec_tbl <- a |> 
  dplyr::mutate(Picos = c("1 Trimestre 2001",
                          "4 Trimestre 2002",
                          "3 Trimestre 2008",
                          "1 Trimestre 2014",
                          "1 Trimestre 2020"),
                Vales = c("4 Trimestre 2001",
                          "2 Trimestre 2003",
                          "1 Trimestre 2009",
                          "4 Trimestre 2016",
                          "Sem data de termino"),
                Duracao = (c("3", "2", "2", "11", "-")))
 

rec_tbl |> 
  gt::gt() |> 
  gt::tab_header(
    title = "Variaveis do Modelo",
  ) |> 
  gt::tab_spanner(
    label = "Harding & Pagan (2002)",
    columns = c(Peaks, Troughs, Duration)
  ) |> 
  gt::tab_spanner(
    label = "Codace (2020)",
    columns = c(Picos, Vales, Duracao)
  ) |> 
  gt::tab_source_note(
    source_note = "Fonte: Elaboracao Propria"
  )
