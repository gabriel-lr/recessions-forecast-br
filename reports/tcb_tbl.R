# Indice de Deterioracao Table

df.tbl <- df.tcb |> 
  mutate(dem_cons = dplyr::coalesce(dem_desc,dem_att)) |> # encadeando séries de demissões
  dplyr::select(-dem_desc, -dem_att)

st.date <- function(x){
  df.tbl |> 
    dplyr::select(data, x) |> 
    tidyr::drop_na() |> 
    dplyr::select(data) |> 
    dplyr::slice_min(data)
  
}
n.obs <- function(x){
  df.tbl |> 
    dplyr::select(data, x) |> 
    tidyr::drop_na() |> 
    nrow()
}



####------ init date----------##
start.date <- lapply(colnames(df.tbl)[-1], st.date) |> 
  as.data.frame()
colnames(start.date) <- colnames(df.tbl)[-1]

start.date <- tidyr::gather(start.date) |> 
  dplyr::rename(Ap. = key,
                Inicio = value)

#---- n obs------###
num.obs <- lapply(colnames(df.tbl)[-1], n.obs) |> 
  as.data.frame()
colnames(num.obs) <- colnames(df.tbl)[-1]

num.obs <- tidyr::gather(num.obs)|> 
  dplyr::rename(Ap. = key,
                Obs. = value)


#----------- Nome ------------###

nomes.ofc <- c("Horas Trabalhadas Industria Dessaz.",
               "Expedicao Papel Ondulado Qtd",
               "Prod. Industria Geral Ind. Base fixa Dessaz.",
               "Index Confianca Empresario Industrial Geral (ICEI)",
               "Indicador IPEA FBCF Const Civil Dessaz",
               "M1 Media Periodo",
               "Ind. Expec Consumidor IEC",
               "Total Demissoes")

#---------------- Fonte ---------------------#

nomes.fonte <- c("IPEA",
                 "IPEA",
                 "IPEA",
                 "IPEA",
                 "IPEA",
                 "IPEA",
                 "IPEA",
                 "IPEA")

tcb_tbl <- dplyr::left_join(start.date, num.obs, by = "Ap.") |> 
  dplyr::mutate(Serie = nomes.ofc,
                Fonte = nomes.fonte) |> 
  dplyr::relocate(Serie, .before = "Ap.")


tcb_tbl |> 
  gt::gt() |> 
  gt::tab_header(
    title = "Indicadores Coincidentes de Atividade Economica",
  )