# Tabela



st.date <- function(x){
my_data |> 
  dplyr::select(data, x) |> 
  tidyr::drop_na() |> 
  dplyr::select(data) |> 
  dplyr::slice_min(data)
  
}
n.obs <- function(x){
  my_data |> 
    dplyr::select(data, x) |> 
    tidyr::drop_na() |> 
    nrow()
}

####------ init date----------##
start.date <- lapply(colnames(my_data)[-1], st.date) |> 
  as.data.frame()
colnames(start.date) <- colnames(my_data)[-1]

start.date <- tidyr::gather(start.date) |> 
  dplyr::rename(Ap. = key,
                Inicio = value)

#---- n obs------###
num.obs <- lapply(colnames(my_data)[-1], n.obs) |> 
  as.data.frame()
colnames(num.obs) <- colnames(my_data)[-1]

num.obs <- tidyr::gather(num.obs)|> 
  dplyr::rename(Ap. = key,
                Obs. = value)


#----------- Nome ------------###

nomes.ofc <- c("Taxa referencial entre Swap e Pré Fixada de 360 dias",
                   "Índice de ações: Ibovespa - fechamento",
                   "Preços - Índice FipeZap Imóveis Anunciados vendas no Brasil",
                   "Índice de Liquidez do Sistema Bancário Nacional",
                   "Índice Composto de Deterioração Macroeconomica",
                   "Datação de Recessões (Harding & Pagan")

#---------------- Fonte ---------------------#

nomes.fonte <- c("IPEA",
                "IPEA",
                "IPEA",
                "SGS BACEN",
                "Formulação Própria",
                "CODACE")

fonte_tbl <- dplyr::left_join(start.date, num.obs, by = "Ap.") |> 
                dplyr::mutate(Serie = nomes.ofc,
                              Fonte = nomes.fonte) |> 
                dplyr::relocate(Serie, .before = "Ap.")

fonte_tbl <- gt::gt(fonte_tbl) 
  
fonte_tbl |> 
  gt::tab_header(
    title = "Variáveis do Modelo",
  )
  


