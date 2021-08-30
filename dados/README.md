Os dados foram todos solicitados via API das seguinte fontes: BCB, IPEA e SIDRA.

O código data_pipe.R importa os dados em frequência mensal;
Cálcula o índice de deterioração macroeconômica;
Efetua a datação de ciclos com o algoritmo de Harding Pagan e converte para frequência mensal;
Trata os dados e organiza todos em um arquivo '.rds'

# Váriaveis Explicativas:

# Codigos API IPEA
- SWAP DI 360 media: BMF12_SWAPDI360F12
- IBOVESPA FECHAMENTO % MENSAL: ANBIMA12_IBVSP12
- FIPEZAP PRECOO VENDA BRASIL: FIPE12_VENBR12

# BCB SGS
- Indice de Liquidez: 28448

# Índice Composto:

# IPEA API

- Horas Trabalhadas Industria Dessaz: CNI12_HTRABD12
- Exped Papel Ondulado QTD: ABPO12_PAPEL12
- Prod Industria Geral Ind Base fix dessaz: PIMPFN12_QIIGSNAS12
- Index conf. empresario industrial geral: CNI12_ICEIGER12
- Indicador IPEA FBCF Const Civil Dessaz: GAC12_INDFBCFCCDESSAZ12
- M1 Média Período: BM12_M1MN12
- Ind. Expec Consumidor IEC: FCESP12_IICF12

# Total Demissões
- até 2019.12: CAGED12_DESLIG
- a partir 2020.01: CAGED12_DESLIGN12

# Datação de Recessão:

# SIDRA:

- PIB Trimestral: "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202"
