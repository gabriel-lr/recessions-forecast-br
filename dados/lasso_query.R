# Querying Multiple Time Series!


library(tidyverse)
library(ipeadatar)

#------------------------ IPEA --------------------------------------#

# Series disponiveis


series <- available_series(language = c("br"))

df <- series |> 
  filter(status == "Ativa" & freq == "Mensal")


nome <- c("Papelão ondulado - expedição de caixas, acessórios e chapas - quantidade",
          "Índice nacional de vendas - variação real",
          "SPC - número de consultas",
          "Índice de ações - Ibovespa - fechamento",
          "Consumo aparente - derivados de petróleo - média - quantidade/dia",
          "Produção - petróleo - média - quantidade/dia",
          "Índice de Atividade Econômica do Banco Central (IBC-Br) - dessazonalizado (2002=100)",
          "Expectativa média de Inflação - IPCA - taxa anualizada para os próximos seis meses",
          "Preços - IPCA - preços livres - var.",
          "Taxa de câmbio - R$ / US$ - comercial - compra - média",
          "Taxa de juros - CDI / Over - acumulada no mês",
          "Taxa de juros - TJLP",
          "Taxa de juros - Over / Selic - acumulada no mês",
          "Balanço de Pagam. - Balança comercial - Saldo",
          "Balanço de Pagam. - Bancos - variação ativos no exterior",
          "Balanço de Pagam. - Invest. dir. ext. - Saldo",
          "DSLP - total - setor público não-financeiro",
          "DSLP - total - setor público não-financeiro - % do PIB",
          "NFSP - setor público -resultado nominal - s/ desvalor. cambial - fluxo mensal corrente",
          "Dívida pública total",
          "Operações de crédito - Inadimplência da carteira de crédito - Total",
          "Meio de Pagamento - Ampliado - M2 - depósitos em poupança   - fim de período",
          "Meio de Pagamento - Restrito (M1) - depósitos à vista - média",
          "Base Monetária  - Restrita (M0) - média",
          "Meio de Pagamento - Restrito (M1) - papel-moeda em poder do público - média",
          "Taxa referencial - swaps - DI x  pré-fixada - 180 dias - média do período",
          "Taxa referencial - swaps - DI x pré-fixada - 360 dias - média do período",
          "Fundo de garantia por tempo de serviço (FGTS) - arrecadação bruta",
          "Indicadores Industriais - horas trabalhadas - indústria - índice dessazonalizado (média 2006 = 100)",
          "Índice de Confiança do Empresário Industrial (ICEI) - geral",
          "Indicadores Industriais - utilização da capacidade instalada - indústria - índice dessazonalizado (média 2006 = 100)",
          "Indicadores Industriais - pessoal empregado - indústria - índice dessazonalizado (média 2006 = 100)",
          "Energia elétrica - consumo -  quantidade",
          "Índice de confiança do consumidor (ICC)",
          "Índice de condições econômicas atuais (ICEA)",
          "Índice de expectativas do consumidor (IEC)",
          "Utilização da capacidade instalada - indústria - média",
          "Preços - IGP-M",
          "Preços - Índice FipeZap - imóveis anunciados - vendas - Brasil - índice (jun. 2012 = 100)",
          "Importações - quantum - índice (média 2006 = 100)",
          "Termos de troca - índice (média 2006 = 100)",
          "Exportações - quantum - índice (média 2006 = 100)",
          "Produção industrial",
          "Produção industrial - indústria geral - quantum - índice (média 2012 = 100)",
          "Vendas reais - varejo - índice (média 2014 = 100)",
          "Taxa de desocupação",
          "Preços - IPCA - geral",
          "Índice de Custos da Tecnologia da Informação (ICTI) - variação no mês",
          "Investimento líquido (formação líquida de capital fixo) (preços 2010)",
          "Consumo aparente - bens de consumo - índice dessaz. (média 2012 = 100)",
          "Consumo aparente - indústria geral - índice dessaz. (média 2012 = 100)",
          "Indicador IPEA de FBCF - índice dessaz. (média 1995 = 100)",
          "Arrecadação das receita federais - receita bruta",
          "índice de ações - NASDAQ - fechamento",
          "índice de ações - Dow Jones - fechamento"
          )
  
cod <- c("ABPO12_PAPEL12",
         "ABRAS12_INVR12",
         "ACSP12_SCPCC12",
         "ANBIMA12_IBVSP12",
         "ANP12_CDEPET12",
         "ANP12_PDPET12",
         "SGS12_IBCBRDESSAZ12",
         "BM12_IPCAEXP612",
         "BM12_IPCAPL12",
         "BM12_ERC12",
         "BM12_TJCDI12",
         "BM12_TJLP12",
         "BM12_TJOVER12",
         "BPAG12_BC12",
         "BPAG12_HFBAN12",
         "BPAG12_IDE12",
         "BM12_DTSPN12",
         "BM12_DTSPY12",
         "BM12_NFSPNNS12",
         "PAN12_DTSPY12",
         "BM12_CIN12",
         "BM12_DEPOUCN12",
         "BM12_DEVM12",
         "BM12_M0MN12",
         "BM12_PMPPM12",
         "BMF12_SWAPDI18012",
         "BMF12_SWAPDI36012",
         "CEF12_FGTS12",
         "CNI12_HTRABD12",
         "CNI12_ICEIGER12",
         "CNI12_NUCAPD12",
         "CNI12_PEEMPD12",
         "ELETRO12_CEET12",
         "FCESP12_IIC12",
         "FCESP12_IICA12",
         "FCESP12_IICF12",
         "CE12_CUTIND12",
         "IGP12_IGPMG12",
         "FIPE12_VENBR12",
         "FUNCEX12_MDQT12",
         "FUNCEX12_TTR12",
         "FUNCEX12_XQT12",
         "PAN12_QIIGG12",
         "PIMPFN12_QIIGN12",
         "PMC12_IVVR12",
         "PAN12_TDESOC12",
         "PRECOS12_IPCAG12",
         "DIMAC_ICTI2",
         "DIMAC_ILIQTOT112",
         "GAC12_CABCDESSAZ12",
         "GAC12_CAIGDESSAZ12",
         "GAC12_INDFBCFDESSAZ12",
         "SRF12_TOTGER12",
         "GM12_NASDAQ12",
         "GM12_DOW12"
         )
chave <- cbind(nome,cod)

#importando
ipea.list <- lapply(chave[,2],ipeadata)

#-------------------------- FRED MD -----------------------------------#
fredr_set_key("893ec9febb02fe0790b3a0d7ef5583df")

fred.nome <- c("Leading Indicators OECD: Reference series: Gross Domestic Product (GDP): Normalised for Brazil",
               "Leading Indicators OECD: Component series: Short-term interest rate: Original series for Brazil",
               "Leading Indicators OECD: Component series: Short-term interest rate: Normalised for Brazil",
               "Leading Indicators OECD: Component series: BTS - Order books: Normalised for Brazil",
               "Production: Manufacturing: Total manufacturing: Total manufacturing for Brazil",
               "Production: Mining: Total mining: Total for Brazil",
               "Leading Indicators OECD: Leading indicators: CLI: Normalised for Brazil",
               "Leading Indicators OECD: Leading indicators: CLI: Amplitude adjusted for Brazil",
               "Business tendency surveys (services): Demand evolution: Future tendency: National indicator for Brazil",
               "Business tendency surveys (retail trade): Employment: Future tendency: National indicator for Brazil",
               "Business tendency surveys (retail trade): Confidence indicators: Composite indicators: National indicator for Brazil",
               "Business tendency surveys (construction): Business situation - Activity: Tendency: National indicator for Brazil"
               
               )



fred.cod <- c("BRALORSGPNOSTSAM",
              "BRALOCOSTORSTM",
              "BRALOCOSTNOSTSAM",
              "BRALOCOBONOSTSAM",
              "BRAPRMNTO01GYSAM",
              "BRAPRMITO01IXOBSAM",
              "BRALOLITONOSTSAM",
              "BRALOLITOAASTSAM",
              "BRABVDEFT02STSAM",
              "BRABREMFT02STSAM",
              "BRABRCICP02STSAM",
              "BRABCBUTE02STSAM")

fred.chave <- cbind(fred.nome,fred.cod)

fred.list <- lapply(fred.chave[,2],fredr)

