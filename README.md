![example workflow](https://github.com/gabriel-lr/recessions-forecast-br/actions/workflows/schedule_data.yaml/badge.svg)
# recessions-forecast-br
Pesquisa de Mestrado desenvolvida no PPGEco Ufes.

O projeto abordará a Previsão de Recessões para a economia brasileira.

Serão utilizados os 4 modelos probit proposto em Kauppi e Saikonen (2008) e técnicas de Statistical Learning.

/dados: Scripts para obtenção e tratamento dos dados trabalhados via api no ambiente R.

/eda: Scripts de análises exploratórias dos dados.

/modelos: Scripts de aplicação dos modelos utilizados.

/report: Scripts dos reports via Rmarkdown.

/workflow/schedule_data.yaml:  job 'data-generate' Automate R script 'data_pipe.R' and create "my_data.rds" object.

## Execução

### data-generate

O script data-generate foi agendado para todo 1º dia do mês corrente inicializar o data_pipe.R e salvar os resultados dentro do arquivo "my_data.rds".
Assim, o arquivo gerado trás a base de dados atualizada mensalmente.

Basta importar o arquivo de dados para dentro dos scripts que desejar rodar a seguir.




