##
## Descricao dos dados

descricao <- read.csv("data-raw/descricao_dados.csv", encoding = "UTF-8")
devtools::use_data(descricao, overwrite = TRUE)


## series brutas naotransformadas

rawmacroseries <- read.csv("data-raw/series.csv", sep="")[,-1]
devtools::use_data(rawmacroseries, overwrite = TRUE)
