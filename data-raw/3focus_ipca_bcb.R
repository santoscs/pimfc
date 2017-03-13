#########################################
### Previsoes da pesquisa Focus - BCB ###
### Projeto: dfm                      ###
#########################################

rm(list=ls())
library(zoo)
library(xts)

#####################################
# Previsao para o ipca mensal       #
# no horizonte de 12 meses a frente #
#####################################

arquivos <- c("ipca_mediana_03_04", "ipca_mediana_05_06",
              "ipca_mediana_07_08", "ipca_mediana_09_10",
              "ipca_mediana_11_12", "ipca_mediana_13_14")

ipca.expec <- vector("list", length = length(arquivos))
for(i in arquivos){
  aux <- read.csv2(paste("data-raw/expectativas/", i,".csv", sep=""), skip=1)
  datas <- aux[,1]
  n <- length(aux[1,])
  serie <- zoo(aux[,c(-1,-n)], order.by=as.Date(datas, format="%d/%m/%Y"))
  ipca.expec[[paste(i)]] <- aggregate(serie, by= as.Date(as.yearmon(time(serie))), mean, na.rm = TRUE)
}


mult2one <- function(x){
  #input: 
  # x(zoo): serie mensal obitida do Focus-BCB
  #output: 
  # serie(zoo):  serie mensal com apenas as previsoes 12 meses a frente
  
  noqual <- as.yearmon(time(x))
  paraoqual <- noqual + 1
  periodo <- as.Date(paste('01', paraoqual), "%d %b %Y") 
  paraoqual <- sub(" ", ".", paraoqual)
  paraoqual <- tolower(paraoqual)
  noqual <- as.Date(noqual)
  serie <- NULL
  for(i in 1:length(noqual)){
    serie <- rbind(serie, x[noqual[i],paraoqual[i]])
  }
  return(serie)
}


ipca.focus <- NULL
for(i in arquivos){
  ipca.focus <- rbind(ipca.focus, mult2one(ipca.expec[[i]]))
}

# acresenta um ano para o periodo corresponder com
# o periodo para o qual se esta prevendo
paraoqual <- as.yearmon(time(ipca.focus))+1
periodo <- as.Date(paste('01', paraoqual), "%d %b %Y") 
ipca.focus <- zoo(ipca.focus, order.by = periodo)




#####################################
# Previsao para o ipca em 12 meses  #
# no horizonte de 12 meses a frente #
#####################################

arquivos <- c("ipca12_mediana_02_03", "ipca12_mediana_04_05",
              "ipca12_mediana_06_07", "ipca12_mediana_08_09",
              "ipca12_mediana_10_11", "ipca12_mediana_12_13",
              "ipca12_mediana_14_15", "ipca12_mediana_16")

## Importa as series de expectativas
i=arquivos[1]
ipca12.focus <- NULL
for(i in arquivos){
  aux <- read.csv2(paste("data-raw/expectativas/", i,".csv", sep=""), skip=1, sep=";")
  datas <- aux[,1]
  n <- length(aux[1,])
  serie <- zoo(aux[,c(-1,-n)], order.by=as.Date(datas, format="%d/%m/%Y"))
  ipca12.focus <- rbind(ipca12.focus,serie)
}

## transforma as series irregulares para mensais
ipca12.focus <- aggregate(ipca12.focus, by= as.Date(as.yearmon(time(ipca12.focus))), mean, na.rm = TRUE)

## salva as series
#write.zoo(cbind(ipca.focus, ipca12.focus), file = "data-raw/focus_ipca_bcb.csv")

## salva os dados
#focus <- cbind(ipca.focus, ipca12.focus)
focus <- ts(ipca12.focus, start = c(2002, 1), frequency = 12)

devtools::use_data(focus, overwrite = TRUE)
