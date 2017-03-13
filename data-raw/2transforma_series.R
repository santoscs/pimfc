### 2 tratamento dos dados ###
### Projeto: dfm           ###
##############################

# importa os dados
library(zoo)
rm(list = ls())
desc <- read.csv("data-raw/descricao_dados.csv")
desc <- desc[!is.na(desc$trans),]
desc <- transform(desc, cod = as.character(cod))
series_bcb <- read.csv("data-raw/series.csv", sep=" ")
series <- as.zoo(series_bcb[,-1], order.by = series_bcb[,1])



##############################################
### Transforma as series nominais em reais ###
##############################################

db <- series
#cria a serie ipca
ipca <- series[,"X38513"]
# calcula o indice deflator a partir do ipca
indice <- vector()
indice[1] <-100
for(i in 2:length(ipca)){
  indice[i] <- indice[i-1] + indice[i-1]*(ipca[i]/100)
}
# deflaciona as series nominais
sele <- desc$cod[desc$trans==1 | desc$trans==4 | desc$trans==5]
db[,sele] <- apply(series[,sele], 2, function(x) x*(indice[1]/indice))



#######################################################
### aplicacao a transformação para estacionaridade ###
#######################################################

tsm <- db[2:length(db[,1]),]
## aplica log diferenca para series
sele <- desc$cod[desc$trans==3 | desc$trans==4]
tsm[,sele] <- apply(db[,sele], 2, function(x){diff(log(x))})
## aplica diferenca para series não normais
sele <- desc$cod[desc$trans==2 | desc$trans==5]
tsm[,sele] <- apply(db[,sele], 2, function(x) diff(x))


###########################
### ajuste para outlier ###
###########################

# inter quartile range
i.quartile <- function(x){
  # calcula o intervalo interqualitico
  return(summary(x)[5] - summary(x)[2])
}

# Substitui valores maiores que 6 vezes o intervalo interquartilico 
# pela mediana dos ultimos 5 valores
outlier.adj <- function(x){
  iqr <- i.quartile(x)
  aux <-which((abs(x-median(x))>6*iqr)==TRUE)
  if(length(aux)!=0){
    for(i in 1:length(aux)){
      if(aux[i]>6){
        x[aux[i]]<- median(x[(aux[i]-6):(aux[i]-1)])
      }else{
        x[aux[i]]<- median(x[1:(aux[i]-1)])
      }
    }
  }
  return(x)
}

# realiza o ajuste
tsm <- apply(tsm, 2, outlier.adj)  
tsm <- ts(tsm, start = c(1996, 2), frequency = 12)


# salva os dados tratados
#write.zoo(tsm, file = "data-raw/series_transformadas.csv")

macroseries <- tsm
devtools::use_data(macroseries, overwrite = TRUE)
