#################################
### Importa as series do BCB  ###
### Projeto: dfm              ###
#################################

# Nao esquecer de configurar as datas na funcao getSerie


#
# Necessario instalar os packages abaixo na primeira fez
#
# install.packages("RCurl")
# install.packages("XML")
# source("http://bioconductor.org/biocLite.R")
# biocLite("XMLSchema")
# biocLite("SSOAP")


# Part of script was originally written during for a summer course
# at FGV in January 2011. The script was used to show the
# students how to retrive data from Banco Central do Brasil
# (http://bcb.gov.br/?SERIETEMP) using SOAP protocol and R.
# fonte: http://arademaker.github.io/blog/2012/01/02/package-SSOAP.html


library(SSOAP)
library(XML)
library(RCurl)

rm(list = ls())

wsdl <- getURL("https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl", ssl.verifypeer = FALSE)
doc <- xmlInternalTreeParse(wsdl)
def <- processWSDL(doc)
ff <- genSOAPClientInterface(def = def)


getSeries <- function(codigos, data.ini = "01/01/1996", data.fim = "31/12/2015", remove.old = TRUE) {
  require(zoo)
  xmlstr <- ff@functions$getValoresSeriesXML(codigos, data.ini, data.fim,
                                             .opts = list(ssl.verifypeer = FALSE))
  doc <- xmlInternalTreeParse(xmlstr)
  
  cleanup <- xpathApply(doc,"//SERIE", function(s) {
    id <- xmlGetAttr(s, "ID")
    s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
    s1 <- t(s1)
    dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
    df <- as.data.frame(s1, stringsAsFactors=FALSE)
    df$SERIE <- id
    df
  })
  df <- Reduce(rbind, cleanup)
  df$data <- as.Date(sapply(strsplit(df$DATA, "/"),
                            function(x) paste(c(x[2:1], 1), collapse="-")), "%Y-%m-%d")
  df$valor <- as.numeric(df$VALOR)
  df$serie <- factor(df$SERIE)
  if(remove.old){
    df$BLOQUEADO <- NULL
    df$SERIE <- NULL
    df$DATA <- NULL
    df$VALOR <- NULL
  }
  df
  tsl <- split(df$valor, df$serie)
  # transforma list para matrix
  tsm <- matrix(unlist(tsl), nrow = length(tsl[[1]]), byrow = F)
  colnames(tsm) <- names(tsl)
  # serie em formato zoo
  tempo <- split(df$data, df$serie)
  series <- zoo(tsm, tempo[[1]])
  return(series)
}



# Ipeadata
# O script em R que utilizei para Webscraping é apresentado abaixo:


get_ipea <- function(id){
  library(rvest)
  library(zoo)
  pagina <- read_html(paste("http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=", id, "&module=M", sep=""))
  dados <-pagina %>% html_nodes(".dxgv")%>% html_text()
  # remove da memoria 
  rm(pagina)
  dados <- dados[dados!=""]
  #valor e data
  data <- dados[seq(1, length(dados), by = 2)]
  data <- as.Date(paste(data, ".01", sep=""), "%Y.%m.%d")
  valor <- dados[seq(2, length(dados), by = 2)]
  valor <- gsub("[.]","", valor)
  valor <- as.numeric(gsub(",",".", valor))
  # serie em formato zoo
  serie <- zoo(valor, data)
  return(serie)
}

# Importando os dados com a funcao getSeries

desc <- read.csv("data-raw/descricao_dados.csv")
cod <- substring(desc[desc$fonte=="Ipeadata", 2], 2)

i=1
series <- get_ipea(cod[i])
for(i in 2:length(cod)){
  series <- cbind(series, get_ipea(cod[i]))
}
colnames(series)<-cod

# importa series do banco central
cod <- substring(desc[desc$fonte=="Banco Central", 2], 2)
series <- cbind(series, getSeries(cod))

tsm <- window(series, start=index(series)[889], end=index(series)[1106])

write.zoo(tsm, file = "data-raw/series.csv")

