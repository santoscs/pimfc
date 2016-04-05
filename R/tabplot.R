#' Tabela compara REQM dos modelos
#' 
#' A partir de uma matrix x com as previsoes dos modelos e 
#' a series observada, retorna uma tabela com REQM, REQM relativo
#' e teste dm
#' 
#' @param x matriz com previsoes nas colunas
#' @param obs nome da serie observada
#' @param ref nome da previsao de referencia
#'
#' @return Uma matriz com REQM, EQMR e teste DM 
#' 
#' @export 
#' 
tab.reqm <- function(x, obs="ipca", ref="ipca.focus"){
  y <- x[,obs]
  nomes <- c(ref ,colnames(x[, colnames(x)!=obs & colnames(x)!=ref]))
  # calculca reqm e reqm relativo
  eqm <- reqm <- NULL
  for(i in 1:length(nomes)){
    reqm <- cbind(reqm,sqrt(mean((x[,nomes[i]]-y)^2)))
  }
  for(i in 1:length(nomes)){
    eqm <- cbind(eqm,mean((x[,nomes[i]]-y)^2))
  }
  colnames(reqm) <- colnames(eqm) <- nomes
  eqmr <- eqm/eqm[,ref]
  #dm test
  dm<-NULL
  e <- apply(x[,nomes], 2, function(x) x-y)
  sele <- colnames(x[, colnames(x)!=obs & colnames(x)!=ref])
  for(i in sele){
    dm <- cbind(dm, forecast::dm.test(e1=e[,i], e2=e[,ref], h=12)$p.value)
  }
  colnames(dm) <- sele
  # formatando numeros
  dm <- format(round(dm, digits = 2), decimal.mark = ",")
  reqm <- format(round(reqm, digits = 2), decimal.mark = ",")
  eqmr <- format(round(eqmr, digits = 2), decimal.mark = ",")
  tab <- rbind(reqm, eqmr)
  tab <- rbind(tab, c("",dm))
  tab <- t(tab)
  colnames(tab) <- c("reqm", "eqmr", "dm test")
  return(tab)
}

#' Tabela com teste de previsao incorporada
#' 
#' A partir de uma matrix x com as previsoes dos modelos e 
#' a series observada, retorna uma tabela com o teste de previsao
#' incorporada de Harvey et. al. 1998
#' 
#' @inheritParams tab.reqm
#' 
#' @return Uma matriz com a lambda estimado e o p valor do teste
#' de previsao incorporada
#' 
#' @export
#' 
tab.enctest <- function(x, obs="ipca12", ref="ipca12.focus"){
  n <- length(x[1,])-2
  tabela1 <- matrix(NA, n, 2)
  nomes <- c(ref ,colnames(x[, colnames(x)!=obs & colnames(x)!=ref]))
  colnames(tabela1) <- c("Lambda", "(valor p)")
  nomeB <- nomes[nomes!=ref]
  for(j in 1:length(nomeB)){
    aux <- enc.test(y=x[,obs], fA=x[,ref], fB=x[,nomeB[j]])
    tabela1[j,"Lambda"] <- aux["I(fB - fA)","Estimate"]
    tabela1[j,"(valor p)"] <- aux["I(fB - fA)","Pr(>|t|)"]
  }
  # formatando numeros
  tab <- format(round(tabela1, digits = 2), decimal.mark = ",")
  # junta as tabelas
  tabela <- matrix(paste(tab[,1], sub(" ", "", paste("(", tab[,2], ")", sep=""))), nrow = n)
  tabela <- cbind(nomeB, tabela)
  tabela <- cbind("ipca12.focus", tabela)
  colnames(tabela) <- c("Modelo A", "Modelo B", "Lambda (valor p)")
  return(tabela)
}

