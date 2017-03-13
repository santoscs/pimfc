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


#' Tabela com teste de previsao incorporada
#' 
#' A partir de uma matrix x com as previsoes dos modelos e 
#' a series observada, retorna uma tabela com o teste de previsao
#' incorporada de Harvey et. al. 1998
#' 
#' @inheritParams tab.reqm
#' @param h Length ahead of the forecast.
#' 
#' @return Uma matriz com a lambda estimado e o p valor do teste
#' de previsao incorporada
#' 
#' @export
#' 
tab.comptest <- function(x, obs="ipca12", ref=NULL, h){
  n <- length(x[1,])-2
  tabela1 <- matrix(NA, n, 2)
  nomes <- c(ref ,colnames(x[, colnames(x)!=obs & colnames(x)!=ref]))
  colnames(tabela1) <- c("Lambda", "(valor p)")
  nomeB <- nomes[nomes!=ref]
  for(j in 1:length(nomeB)){
    aux <- comp.test(y=x[,obs], fA=x[,ref], fB=x[,nomeB[j]], h = h)
    tabela1[j,"Lambda"] <- aux["fB","Estimate"]
    tabela1[j,"(valor p)"] <- aux["fB","Pr(>|t|)"]
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


#' Plota series temporais com ggplot2
#' 
#' @param x objeto ts ou mts com as series temporais
#' @param y (opicional) objeto ts ou mts com dimensao de x para ser
#' plotado junto com x no mesmo grafico 
#' @param escala Are scales shared across all facets
#'  ("fixed"), or do they vary across 
#'  rows ("free_x"), columns (the default, "free_y"), or both 
#'  rows and columns ("free")
#' @param facet as series sao plotadas em graficos diferente (facet = TRUE, the default),
#' ou no mesmo grafico (facet = FALSE)
#' @param name optional name for ts univariate
#' 
#' @return ggplot das series
#' 
#' @import ggplot2 zoo
#' 
#' @export
#' 

tsplot <- function(x, y = NULL, escala = 'free_y', facet = TRUE, name = NULL){
  nseries <- NCOL(x)
  ntime <- NROW(x)
  x <- zoo::as.zoo(x)
  df.x <- zoo::fortify.zoo(x, melt = TRUE)
  if(nseries==1 & !is.null(name)){
    df.x[,"Series"] <- rep(name, ntime)
  }
  if(!is.null(y)){
    y <- zoo::as.zoo(y)
    df.y <- zoo::fortify.zoo(y, melt = TRUE)
    if(facet){
      df <- ggplot2::fortify(cbind(df.x, Value2=df.y[,3]), index.name = "Index")
      p <- ggplot2::ggplot(data = df, ggplot2::aes(x = Index, y = Value))
      p <- p + ggplot2::geom_line(data = df, ggplot2::aes(x = Index, y = Value2),
                                  linetype=2, colour="red", size = 1/2, alpha = 1)
      p <- p + ggplot2::geom_line(size = 1/2, alpha = 1, colour="blue")  
      p <- p + ggplot2::facet_grid(Series ~ ., scales = "free_y") 
      #p <- p + ggplot2::facet_wrap(~ Series, scales = "free_y")
    }else{
      p <- ggplot(df.x, aes(x = Index, y = Value))
      p <- p + geom_line(data = df.y, aes(x = Index, y = Value, group = Series), size = 1/2, alpha = 1, colour="blue")
      p <- p + geom_line(linetype=2, size = 1/2, alpha = 1, colour="blue")  # Drawing the "overlayer"
    }
    p <- p + ggplot2::labs(y="", x="")
    p <- p + ggplot2::theme_bw(base_size=14)
    return(p)  
  }
  if(!facet){
    p <- ggplot2::ggplot(data = df.x, ggplot2::aes(x = Index, y = Value, color=Series, linetype=Series))
    p <- p + ggplot2::geom_line(size = 3/4)  
    p <- p + ggplot2::labs(y="", x="")
    p <- p + ggplot2::theme_bw(base_size=14)
    return(p)  
  }else{
    p <-ggplot2::ggplot(df.x, ggplot2::aes(x=Index, y=Value, group_by())) +
      ggplot2::geom_line(size = 1/2, alpha = 1, colour="blue") +
      ggplot2::facet_grid(Series ~ ., scales = escala) +
      ggplot2::labs(y="", x="") +
      ggplot2::theme_bw(base_size=14)
  }
  return(p)
}

