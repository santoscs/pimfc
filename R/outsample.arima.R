#' Previsao fora da amostra com arima
#' 
#' outsample.arima e usada para gerar previsoes fora da amostra
#' usando modelo arima estimado automaticamente segundo package 
#' \code{forecast}
#' 
#' @param x serie a ser prevista
#' @param h horizonte de previsao
#' @param k numero de previsoes fora da amostra
#' @param max.p	Maximum value of p
#' @param max.q	Maximum value of q
#' @param max.P	Maximum value of P
#' @param max.Q	Maximum value of Q
#' @param max.order	Maximum value of p+q+P+Q if model selection is not stepwise.
#' @param max.d	Maximum number of non-seasonal differences
#' @param max.D	Maximum number of seasonal differences
#' 
#' 
#' @return uma lista com
#' fcast(ts): previsao pontual e intervalos de predicao usando arima 
#' outdate(num): vetor contendo periodo fora da amostra 
#' fit(Arima): ultimo modelo arima estimado
#' order(num): ordem do arima no inicio e fim da amostra
#' 
#' @importFrom forecast forecast auto.arima
#' 
#' @export

outsample.arima <- function(x, h, k, max.p=5, max.q=5,
                            max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1){
  Tn <- length(x)
  timeline <- time(x)
  freq <- frequency(x)
  fcast <- NULL
  outdate <- timeline[(Tn - k + 1):Tn]
  j <- 0
  ordem.arma <- matrix(NA, 2, 7)
  id <- c(2,2,1,1)
  for (i in 1:k) {
    x.amostra <- window(x, end = timeline[Tn - k - h + i])
    fit <- auto.arima(x.amostra, start.p=id[1], start.q=id[2], start.P=id[3], start.Q=id[4],
                      max.p=max.p, max.q=max.q, max.P=max.P, max.Q=max.Q, max.order=max.order, max.d=max.d, max.D=max.D)
    id <- fit$arma
    df <- data.frame(forecast(fit, h))
    fcast <- rbind(fcast, df[h, ])
    if(i==1|i==k){
      # registra o arima escolhido no inicio e no fim da amostra
      j<-j+1
      ordem.arma[j,] <- fit$arma
    }
  }
  fcast <- ts(fcast, start = outdate[1], frequency = freq)
  return(list(fcast = fcast[,1], outdate = outdate, fit = fit, order = ordem.arma))
}

