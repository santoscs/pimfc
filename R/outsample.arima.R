#' Previsao fora da amostra com arima
#' 
#' outsample.arima e usada para gerar previsoes fora da amostra
#' usando modelo arima estimado automaticamente segundo package 
#' \code{forecast}
#' 
#' @param x serie a ser prevista
#' @param h horizonte de previsao
#' @param k numero de previsoes fora da amostra
#' 
#' @return uma lista com
#' fcast(ts): previsao pontual e intervalos de predicao usando arima 
#' outdate(num): vetor contendo periodo fora da amostra 
#' fit(Arima): ultimo modelo arima estimado
#' 
#' @import forecast
#' @export


outsample.arima <- function(x, h, k){
  Tn <- length(x)
  timeline <- time(x)
  freq <- frequency(x)
  fcast <- NULL
  outdate <- timeline[(Tn-k+1):Tn]
  for(i in 1:k){
    # restringe os dados
    x.ajuste <- window(x, end=timeline[Tn-k-h+i])
    # estima o modelo
    fit <- auto.arima(x.ajuste)
    # get prediction for ith value
    df <- data.frame(forecast(fit, h))
    fcast <- rbind(fcast, df[h,])
  }
  fcast <- ts(fcast, start=outdate[1], frequency = freq)
  return(list(fcast=fcast, outdate=outdate, fit=fit))
}

