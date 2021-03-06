% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/di.R
\name{mdd.selec}
\alias{mdd.selec}
\title{Seleciona o modelo defasagem distribuida}
\usage{
mdd.selec(yh, yt, x, h, m, p)
}
\arguments{
\item{yh}{serie a ser prevista \eqn{y_{t+h}^h}}

\item{yt}{preditor \eqn{y_{t-j+1}}}

\item{x}{preditor}

\item{h}{horizonte de previsao}

\item{m}{numero maximo de defasagens de x}

\item{p}{numero maximo de defasagens de yt}
}
\value{
best.fit o modelo selecionado dyn
}
\description{
Seleciona o modelo defasagem distribuida com menor BIC
}

