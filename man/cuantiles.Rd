% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ut.r
\name{cuantiles}
\alias{cuantiles}
\title{Cuantiles}
\usage{
cuantiles(x, cuan = 0.25, mayor_que_0 = TRUE, ...)
}
\arguments{
\item{x}{numeric y con más de un elemento}

\item{cuan}{numeric: intervalo fijo}

\item{mayor_que_0}{logical: excluir datos menor o igual a cero?. TRUE por
defecto.}

\item{...}{adicionales pasados a función quantile}
}
\value{
NA o numeric
}
\description{
Cuantiles
}
\details{
Es una especialización de la función quantile. Las
probabilidades corren al intervalo fijo indicado en el
parámetro «cuan», y permite incluir en el cálculo sólo datos
mayores que 0 y no NA (parámetro «mayor_que_0»).
}
\examples{
cuantiles(sample(1:10, 100, replace = TRUE), cuan = 0.1)
}
\seealso{
quantile, deciles, quintiles
}
