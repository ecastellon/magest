% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/me.r
\name{out_remplazo}
\alias{out_remplazo}
\title{Remplazo de outlier}
\usage{
out_remplazo(x, cota = 0, fun, mayor = TRUE, msj = FALSE, ...)
}
\arguments{
\item{x}{numeric: los datos}

\item{cota}{numeric escalar: cota}

\item{fun}{function: función de los datos (x) que devuelve
sustituto de outlier}

\item{mayor}{logical: cota es cota superior? TRUE por omisión.}

\item{msj}{logical: un mensaje con indicadores?; FALSE por omisión}

\item{...}{argumentos pasados a fun}
}
\value{
numeric, invisible
}
\description{
Remplaza outlier
}
\details{
Sustituye los datos que son mayores (menores) que
arg. "cota", por los producidos por arg. "fun". La alternativa
mayor (menor) que el arg. "cota", la determina el
arg. "mayor". La función no verifica los argumentos pasados a
la función en pmt. "..."; cualquier error lo identifica la
función en cuestión, y en tal caso, los datos no son
modificados.
}
\examples{
(out_remplazo(c(200, 1:5, 1000), 100, mean, trim = 0.1))
}
