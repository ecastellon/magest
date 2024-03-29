% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mp.r
\name{xsql_t}
\alias{xsql_t}
\title{SQL expresión matriz}
\usage{
xsql_t(
  x = character(),
  cam = character(),
  nvb = character(),
  idr = "quest",
  xfi = TRUE,
  idc = integer(),
  cid = character(),
  all = TRUE
)
}
\arguments{
\item{x}{character: nombre de la tabla de la base de datos que se
va a consultar}

\item{cam}{character: nombre de los campos}

\item{nvb}{character: nombre de las columnas que tendrá el
data.frame}

\item{idr}{character: nombre del campo de la tabla con el "id" de
los registros. Por omisión, "quest".}

\item{xfi}{logical: la secuencia de los datos que corresponden a
los campos es por fila (TRUE, por defecto) o por columna
(FALSE).}

\item{idc}{números (integer) o nombres (character) que van
etiquetar los registros producidos por cada consulta. Es
opcional.}

\item{cid}{character: nombre de la columna que llevará la etiqueta
de las consultas. Es opcional, pero atado al parámetro ("idc").}

\item{all}{logical: incluir todos los registros (TRUE por defecto,
cláusula «union all») o no permitir duplicados (FALSE, cláusula
«union»).}
}
\value{
character
}
\description{
Expresión SQL para leer campos que forman una matriz
de datos
}
\details{
La mayoría de las tablas donde se almacenan los datos de
las encuestas son "planas" (todas las variables aparecen como
un campo de la tabla), aunque para la estimación y el análisis,
los datos se arreglan en un cuadro con la siguiente estructura
(c??? es el nombre del campo)\preformatted{cxx1 cxx2 cxx3 cxx4

cxx5 cxx6 cxx7 cxx8

.... .... .... ....

o

cxx1 cxx3 cxx5 cxx7

cxx2 cxx4 cxx6 cxx8

donde las columnas del cuadro son las variables (p.ej. cultivo,
manzanas sembradas, etc.) y las filas corresponden a un
registro o cuestionario.

Cada fila se lee de la base de datos por medio de una
subconsulta SQL, y todas se combinan en una sola con la
cláusula 'union all', para reunir (con una especie de
\code(rbind)) a todos los data.frame que devuelven las
subconsultas. Para que esto funcione, además del nombre de los
campos (parámetro "cam"), la función pide el nombre común que
tendrán las columnas o variables (parámetro "nvb") de los
data.frame. Por fuerza, el número de campos
(\code{length(cam)}) debe ser múltiplo del número de columnas
(\code{length(nvb)}).

Es opcional agregar una variable que contenga una etiqueta para
identificar cada uno de los data.frame. El parámetro "cid"
recibiría el nombre de esa columna y el parámetro "idc" el de
las etiquetas, que deberían ser tantas como los data.frame
parciales. En consecuencia se debería satisfacer la condición
\code{length(cam) == length(nvb) * length(idc)}.

Para fines de comprobación, cada fila de los data.frame trae el
«id» del registro de la tabla (parámetro "idr", que
generalmente es el número de cuestionario) de donde se leyeron
los datos.
}
}
\examples{
xsql_t("segene", paste0("c", 121:126), c("cult", "semb", "per"))
}
\seealso{
xsql_u, normalizar_data
}
