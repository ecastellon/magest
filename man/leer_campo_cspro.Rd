% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mp.r
\name{leer_campo_cspro}
\alias{leer_campo_cspro}
\title{Campo-CSpro}
\usage{
leer_campo_cspro(
  tab_dict = character(),
  campo = "questionnaire",
  con_borrado = TRUE,
  nreg = -1,
  ...
)
}
\arguments{
\item{tab_dict}{character: nombre de la tabla en la base de datos}

\item{campo}{character: nombre del campo}

\item{con_borrado}{logical: incluir el campo "deleted" en la
consulta?. TRUE por omisión.}

\item{nreg}{numeric: número de registros a devolver. Por omisión,
todos (nreg = -1)}

\item{...}{argumentos para los parámetros de la función conn_mysql}
}
\value{
data.frame o NULL si error en la conexión a la base de
datos
}
\description{
Lee un campo de una base de datos construida con CSpro
}
\details{
Entre los argumentos del parámetro "..." puede estar un
objeto de conexión a la base de datos (conn); si no está, se
intentará establecer la conexión con los argumentos a los
parámetros de la función conn_mysql
}
\examples{
x <- par_conn_mysql()
leer_campo_cspro("hatodict", "id_Quest", host = x$host,
                  user = x$user, password = x$pwd,
                  dbname = x$dbname)

cn <- conn_mysql()
leer_campo_cspro("hatodict", "id_Quest", conn=cn)
}
\seealso{
conn_mysql
}
