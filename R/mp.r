# -*- coding: utf-8 -*-

##--- funciones procesamiento de datos ---

#' @import magfoo

#' @export
set_odb <- function(ob, ...) UseMethod("set_odb")
#' @export
odb_check <- function(ob, ...) UseMethod("odb_check")
#' @export
get_atr <- function(x, atr) UseMethod("get_atr")
#' @export
get_data <- function(ob, ...) UseMethod("get_data")
#' @export
db_constr <- function(ob) UseMethod("db_constr")
#' @export
db_open <- function(ob) UseMethod("db_open")
#' @export
db_close <- function(ob) UseMethod("db_close")
#' @export
db_info <- function(ob) UseMethod("db_info")
#' @export
db_tablas <- function(ob, msg) UseMethod("db_tablas")
#' @export
db_columnas <- function(ob, tabla) UseMethod("db_columnas")
#' @export
db_qry <- function(ob, xs, ...) UseMethod("db_qry")
#' @export
db_fetch <- function(ob, tabla, ...) UseMethod("db_fetch")
#' @export
db_save <- function(ob, df, tb, nr, ...) UseMethod("db_save")
#' @export
db_drop <- function(ob, tb) UseMethod("db_drop")
#' @export
tabla_exists <- function(ob, tb) UseMethod("tabla_exists")
#' @export
is_open <- function(ob) UseMethod("is_open")

#' Clase S3 odb
#' @description Crea e inicializa un objeto de clase odb.
#'
#' @details Con las propiedades de este objeto se construye la cadena
#'     de conexión ('connection string') que habilita 'conectarse' o
#'     'abrir' la base de datos para mandar ejecutar instrucciones
#'     de consulta.
#'
#'     Para cada servidor de base de datos (SQL server, Excel, Access,
#'     MySQL, SQLite, etc.) hay una 'connection string' específica
#'     según la aplicación que sirve de intermediario con el servidor.
#'
#'     Con las librería RODBC y RODBCDBI se usan "driver" o
#'     controladores ODBC, administrados por la aplicación
#'     (\url{c:/windows/system32/odbcad32.exe})
#'     "administrador de datos ODBC". El menú "controladores" muestra
#'     los "driver" ODBC instalados en el equipo. Si no aparece el
#'     indicado hay que descargar e instalar el "software" que lo
#'     instala.
#'
#'     En \url{https://www.connectionstrings.com} y otras similares,
#'     están descritas las 'connection string' más comunes. Estas
#'     llevan uno o más parámetros junto con el valor que les
#'     corresponde. Por ejemplo, la conexión ODBC mínima a libros
#'     Excel versión 2007 requiere los parámetros 'Driver' y 'Dbq': el
#'     primero para indicar el controlador y el segundo para la ruta
#'     de acceso al libro: 'Driver={Microsoft Excel Driver (*.xls,
#'     *.xlsx, *.xlsm, *.xlsb)}; DBQ=\url{c:/MyExcel.xlsx};'. En este
#'     caso, 'Driver' y 'Dbq' son las propiedades que se deben
#'     especificar por medio de un objeto de la clase odb.
#' @param ... captura uno o más de los parámetros necesarios para
#'     especificar la 'connection string'. Si no se indica ninguno el
#'     resultado será una lista sin elementos, que después se podrá
#'     llenar con la función 'setter' \code{set_odb}.
#' @return objeto de clase odb
#' @seealso \code{odb_sql}, \code{odb_xcl}
#' @examples
#' ob <- odb(Driver = "{SQL Server}", server = "ser.o.no.ser",
#'           database = "dt", uid = "yo_mero", pwd = "123-probando")
#' @export
odb <- function(...) {
    x <- list(...)
    class(x) <- "odb"
    invisible(x)
}

#' Clase odb
#' @description Es objeto clase odb?
#' @param x objeto
#' @return logical
#' @examples
#' ob <- odb()
#' is_odb(ob)
#' @export
is_odb <- function(x) {
    inherits(x, "odb")
}

#' print.odb
#' @description Muestra los datos. Si existe palabra clave se muestra
#'     como serie de "*"
#' @param x objeto odb
#' @examples
#' ob <- odb()
#' print(odb)
#' @export
print.odb <- function(x) {
    xx <- x
    if (is.element("pwd", names(xx))){
        xx["pwd"] <- strrep("*", nchar(xx$pwd))
    }
    class(xx) <- "list"
    print(xx)
}

#' str.odb
#' @description Ver la estructura de objeto odb. Si hay palabra clave
#'     se muestra como serie de "*"
#' @param x objeto odb
#' @examples
#' ob <- odb()
#' str(ob)
#' @export
str.odb <- function(x) {
    print(x)
}

#' Get odb property
#' @description devuelve una propiedad
#' @param x objeto odb
#' @param atr character: atributo o propiedad
#' @return valor atributo o NA
#' @examples
#' ob <- odb(server = "{SQL Server}", database = "db")
#' get_atr(ob, "server")
#' @export
get_atr.odb <- function(x, atr){
    stopifnot("arg atr inadmisible" = filled_char(atr))
    ifelse(is.element(atr, names(x)), x[[atr]], NA_character_)
}

#' Modificar odb
#' @description Modifica o agrega una o más propiedades a un objeto
#'     odb. Las propiedades previamente definidas son actualizadas a
#'     los nuevos valores.
#' @param x objeto odb
#' @param ... nuevas propiedades o modificaciones a las ya definidas
#' @return objeto odb
#' @examples
#' ob <- odb(server = "{SQL Server}", database = "db")
#' ## agrega user y modifica database
#' ob <- set_odb(ob, database = "ndb", user = "me")
#' str(ob)
#' @export
set_odb.odb <- function(x, ...) {
    cc <- names(x)
    xx <- list(...)
    zz <- c(x[setdiff(cc, names(xx))], list(...))
    class(zz) <- class(x)
    invisible(zz)
}

#' Check - odb
#' @description Comprueba existencia atributos
#' @param x objeto odb
#' @return nada
#' @keywords internal
odb_check.odb <- function(x) {
    nm <- names(x)
    for (ss in c("driver", "server", "database", "uid", "pwd")) {
        if (is.null(x[[ss]]) || !is.element(ss, nm)) {
            warning(sprintf("%s no indicado\n", ss))
        }
    }
}

#' odb SQL Server
#' @description Inicializa objeto de clase odb para conectar con un
#'     servidor que corre SQL Server
#' @details La "connection string" debe contener el nombre y palabra
#'     clave del usuario. Por seguridad, una alternativa para
#'     "ocultar" esos datos es crear variables-ambiente definidas en
#'     el archivo de usuario .Renviron. La función las busca en las
#'     variables SQLUID (usuario), SQLPWD ("password") y
#'     SQLSERVERMAGIP, si estas no se pasan entre los argumentos.
#' @param driver character: "{SQL Server}" por defecto.
#' @param server character: dirección IP del servidor
#' @param database character: nombre de la base de datos
#' @param uid character: nombre del usuario
#' @param pwd character: contraseña del usuario
#' @return objeto de clase odb
#' @examples
#' ob <- db_sql(database = "encuestas")
#' str(ob)
#' @export
db_sql <- function(driver = "{SQL Server}", server = character(),
                   database = character(), uid = character(),
                   pwd = character()) {
    stopifnot("arg. drv. inadmisible" = filled_char(driver) &&
                  is_scalar(driver),
              "arg. serv. inadmisible" = is.character(server),
              "arg. data. inadmisible" = is.character(database),
              "arg. uid inadmisible" = is.character(uid),
              "arg. pwd inadmisible" = is.character(pwd))

    if (is_vacuo(uid)) {
        uid <- Sys.getenv("SQLUID")
    }
    if (is_vacuo(pwd)){
        pwd <- Sys.getenv("SQLPWD")
    }
    if (is_vacuo(server)) {
        server <- Sys.getenv("SQLSERVERMAGIP")
    }

    ob <- odb(driver = driver, server = server, database = database,
              uid = uid, pwd = pwd)
    ## validar
    odb_check(ob)

    invisible(ob)
}

#' odb excel
#' @description Inicializa objeto de clase odb para conectar con un
#'     libro excel.
#' @param file character: ruta de acceso al archivo
#' @param version7 versión 2007 de excel?. TRUE por defecto.
#' @param ronly acceso de sólo lectura?. TRUE por defecto.
#' @return objeto de clase odb
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx",
#' package = "XLConnect"))
#' str(ox)
#' @export
db_xcl <- function(file = character(), version7 = TRUE, ronly = TRUE) {

    stopifnot("arg. file inadmisible" = filled_char(file))

    ff <- tryCatch(normalizePath(file, mustWork = TRUE),
                   error = function(e){
                       stop("\n...archivo no existe!!")})

    dr <- "{Microsoft Excel Driver (*.xls"
    id <- "790"
    if (version7){
        dr <- paste0(dr, ", *.xlsx, *.xlsm, *.xlsb")
        id <- NULL
    }
    dr <- paste0(dr, ")}")

    invisible(odb(Driver = dr, DriverId = id, Dbq = ff,
                  DefaultDir = normalizePath(dirname(ff)),
                  ReadOnly = ronly))
}

#' Connection string
#' @description Devuelve la 'connection string' que se genera con las
#'     propiedades del objeto
#' @param x objeto de clase odb
#' @return character
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' db_constr(ox)
#' @export
db_constr.odb <- function(x) {
    cc <- c(x, recursive = TRUE)
    paste0(paste0(names(cc), "="), cc, collapse=";")
}

#' Conectar a servidor
#' @description Devuelve objeto que habilita llamadas al servidor de
#'     datos.
#' @details Llama la función \code{odbcDriverConnect} de RODBC.
#' @param x objeto odb
#' @return objeto RODBC o NULL
#' @seealso \code{is_rodbc}
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package
#' XLConnect))
#' op <- db_open(ox)
#' @export
db_open.odb <- function(x) {
    constr <- db_constr(x)
    invisible(tryCatch(
        RODBC::odbcDriverConnect(connection=constr,
                          tabQuote = c("[", "]")),
        error = function(e){
            message("\n...ERROR", constr)
        },
        warning = function(e) message("\n...ADVERTENCIA de conexión")))
}

#' Interrumpe conexión
#' @description Deshabilita el acceso a la base de datos
#' @param x objeto RODBC
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' db_close(op)
#' @return NULL
#' @export
db_close.RODBC <- function(x) {
    ww <- tryCatch(RODBC::odbcClose(x),
                   error = function(e) e)
    invisible(NULL)
}

#' Validar RODBC
#' @description Es objeto RODBC?.
#' @param x objeto RODBC
#' @return logical
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' is_rodbc(op)
#' db_close(op)
#' @export
is_rodbc <- function(x) {
    inherits(x, "RODBC")
}

#' Canal abierto
#' @description Habilitado el acceso a la base de datos?
#' @param x objeto RODBC
#' @return logical
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' is_open(op)
#' db_close(op)
#' @export
is_open.RODBC <- function(x) {
    !inherits(db_info(x), "error")
}

#' Database info
#' @description Información básica sobre la base de datos y el
#'     "driver"
#' @param x objeto RODBC
#' @return character
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' db_info(op)
#' db_close(op)
#' @export
db_info.RODBC <- function(x) {
    ww <- tryCatch(RODBC::odbcGetInfo(x),
                   error = function(e) e)
    invisible(ww)
}

#' Tablas base de datos
#' @description Nombres de las tablas en la base de datos.
#' @details Simplifica el resultado de llamar la función
#'     \code{sqlTables} de RODBC
#' @param x objeto RODBC
#' @param msg cuántas tablas? TRUE por defecto
#' @return character o NULL
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' db_tablas(op)
#' db_close(op)
#' @export
db_tablas.RODBC <- function(x, msg = TRUE) {
    xx <- tryCatch(RODBC::sqlTables(x, tableType = "TABLE"),
                   error = function(e){
                       message("\n!!! ERROR")
                       NULL})
    if (!is.null(xx)){
        xx <- xx[["TABLE_NAME"]]
        if (msg){
            message("\n tablas: ", length(xx))
        }
    }
    xx
}

#' Tabla existe
#' @description existe tabla en base de datos?
#' @param x objeto RODBC
#' @param tb character: nombre de tabla
#' @return logical
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' tabla.exists(op, "mydata")
#' tabla.exists(op, "data")
#' db_close(op)
#' @export
tabla_exists.RODBC <- function(x, tb = character()) {
    stopifnot("arg. tb inadmisible" = is_scalar_name(tb))
    is.element(tolower(tb), tolower(db_tablas(x)))
}

#' Nombre de columnas
#' @description Nombre de las columnas de una tabla en base de datos
#' @details Simplifica el resultado que devuelve la función
#'     \code{sqlColumns} de RODBC
#' @param x objeto RODBC
#' @param tb character: nombre de la tabla
#' @return character o NULL
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' db_columnas(op, "mydata")
#' db_close(op)
#' @export
db_columnas.RODBC <- function(x, tb = character()) {
    stopifnot("arg. tb inadmisible" = is_scalar_name(tb))
    xx <- tryCatch(RODBC::sqlColumns(x, tb),
                   error = function(e){
                       message("\n!!! ERROR")
                       NULL})
    if (!is.null(xx)) {
        xx <- xx[["COLUMN_NAME"]]
    }
    xx
}

#' Ejecutar consulta
#' @description Devuelve resultado de una consulta a la base de datos.
#' @details Captura los errores y el resultado de consultar la base de
#'     datos con la función \code{sqlQuery} de RODBC
#' @param x objeto RODBC
#' @param xs character: expresión SQL
#' @param strfac logical: las cadenas de caracteres en el resultado
#'     devueltas como factor? FALSE por defecto
#' @param max numeric: número máximo de registros en el resultado; 0,
#'     sin límite, por defecto
#' @param ... otros parámetros de \code{sqlGetResults}
#' @return data.frame (invisible) o el mensaje de error
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' ww <- db_qry(op, "select mgp, cyl from mydata")
#' db_close(op)
#' @export
db_qry.RODBC <- function(x, xs = character(), strfac = FALSE,
                         max = 0L, ...) {
    stopifnot("arg. xs inadmisible" = is_scalar(xs) &&
                  filled_char(xs) && nzchar(xs))
    ww <- tryCatch(
        RODBC::sqlQuery(x, xs, errors = TRUE, stringsAsFactors = strfac,
                 max = max, ...),
        error = function(e) e,
        message = function(e) e,
        warning = function(e) e)

    if (!is.data.frame(ww)){
        message("\n!!!...error durante lectura")
    }
    invisible(ww)
}

#' Leer tabla
#' @description Los datos en una tabla de la base de datos.
#' @details Captura los errores y el resultado de leer todos los
#'     campos de una tabla con \code{sqlFetch} de RODBC
#' @param x objeto RODBC
#' @param tb character: nombre de la tabla
#' @param strfac logical: las cadenas de caracteres en el resultado
#'     devueltas como factor? FALSE por defecto
#' @param max numeric: número máximo de registros en el resultado; 0,
#'     sin límite, por defecto
#' @param ... otros parámetros de \code{sqlGetResults} o
#'     \code{sqlQuery}
#' @return data.frame (invisible) o el mensaje de error
#' @examples
#' ox <- db_xcl(system.file("demofiles/mydata.xlsx", package =
#' "XLConnect"))
#' op <- db_open(ox)
#' ww <- db_fetch(op, "mydata")
#' db_close(op)
#' @export
db_fetch.RODBC <- function(x, tb = character(), strfac = FALSE,
                           max = 0L, ...) {
    stopifnot("arg. tb inadmisible" = is_scalar_name(tb))

    ww <- tryCatch(RODBC::sqlFetch(x, tb, stringsAsFactors = strfac,
                            max = max, ...),
                   error = function(e) e)

    if (!is.data.frame(ww)){
        message("\n!!!...error durante lectura")
    }
    invisible(ww)
}

#' Guardar tabla
#' @description Guarda un data frame como una tabla de la base de
#'     datos mediante \code{RODBC::sqlSave}
#' @param x ojeto RODBC
#' @param df data.frame
#' @param tb character: nombre de la tabla
#' @param rn logical: agrega nombre de filas a la tabla? FALSE por
#'     defecto
#' @param ... adicionales para \code{RODBC::sqlSave}
#' @return NULL o mensaje de error
#' @examples
#' fi <- system.file("demofiles/mydata.xlsx", package =
#' "XLConnect")
#' ox <- db_xcl(fi)
#' op <- db_open(ox)
#' ww <- data.frame(x = 1:3, y = 3:1)
#' ##NOT RUN !!
#' # db_save(op, ww, "mydata")
#' db_close(op)
#' @export
db_save.RODBC <- function(x, df, tb = character(),
                          rn = FALSE, ...) {
    stopifnot("arg. df inadmisible" = inherits(df, "data.frame"),
              "arg. tb inadmisible" = is_scalar_name(tb))

    ww <- tryCatch(RODBC::sqlSave(x, df, tablename = tb,
                                  rownames = rn, ...),
                   error = function(e) e)
    if (inherits(ww, "try-error")){
        message("\n!!!... ERROR")
    }
    invisible(ww)
}

#' Eliminar tabla
#' @description elimina una tabla de la base de datos mediante
#'     \code{sqlDrop}
#' @param x objeto RODBC
#' @param tb character: nombre de la tabla
#' @return NULL o mensaje de error
#' @keywords internal
db_drop.RODBC <- function(x, tb = character()) {
    stopifnot("arg. tb inadmisible" = is_scalar_name(tb))

    ww <- tryCatch(RODBC::sqlDrop(x, df),
                   error = function(e) e)
    if (inherits(ww, "try-error")){
        message("\n!!!... ERROR")
    }
    invisible(ww)
}

#' Guardar-excel
#' @description Escribir data.frame en archivo excel
#' @details Utiliza funciones de la librería «openxlsx» para guardar
#'     un data.frame en archivo excel. No es permitido escribir sobre
#'     celdas con datos.
#' @param x data.frame
#' @param archivo character: archivo excel
#' @param hoja character: hoja del archivo donde se va escribir. Si no
#'     existe, se agrega al libro excel. Si no se pasa un nombre, se
#'     agrega una nueva de nombre "Hojaxx", donde "xx" son números.
#' @param sobre_hoja logical: indicar que acepta escribir en una hoja
#'     que ya existe. FALSE por omisión.
#' @param columna integer: número de la columna de la celda superior
#'     izquierda de la tabla donde se guardarán los datos
#' @param fila integer: número de la fila
#' @param titulo character: una línea de título
#' @param el_dia logical: Escribir una celda con la fecha y hora
#'     cuando se escribieron los datos?. TRUE por omisión
#' @return nada
#' @export
guardar_excel <- function(x, archivo, hoja = "", sobre_hoja = FALSE,
                          fila = 1L, columna = 1L, titulo = character(0),
                          el_dia = TRUE) {
    ## verificar que sea hoja válida
    ## verificar hoja existe
    ## anunciar que se sobrescribirán datos
    ## opción para sobrescribir?
    if (file.exists(archivo)) {
        wb <- openxlsx::loadWorkbook(archivo)
        sh <- openxlsx::sheets(wb) %>% tolower()
        if (nzchar(hoja)) {
            if ( is.element(tolower(hoja), sh) ) {
                warning("!!! hoja excel «", hoja, "» ya EXISTE",
                        call. = FALSE)
                stopifnot("sobrescribir-hoja" = sobre_hoja)
            } else {
                openxlsx::addWorksheet(wb, hoja)
            }
        } else {
            nh <- length(sh) + 1L
            hoja <- paste0("Hoja", nh)
            while (is.element(hoja, sh)) {
                nh <- nh + 1L
                hoja <- paste0("H", nh)
            }
            openxlsx::addWorksheet(wb, hoja)
        }
    } else {
        wb <- openxlsx::createWorkbook( )
        if (nzchar(hoja)) {
            openxlsx::addWorksheet(wb, hoja)
        } else {
            hoja <- 1L
        }
    }

    if (el_dia) {
        ss <- format(Sys.time(), "%d.%b%Y:%I:%M%p")
        openxlsx::writeData(wb, hoja, ss, columna, fila)
        fila <- fila + 1L
    }

    if (filled_char(titulo)) {
        openxlsx::writeData(wb, hoja, titulo, columna, fila)
        fila <- fila + 1L
    }

    openxlsx::writeDataTable(wb, hoja, x, columna, fila)
    openxlsx::saveWorkbook(wb, archivo,
                           overwrite = TRUE, returnValue = TRUE)
}

#' @title Quita hoja excel
#' @description Elimina hoja de libro excel
#' @param arch character: nombre del archivo
#' @param hoja character: nombre de la hoja que se eliminará
#' @return character
#' @export
quitar_hoja_excel <- function(arch, hoja) {
    stopifnot("args. inadmisibles" = file.exists(arch) &&
                  filled_char(hoja) && nzchar(hoja))
    wb <- openxlsx::loadWorkbook(arch)
    if (en(hoja, openxlsx::sheets(wb))) {
        openxlsx::removeWorksheet(wb, hoja)
        openxlsx::saveWorkbook(wb, arch, overwrite = TRUE)
    } else {
        message("... hoja NO existe !!!")
    }
    hoja
}

##--- expresiones SQL ---

#' SQL-expresión
#' @description Construye expresión SQL
#' @details Los nombres de las tablas y los campos respectivos
#'     involucradas en la expresión, se especifican en una lista
#'     compuesta de tantas listas como tablas intervienen en la
#'     expresión. La lista de cada tabla está compuesta de dos
#'     vectores tipo character: el primero es el nombre de la tabla;
#'     el segundo, el de los campos tomados en cuenta. Si el primero o
#'     el segundo tienen sus elementos nombrados, estos nombres se
#'     utilizarán como "alias" en el caso de la tabla, o para
#'     renombrar el campo en el resultado de la consulta (en la
#'     cláusula «as»). Si sólo hay una tabla involucrada, las listas
#'     pueden ser sustituidas por un vector.
#'
#'     Las cláusulas «where», «join» (inner, left, right), «order by»,
#'     «order by», son opcionales y se especifican como argumentos de
#'     los correspondientes parámetros. Estas se deben introducir con
#'     la sintaxis correcta pues la función no valida eso.
#'
#'     Si la expresión contiene un «join», se requieren dos tablas, y
#'     la cláusula «on» es completada por el argumento al parámetro
#'     "whr". El tipo de «join» se puede indicar sólo con las primeras
#'     letras; e.g in(nner)
#' @param x lista de lista de character
#' @param whr character: cláusula where
#' @param ord character: cláusula order by
#' @param gby character: cláusula group by
#' @param joi character: cláusula join
#' @return character
#' @export
#' @examples
#' xsql(list(list("tab", c("cm1", "cm2"))))
#' ##-> "select cm1, cm2 from tab"
#' xsql(list(list("tab", c(x = "cm1", y = "cm2"))))
#' ##-> "select cm1 as x, cm2 as y from tab"
#' xsql(list(list(a = "tab", c("cm1", "cm2"))))
#' ##-> "select a.cm1 as x, a.cm2 as y from tab a"
#' tt <- list(list(a = "ta", c(w = "a", x = "b")),
#'            list(b = "tb", c(y = "c", z = "d")))
#' xsql(tt, joi = "in", whr = "a.a=b.c")
#' ##-> "select a.a as w,a.b as x, b.c as y, b.d as z ...
#' ##    from ta a, tb b inner join on a.a=b.c"
xsql <- function(x = list(), whr = character(), ord = character(),
                 qby = character(), joi = character()) {
    stopifnot(exprs = {
        "arg. x inadmisible" = filled_list(x) &&
                                all(sapply(x, length) == 2)
        "arg. whr inadmisible" = is.character(whr) &&
            is_scalar0(whr)
        "arg. ord inadmisible" = is.character(ord) &&
            is_scalar0(ord)
        "arg. qby inadmisible" = is.character(qby) &&
            is_scalar0(qby)
        "arg. joi inadmisible" = is.character(joi) &&
            is_scalar0(joi)
    })

    if (filled_char(joi)) {
        cc <- paste(c("inner", "left", "right"), "join")
        joi <- cc[pmatch(joi, cc)]
        stopifnot("join 2 tablas" = is.list(x) && length(x) == 2,
                  "arg. joi inadmisible" = !is.na(joi),
                  "arg. whr si join" = filled_char(whr))
        joi <- paste(joi, "on", whr)
    }

    tbc <- function(x) {
        cm <- x[[2]]
        nx <- names(cm)
        if (filled(nx)) {
            cm <- paste(cm, nx, sep = " as ")
        }

        nx <- names(x)
        tb <- x[[1]]
        if (filled(nx)) {
            cm <- paste(nx[1], cm, sep = ".")
            tb <- paste(tb, nx[1])
        }

        x[[1]] <- tb
        x[[2]] <- paste(cm, collapse = ",")
        x
    }

    rdu <- function(x, y) {
        c(paste(x[[1]], y[[1]], sep = ","),
          paste(c(x[[2]], y[[2]]), collapse = ","))
    }
    x <- lapply(x, tbc) %>%
        Reduce(rdu, ., init = list("", "")) %>%
        substring(2)

    ss <- paste("select", x[2], "from", x[1])

    ## condiciones
    if (is_scalar(joi)) {
        ss <- paste(ss, joi)
    } else {
        if (filled(whr) && nzchar(whr)) {
            ss <- paste(ss, "where", whr)
        }
    }

    if(filled(ord) && nzchar(ord)) ss <- paste(ss, "order by", ord)
    if(filled(qby) && nzchar(gby)) ss <- paste(ss, "group by", gby)

    ss
}

#' SQL expresión simple
#' @description Expresión SQL sencilla
#' @param x character: nombre de la tabla
#' @param cam character: nombre de los campos. Si los elementos con
#'     nombre, estos son puestos en la cláusula «as». Por omisión es
#'     "*".
#' @return character
#' @export
#' @examples
#' xsql_s("a", c(x = "a", y = "b"))
#' #-> "select a as x,b as y from a"
#' xsql_s("a")
#' #-> "select * from a"
xsql_s <- function(x = character(), cam = "*") {
    stopifnot("arg. inadmisibles" = is_scalar_name(x) &&
                  filled_char(cam))

    if (cam[1] != "*") {
        nm <- names(cam)
        if (filled(nm)) {
            cam <- paste(cam, nm, sep = " as ")
        }
        cam <- paste(cam, collapse = ",")
    }
    paste("select", cam, "from", x)
}

#' SQL union
#' @description Construye la expresión SQL "union"
#' @details Con «union» se juntan (al modo de un \code{rbind}) en un
#'     solo conjunto de datos los resultados de varias consultas que
#'     producen columnas respectivas del mismo tipo. Para identificar
#'     en el resultado final los registros obtenidos con cada
#'     consulta, se puede agregar a cada una un campo de valor
#'     "constante", el cual aparecerá en el data.frame cuando se mande
#'     ejecutar la consulta. La función brinda esa facilidad por medio
#'     de los parámetros "idc" y "cid"
#'
#'     La expresión «union» tiene dos variantes: «union» que no
#'     devuelve registros que se repiten, y «union all» que incluye a
#'     todos.
#' @param x character: vector con las expresiones SQL
#' @param idc character o numeric: sus elementos sirven para etiquetar
#'     los resultados de cada consulta. Es opcional; por omisión,
#'     secuencia de enteros de longitud igual a la de x
#' @param cid character: nombre de la columna del conjunto de datos,
#'     que alojará las etiquetas de las consultas. Es opcional; por
#'     omisión, "con"
#' @param all logical: \code{union all}?; TRUE por omisión
#' @return character
#' @seealso xsql_t
#' @export
#' @examples
#' cn <- c("select x, y from tx where y = 'a'",
#'         "select x, z from tx where z = 'b'")
#' xsql_u(cn, idc = c(1, 2), cid = "set")
#' #-> "select x, y, set = 1 from tx where y = 'a' union all(...
#'      select x, z, set = 2 from tx where z = 'b')
#' # todos los registros que produzca la primera consulta llevarán
#' # "1" en la columna "set" del data.frame que resulte de hacer la
#' # consulta a la base de datos, y todos los producidos por la segunda
#' # llevarán "2". Se supone que "y" y "z" son de igual tipo.
xsql_u <- function(x = character(), idc = seq_along(x),
                   cid = "con", all = TRUE) {
    stopifnot(exprs = {
        "arg. x inadmisible" = filled_char(x) && !is_scalar(x)
        "arg. idc inadmisible" = is.character(idc) || is.numeric(idc)
        "arg. idc,cid inadmisible" = ifelse(is_vacuo(idc),
                                            is_vacuo(cid),
                                            is_scalar_name(cid) &&
                                            length(idc) == length(x))
    })

    ## agrega constantes
    if (filled(idc)) {
        if (filled_char(idc)) {
            idc <- sQuote(idc)
        }
        ac <- function(z) {
            cc <- paste0("\\1,", cid, "=", idc[z], "\\2")
            sub("(.+)(\\s+from.+)", cc, x[z], perl = TRUE)
        }
        x <- vapply(seq_along(x), ac, "a", USE.NAMES = FALSE)
    }

    cc <- ifelse(all, " union all(", " union(")
    Reduce(function(x,y){paste0(x, cc, y)}, x) %>%
        paste0(strrep(")", length(x) - 1)) #strrep R > 3.0.0
}

#' SQL expresión matriz
#' @description Expresión SQL para leer campos que forman una matriz
#'     de datos
#' @details La mayoría de las tablas donde se almacenan los datos de
#'     las encuestas son "planas" (todas las variables aparecen como
#'     un campo de la tabla), aunque para la estimación y el análisis,
#'     los datos se arreglan en un cuadro con la siguiente estructura
#'     (c??? es el nombre del campo)
#'
#'     cxx1 cxx2 cxx3 cxx4
#'
#'     cxx5 cxx6 cxx7 cxx8
#'
#'     .... .... .... ....
#'
#'     o
#'
#'     cxx1 cxx3 cxx5 cxx7
#'
#'     cxx2 cxx4 cxx6 cxx8
#'
#'     donde las columnas del cuadro son las variables (p.ej. cultivo,
#'     manzanas sembradas, etc.) y las filas corresponden a un
#'     registro o cuestionario.
#'
#'     Cada fila se lee de la base de datos por medio de una
#'     subconsulta SQL, y todas se combinan en una sola con la
#'     cláusula 'union all', para reunir (con una especie de
#'     \code(rbind)) a todos los data.frame que devuelven las
#'     subconsultas. Para que esto funcione, además del nombre de los
#'     campos (parámetro "cam"), la función pide el nombre común que
#'     tendrán las columnas o variables (parámetro "nvb") de los
#'     data.frame. Por fuerza, el número de campos
#'     (\code{length(cam)}) debe ser múltiplo del número de columnas
#'     (\code{length(nvb)}).
#'
#'     Es opcional agregar una variable que contenga una etiqueta para
#'     identificar cada uno de los data.frame. El parámetro "cid"
#'     recibiría el nombre de esa columna y el parámetro "idc" el de
#'     las etiquetas, que deberían ser tantas como los data.frame
#'     parciales. En consecuencia se debería satisfacer la condición
#'     \code{length(cam) == length(nvb) * length(idc)}.
#'
#'     Para fines de comprobación, cada fila de los data.frame trae el
#'     «id» del registro de la tabla (parámetro "idr", que
#'     generalmente es el número de cuestionario) de donde se leyeron
#'     los datos.
#'
#' @param x character: nombre de la tabla de la base de datos que se
#'     va a consultar
#' @param cam character: nombre de los campos
#' @param nvb character: nombre de las columnas que tendrá el
#'     data.frame
#' @param idr character: nombre del campo de la tabla con el "id" de
#'     los registros. Por omisión, "quest".
#' @param xfi logical: la secuencia de los datos que corresponden a
#'     los campos es por fila (TRUE, por defecto) o por columna
#'     (FALSE).
#' @param idc números (integer) o nombres (character) que van
#'     etiquetar los registros producidos por cada consulta. Es
#'     opcional.
#' @param cid character: nombre de la columna que llevará la etiqueta
#'     de las consultas. Es opcional, pero atado al parámetro ("idc").
#' @param all logical: incluir todos los registros (TRUE por defecto,
#'     cláusula «union all») o no permitir duplicados (FALSE, cláusula
#'     «union»).
#' @return character
#' @seealso xsql_u, normalizar_data
#' @examples
#' xsql_t("segene", paste0("c", 121:126), c("cult", "semb", "per"))
#' @export
xsql_t <- function(x = character(), cam = character(),
                   nvb = character(), idr = "quest", xfi = TRUE,
                   idc = integer(), cid = character(), all = TRUE) {

    stopifnot(exprs = {
        "arg. x inadmisible" = is_scalar_name(x)
        "arg. cam inadmisible" = is_name(cam)
        "arg. nvb inadmisible" = is_name(nvb)
        "arg. cam no mult. nvb" = length(cam) %% length(nvb) == 0
        "arg. cid inadmisible" = is_vacuo(cid) || is_scalar_name(cid)
        "arg. idc inadmisible" = (is_vacuo(idc) && is_vacuo(cid)) ||
            ((filled_int(idc) || filled_char(idc)) && filled(cid) &&
             length(cam) == length(nvb) * length(idc))
        "arg. idr inadmisible" = is_scalar_name(idr)
    })

    cn <- c(idr, nvb)
    mk <- matrix(cam, ncol = length(nvb), byrow = xfi) %>%
        cbind(idr, .) %>%
        magrittr::set_names(cn) %>%
        apply(1, function(z) xsql_s(x, cam = setNames(z, cn)))


    #ss <- split(mk, seq_len(nrow(mk))) %>%
    #ss <- apply(mk, 1, function(z)xsql_s(x, cm = setNames(z, cn)))
    #           USE.NAMES = FALSE)

    xsql_u(mk, idc, cid, all)
}

#' SQL expresión con o sin union
#' @description Llama «xsql_s» o «xsql_t» para construir la expresión
#'   SQL. Asume que el nombre del campo que identifica los registros
#'   es "quest".
#' @seealso \code{xsql_t}, \code{xsql_s}
#' @param x list: con los nombres de los parámetros y los argumentos
#'   que serán pasados a la correspondiente función. El primer
#'   elmento, con nombre «x», indica cuál de las funciones: 1 para
#'   llamar «xsql_s», 2 para llamar «xsql_t».
#' @param tabla character: nombre de la tabla de donde se extraerán
#'   los datos
#' @return character: expresión SQL
#' @examples
#' xsql_st(list(x = 1, cam = c("c5000", "copiade")),
#'         tabla = "seguimjul2022")
#' xsql_st(list(x = 2, cam = c("c001", "c002", "c003", "c004"),
#'         nvb = c("cul", "mz")), tabla = "seguimjul2022")
#' xsql_st(list(x = 2, cam = c("c001", "c002", "c003", "c004"),
#'         nvb = c("cul", "mz"), idc = c("mai", "fri"),
#'         cid = "cultivo"), tabla = "seguimjul2022")
#' @export
xsql_st <- function(x, tabla) {
    tip <- x[[1]]
    stopifnot("arg. no válidos" = is.character(tabla) &&
                  nzchar(tabla) && (tip == 1 | tip == 2))
    x[[1]] <- tabla
    if (tip == 1) {
        nm <- names(x[[2]])
        if (is.null(nm)) {
            x[[2]] <- append(x[[2]], "quest", 0)
        } else {
            x[[2]] <- append(x[[2]], c(quest = "quest"), 0)
        }
        xs <- do.call("xsql_s", x)
    } else {
        xs <- do.call("xsql_t", x)
    }
    xs
}

## === obtener datos de la base de datos ===

#' Datos-encuesta
#' @description Devuelve los datos que produce una consulta SQL, y de
#'     manera opcional, filtra los datos no igual a cero, remplaza NA
#'     por ceros y atribuye metadatos.
#' @param x objeto odb
#' @param qstr character: expresión de consulta SQL
#' @param meta character: metadatos
#' @param sin_0 logical: filtrar los registros con datos diferentes de
#'     0? FALSE por defecto
#' @param na_0 logical: convierte columnas numéricas de NA a cero?
#'     FALSE por defecto
#' @param max numeric: número máximo de registros en el
#'     resultado. Todos por defecto.
#' @param ... argumentos adicionales para la función quitar_0
#' @return data.frame o NULL
#' @examples
#' fi <- system.file("demofiles/multiregion.xlsx", package = "XLConnect")
#' ob <- db_xcl(fi)
#' ww <- get_data(ob, "select year, days from calendar where year < 2002")
#' @export
get_data.odb <- function(x, qstr = character(), meta = character(),
                         sin_0 = FALSE, na_0 = FALSE, max = 0, ...) {
    stopifnot("arg. qstr inadmisible" = filled_char(qstr) &&
                  is_scalar(qstr) && nzchar(qstr),
              "arg. meta inadmisible" = is.character(meta),
              "arg. max inadmisible" = filled_num(max) && is_scalar(max)
              )

    kk <- db_open(x)
    if (ok <- is_rodbc(kk)) {
        ww <- db_qry(kk, qstr, max = max)
        db_close(kk)

        if (ok <- is.data.frame(ww)) {
            if (na_0) {
                ww[] <- lapply(ww, na0)
            }
            if (sin_0) {
                ww <- quitar_0(ww, ...)
            }
            if (filled(meta)) {
                attr(ww, "meta") <- meta
            }
        }
    }
    if (!ok) ww <- NULL

    invisible(ww)
}

#' Datos-encuesta
#' @description Devuelve los datos que produce una consulta SQL, y de
#'     manera opcional, filtra los datos no igual a cero, remplaza NA
#'     por ceros y atribuye metadatos.
#' @param x objeto odb
#' @param qstr character: expresión de consulta SQL
#' @param meta character: metadatos
#' @param sin_0 logical: filtrar los registros con datos diferentes de
#'     0? FALSE por defecto
#' @param na_0 logical: convierte columnas numéricas de NA a cero?
#'     FALSE por defecto
#' @param max numeric: número máximo de registros en el
#'     resultado. Todos por defecto.
#' @param ... argumentos adicionales para la función quitar_0
#' @return data.frame o NULL
#' @keywords internal
#' @examples
#' fi <- system.file("demofiles/multiregion.xlsx", package =
#' "XLConnect")
#' ob <- db_xcl(fi)
#' ww <- get_data(ob,
#'          "select year, days from calendar where year < 2002")
get_dat.odb <- function(...) {
    message("Use get_data" )
}


## -- csentry --

#' Parámetros-conexión-RMariaDB
#' @description Construye lista con los parámetros necesarios para
#'     conectarse a una base de datos MySQL.
#' @details Inicia la lista con el valor de las "variables ambiente"
#'     MYSQLSERVERMAG, MYSQLDB, MYSQLUID, MYSQLPWD, y la modifica con
#'     los pasados como argumentos. Mensaje de alerta si alguno
#'     "vacío".
#' @param ... character: admisibles: "host", "dbname", "user", "pwd"
#' @return list
#' @export
#' @examples
#' par_conn()
#' par_conn(dbname = "cspro")
#' par_conn(user = "pepe", dbname = "data", pwd = "mefistofeles")
par_conn_mysql <- function(...) {
    x <- list(host = Sys.getenv("MYSQLSERVERMAG"),
              dbname = Sys.getenv("MYSQLDB"),
              user = Sys.getenv("MYSQLUID"),
              pwd = Sys.getenv("MYSQLPWD"))

    z <- list(...)
    if (length(z) > 0) {
        mm <- match(names(x), names(z))
        no_na <- !is.na(mm)
        if (any(no_na)) {
            x[no_na] <- z[mm[no_na]]
        }
    }

    if (!all(sapply(x, nzchar))) {
        warning("... hay parámetros no definidos !!!", call. = FALSE)
    }

    x
}

#' Conectar MySQL
#' @description Inicia la conexión con la base de datos MySQL
#' @details Si alguno de los argumentos no está en ..., se toma de
#'     "variables de ambiente"
#' @seealso par_conn_mysql
#' @param ... character. Argumentos a la función "dbConnect": host,
#'     dbname, user, password.
#' @return objeto DBI o NULL (si la conexión no es válida)
#' @export
#' @examples
#' cn <- conn_mysql()
#' RMariaDB::dbListTables(cn)
#' RMariaDB::dbListFields(cn, "hato_dict")
#' RMariaDB::dbDisconnect(cn)
conn_mysql  <- function(...) {

    x <- par_conn_mysql(...)

    con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                               host = x$host, dbname = x$dbname,
                               user = x$user, password = x$pwd)
    if (!DBI::dbIsValid(con)) {
        message("\n... ERROR de conexión !!!")
        con <- NULL
    }

    invisible(con)
}

#' Valida conector con base de datos
#' @description El objeto que permite acceso a base de datos es
#'     válido?
#' @param cn objeto conector con base de datos
#' @return logical
#' @export
conn_valido <- function(cn) {
    (!is.null(cn)) && DBI::dbIsValid(cn)
}

#' Cierra conexión base de datos MySql
#' @param x objeto: Conexión base datos MySql
#' @export
close_mysql <- function(x) {
    if (inherits(x, "MariaDBConnection") && conn_valido(x)) {
        RMariaDB::dbDisconnect(x)
    }
}

#' Leer diccionario de datos Cspro
#' @description Leer el diccionario de una base de datos de Cspro,
#'     almacenado en una hoja de Excel
#' @details El diccionario está formado por 3 columnas con los
#'     nombres: variable (llenada con los nombres de las variables del
#'     cuestionario), start (posición del primer caracter del dato
#'     correspondiente a la variable) y length (número de caracteres
#'     que componen el dato)
#' @param xlsx character: nombre del archivo excel
#' @param filas integer: número de filas que ocupa el diccionario en
#'     la hoja excel. Si es un escalar, se supone que las filas van
#'     desde la número 1 hasta la fila especificada en el número
#'     pasado como argumento
#' @param columnas integer: número de columnas que ocupa el
#'     diccionario
#' @param hoja integer: número de la hoja que contiene el
#'     diccionario. Por omisión, la primera hoja
#' @return data.frame con los nombres de variables en minúscula.
#' @examples
#' \donttest{
#'     leer_dic_dat_xlsx("datos/hato-dic.xlsx", 534, 2:4)
#' }
#' @export
leer_dic_dat_xlsx <- function(xlsx, filas, columnas, hoja = 1L) {
    if ( length(filas) == 1 ) filas <- seq_len(filas)

    dic <- openxlsx::read.xlsx(xlsx, hoja,
                               rows = filas,
                               cols = columnas)
    names(dic) <- tolower(names(dic))
    dic["variable"] <- tolower(dic$variable)
    dic
}

## ' ADVERTENCIA. Cuando el ingreso de los datos se corta antes del
## ' último (por ejemplo cuando es no agrícola) la cadena de caracteres
## ' llega hasta donde se ingresó el último dato. Para la lectura con
## ' read.fwf esto es un inconveniente porque aquellas variables que
## ' están después del último ingresado, se devuelven como character con
## ' "'NA'" (no se "leen" porque el registro se queda corto. Puede ser
## ' que el registro sólo tenga, digamos, 80 caracteres, y la variable
## ' busca leerse a partir del caracter 81 o después). Por otra parte,
## ' si se pasan argumentos al parámetro colClasses, se genera un error
## ' y se detiene el proceso de lectura cuando trata de leer datos que
## ' no existen.
## '
## ' Una opción es preprocesar el archivo y completar el registro con
## ' caracteres "en blanco", según el número de caracteres especificados
## ' en el diccionario de datos. La opción de leer y procesar después
## ' para ajustar ("'NA'" convertidos a NA y modificar el tipo de la
## ' variable que fue devuelta como character) demandaría más código por
## ' los ajustes.
## '
## ' Con esa opción, habría que pasar TRUE al parámetro strip.white y ""
## ' a na.strings, para que devuelva NA y no "NA"


#' Cspro. Ajustar longitud de los registros
#' @description Completar la longitud de los registros del campo
#'     "questionnare".
#' @details Parece que cuando el ingreso de los datos se corta antes
#'     del último definido en el diccionaro de datos, la cadena de
#'     caracteres que forma un registro del campo "questionnaire"
#'     llega hasta donde se ingresó el último. Para la lectura con
#'     read.fwf, de los datos exportados a un archivo tipo texto, esto
#'     es problema porque aquellas variables que están después del
#'     último ingresado, se devuelven como character con "'NA'" (no se
#'     "leen" porque el registro se queda corto. Puede ser que el
#'     registro sólo tenga, digamos, 80 caracteres, y la variable
#'     busca leerse a partir del caracter 81 o después). Además, si se
#'     pasan argumentos al parámetro colClasses de read.fwf, se genera
#'     un error y se detiene el proceso de lectura cuando trata de
#'     leer datos que no existen.
#'
#'     La función completa los registros con espacios al final,
#'     conforme a las especificaciones en el diccionario.
#' @param x character: vector con los registros leídos
#' @param df_dic: data.frame con los datos del diccionario
#' @return data.frame
#' @export
ajustar_lon_reg_cs <- function(x, df_dic) {
    nc <- sum(df_dic$length)

    nn <- vapply(x, nchar, 1L, USE.NAMES = FALSE)

    if ( any(nn != nc) ) {
        warning("\n!!! Hay registros que discrepan con el diccionario" )
    }

    nr <- (nc - nn) %>%
        is_greater_than(0) %>%
        which()

    if ( length(nr) > 0 ) {
        sp <- rep(" ", length.out = nc) %>%
            paste0(collapse = "")

        for ( k in nr ) {
            x[k] <- paste0(x[k], substring(sp, nn[k] + 1L, nc))
        }
    }
    invisible(x)
}

#' Cuantos caracteres ocupan las variables en registros Cspro
#' @description Construye el vector que se pasa como argumento del
#'     parámetro "loncam" de la función get_data_cspro
#' @details Los elementos del vector indican el número de caracteres
#'     que ocupa una variable dentro de la cadena de caracteres de un
#'     registro, o bien el número de caracteres (número negativo) que
#'     hay entre dos variables que no están contigüas en el registro.
#'
#'     El parámetro "dic" es un data.frame con las columnas "variable"
#'     y "length" tomadas del diccionario de datos de la tabla de
#'     Cspro.
#' @seealso get_data_cspro, read.fwf
#' @param x character: nombres de las variables
#' @param dic data.frame con los nombres de las variables y las
#'     correspondientes longitudes
#' @return integer
#' @export
#' @examples
#' dd <- data.frame(variable = c("boleta", "nombre", "direccion"),
#'                  length = c(5, 50, 100))
#' longitud_variables(c("boleta", "direccion"), dd) #-> c(5, -50, 100)
longitud_variables <- function(x = character(), dic) {
    stopifnot("arg. x inadmisible" = is.character(x) && length(x))

    x <- ordenar_conforme(x, dic$variable)

    m <- match(x, dic$variable) %>% Filter(Negate(is.na), .)
    if (length(m) < length(x)) {
        warning("\n... hay variables que no están en diccionario !!!")
    }
    k <- seq.int(min(m), max(m))

    z <- dic$variable[k]
    y <- dic$length[k]

    ## variables enmedio excluidas ("saltar")
    ## elementos correspondientes a negativo
    i <- z %in% x
    y[!i] <- -y[!i]

    ## acumula rachas de negativos
    ## y pone a 0 todos menos el acumulado
    n <- length(y)
    cum <- 0L
    while(n > 0) {
        if (y[n] > 0) {
            cum <- 0L
        } else {
            if (cum < 0) {
                y[n + 1] <- 0L
            }
            cum <- cum + y[n]
            y[n] <- cum
        }
        n <- n - 1
    }

    y <- y[y != 0L]

    ## variables que "saltar" al inicio
    if (k[1] > 1) {
        cum <- cumsum(dic$length[seq_len(k[1] - 1)])
        y <- purrr::prepend(y, -cum)
    }

    y
}

#' Exportar datos cspro
#' @description Exporta los datos "limpios" a un archivo tipo texto.
#' @details Lee los datos de la base en MySQL, ajusta la longitud de
#'     los registros, elimina los registros marcados como borrados y
#'     los que tienen datos duplicados en la variable "quest", bajo el
#'     supuesto de que el último registro es el más reciente y es el
#'     "correcto".
#' @param tab character: nombre de la tabla en la base de datos MySQL
#' @param dic data.frame: los campos del diccionario de datos
#' @param artx character: nombre del archivo de salida. Por omisión,
#'     un archivo temporal con extensión ".txt" y "cs" en primeros
#'     caracteres.
#' @param sin_duplicados logical: TRUE por omisión
#' @param cn objeto para tener acceso a la base de datos. En el caso
#'     de que sea \code{NULL} (valor por defecto) la función intenta
#'     generar un objeto válido a partir de los parámetros de la
#'     conexión tomados de variables de ambiente.
#' @return character: nombre del archivo o NULL si no se logró
#'     exportar
#' @seealso conn_mysql, par_con_mysql, ajustar_lon_reg_cs
#' @examples
#' \donttest{
#'      dic <- leer_dic_dat_xlsx("datos/hato-dic.xlsx", 534, 2:4)
#'      exportar_datos_cs("cspro", dic = dic)
#' }
#' @export
exportar_datos_cs <- function(tab, dic,
                              artx = tempfile("cs",
                                              fileext = ".txt"),
                              sin_duplicados = TRUE,
                              cn = NULL) {
    stopifnot("archivo ya existe" = !file.exists(artx),
              "no puedo crear archivo" = ok_fname(artx))

    no_my_sql_con <- !inherits(cn, "MySQLConnection")
    if (no_my_sql_con) {
        cn <- conn_mysql()
    }

    if (!conn_valido(cn)) {
        warning("\n!!! No es válida la conexión a la base de datos")
        return(NULL)
    }

    del <- leer_campo_cspro(tab, "deleted", conn = cn) %>%
        extract2(1) %>%
        magrittr::equals(1L) #una igual en testthat

    txt <- leer_campo_cspro(tab, "questionnaire", conn = cn) %>%
        filter(deleted == 0) %>%
        extract2(1)

    qst <- leer_campo_cspro(tab, "id_QUEST", conn = cn) %>%
        filter(deleted == 0) %>%
        extract2(1)

    if (no_my_sql_con) close_mysql(cn)

    txt <- ajustar_lon_reg_cs(txt, dic)

    nr <- seq.int(length(qst), 1L)
    dr <- 0L
    if ( sin_duplicados && anyDuplicated(qst) ) {
        dr <- qst[nr] %>% duplicated()
        txt <- txt[!dr[nr]]
    }

    nn <- length(del)
    message("\n... número de registros leídos: ", nn)
    message("\n... número de registros borrados: ", sum(del))
    message("\n... número de registros duplicados: ", sum(dr))
    message("\n... número de registros exportados: ", length(txt))

    cat(txt, file = artx, sep = "\n")
    artx
}

#' Leer tabla con campos adyacentes de longitud fija
#' @description Leer datos de un archivo compuesto de líneas de texto
#'     con campos de longitud fija.
#' @details La función es una "wrap function" que utiliza la funcíón
#'     read.fwf, adaptada para leer los datos exportados de Cspro. El
#'     argumento al parámetro "dic" tiene las especificaciones del
#'     diccionario de datos que determinan la posición y el número de
#'     caracteres que ocupa cada variable en los registros.
#' @param variables character: nombre de las variables en diccionario
#'     de datos
#' @param columnas character: nombre de las columnas en el data.frame
#'     que será generado. Por omisión, igual a los nombres de las
#'     variables.
#' @param tipo_col character: tipo de los datos en las columnas.
#' @param dic data.frame: los datos del diccionario de datos
#' @param nomar character: nombre del archivo
#' @seealso read.fwf, exportar_datos_cs
#' @return invisible data.frame; NULL si error
#' @export
leer_datos_fwf <- function(variables = character(),
                           columnas = variables,
                           tipo_col = character(),
                           dic, nomar = character()) {

    loncam <- longitud_variables(variables, dic)
    w <- tryCatch(
        read.fwf(nomar, widths = loncam, col.names = columnas,
                 colClasses = tipo_col,
                 comment.char = "", strip.white = TRUE,
                 na.strings = "", stringsAsFactors = FALSE),
        error = function(e) {
            message("Error durante lectura !!!")
            NULL
            })

    invisible(w)
}

#' Leer y unir cuadros de datos
#' @description Lee los datos de varias variables y hace un «reshape»
#'     para organizarlos en un grupo de atributos comunes
#' @details Lee los datos en un archivo tipo «fwf», de varias
#'     variables relacionadas con un mismo grupo de atributos; separa
#'     las variables conforme a los atributos en común y las une con
#'     un rbind. La variable en arg. "idr" es la que identifica los
#'     registros (generalmente el número del cuestionario), y las
#'     demás deben ser, en número, un múltiplo del número de columnas
#'     (atributos en común) del data.frame resultante. En el ejemplo,
#'     las variables «c001» y «c003» refieren al atributo «cultivo», y
#'     las otras dos al atributo «precio».
#' @param idr numeric o character: nombre o número de la variable que
#'     identifica los registros. Por omisión es 1.
#' @param variables character: los nombres de las variables que se van
#'     a leer del archivo
#' @param tipo_col character: tipo de datos en las columnas
#'     "variables"
#' @param columnas character: nombres de los atributos en común que
#'     serán las columnas del data.frame resultante
#' @param col_nom_fi character: columna con nombres de filas. Opcional
#' @param nom_fi character: etiquetas de filas del cuadro. Opcional
#' @param dic data.frame: data.frame con los datos del diccionario de
#'     datos
#' @param nomar character: nombre del archivo donde están todos los
#'     datos
#' @return data.frame o NULL si error
#' @export
#' @examples
#' \donttest{
#' leer_cuadros_fwf(idr = "quest", c("c001", "c002", "c003", "c004"),
#'                  tipo_col = c("integer", "double"),
#'                  columnas = c("cultivo", "precio"),
#'                  dic = dicc, nomar = "arch.txt")}
leer_cuadros_fwf <- function(idr = 1L, variables, tipo_col, columnas,
                             col_nom_fi = character(0),
                             nom_fi = character(0),
                             dic, nomar) {

    stopifnot("falta arg. nom_fi" = (is_vacuo(col_nom_fi) &&
                  is_vacuo(nom_fi)) || (filled_char(col_nom_fi) &&
                  filled_char(nom_fi)))

    nv <- length(variables)
    nc <- length(columnas)
    cg <- seq_len(nv) + 1L

    ng <- nv %/% nc
    ok <- (nc * ng) == nv

    stopifnot("#variables no múltiplo de #columnas" = ok)

    if ( filled_char(nom_fi) ) {
        stopifnot( "chk. arg. nom_fi" = length(nom_fi) == ng )
    }

    variables <- c(idr, variables)
    if (length(tipo_col) == 1) {
        tipo_col <- rep(tipo_col, nv + 1L)
    }
    stopifnot("arg. tipo_col,variables" = length(tipo_col) == nv + 1L)

    x <- leer_datos_fwf(variables = variables,
                        columnas = variables,
                        tipo_col = tipo_col,
                        dic = dic, nomar = nomar)

    ## names(x)[cg] <- rep(columnas, length.out = nv)
    ## y <- split(cg, rep(seq_len(ng), each = nc)) %>%
    ##     purrr::map_df(function(r) x[, c(1, r)]) %>%
    ##     purrr::list_rbind( )

    if (is.null(x)) {
        z <- NULL
    } else {
        y <- normalizar_data(x, col_id = idr, vbl = columnas)

        ## el número de registros debe ser múltiplo
        if ( filled_char(nom_fi) ) {
            y[col_nom_fi] <- rep(nom_fi, length.out = nrow(y))
        }

        z <- purrr::map_df(y, na0) %>%
            quitar_0(excepto = idr)
    }

    invisible(z)
}

#' Campo-CSpro
#' @description Lee un campo de una base de datos construida con CSpro
#' @details Entre los argumentos del parámetro "..." puede estar un
#'     objeto de conexión a la base de datos (conn); si no está, se
#'     intentará establecer la conexión con los argumentos a los
#'     parámetros de la función conn_mysql
#' @seealso conn_mysql
#' @param tab_dict character: nombre de la tabla en la base de datos
#' @param campo character: nombre del campo
#' @param con_borrado logical: incluir el campo "deleted" en la
#'     consulta?. TRUE por omisión.
#' @param nreg numeric: número de registros a devolver. Por omisión,
#'     todos (nreg = -1)
#' @param ... argumentos para los parámetros de la función conn_mysql
#' @return data.frame o NULL si error en la conexión a la base de
#'     datos
#' @export
#' @examples
#' x <- par_conn_mysql()
#' leer_campo_cspro("hatodict", "id_Quest", host = x$host,
#'                   user = x$user, password = x$pwd,
#'                   dbname = x$dbname)
#'
#' cn <- conn_mysql()
#' leer_campo_cspro("hatodict", "id_Quest", conn=cn)
leer_campo_cspro <- function(tab_dict = character(),
                             campo = "questionnaire",
                             con_borrado = TRUE, nreg = -1, ...) {

    stopifnot("arg. tab_dict inválido" = is.character(tab_dict) &&
                  length(tab_dict) && nzchar(tab_dict))

    x <- list(...)

    conn_arg <- utils::hasName(x, "conn")
    if (conn_arg) {
        conn <- x$conn
    } else {
        conn <- conn_mysql(...)
    }

    if (!inherits(conn, "MySQLConnection")) {
        warning("\n!!! no es MySQLConnection" )
        return(conn)
    }

    if (con_borrado) campo <- paste0(campo, ",deleted")

    ss <- paste("select", campo, "from", tab_dict)
    w <- tryCatch(RMariaDB::dbGetQuery(conn, ss, n = nreg),
                  error = function(e) {
                      message("\n... ERROR durante lectura !!!")
                      NULL}
                  )

    if (!conn_arg) {
        RMariaDB::dbDisconnect(conn)
    }

    invisible(w)
}

#' Leer-CSpro
#' @description Leer los datos de las variables de una encuesta que ha
#'     sido digitada con CSpro o CSentry
#' @details En la base de datos de Cspro cada encuesta está almacenada
#'     en una sola tabla. El campo "questionnaire" trae los datos de
#'     las variables en el cuestionario de la encuesta. Cada elemento
#'     de ese campo corresponde a un cuestionario, y consiste de una
#'     sola ristra de caracteres dentro de la cual los datos de una
#'     variable ocupan una posición y un número de caracteres
#'     determinado, ambos definidos en el "diccionario de datos". Para
#'     cargar los datos en un data.frame, la función manda escribir
#'     cada elemento de "questionnaire" como una línea de un archivo
#'     temporal, y después lee la secuencia de caracteres asociada a
#'     los datos de cada variable, con la función read.fwf. Tener
#'     presente que cspro utiliza un campo al inicio (de longitud 1)
#'     para indicar el tipo de registro. Ver ayuda de read.fwf acerca
#'     del uso de longitudes negativas para "saltar" variables
#' @param tab_dict character: nombre de la tabla en la base de datos
#' @param dat_dict data.frame: diccionario de datos con las columnas
#'     variable y length (caracteres ocupados por la variable)
#' @param columnas character: nombre que se le asignarán a las
#'     variables en el resultado
#' @param clase_col character: tipo de vector de la variable
#'     (character, integer, real, ...). Opcional.
#' @param sin_borrado logical: excluye los registros marcados como
#'     borrados?. Por omisión, TRUE
#' @param nreg integer: número de registros a devolver. Por omisión,
#'     todos (nreg = -1)
#' @param ... character: Argumentos para establecer la conexión (host,
#'     dbname, userid, password) o la conexión si ya fue establecida
#'     (conn)
#' @seealso conn_mysql, par_conn_mysql
#' @return data.frame o NULL
#' @export
#' @examples
#' \donttest{
#' x <- get_data_cspro("caracterizacion_dict",
#'                     dat_dict = dicc,
#'                     columnas = c("reg", "quest", "tecnico",
#'                                  "copiade", "cx", "cy",
#'                                  "informante"),
#'                     conn = conn_mysql())
#' x <- get_data_cspro("caracterizacion_dict",
#'                     dat_dict = dicc,
#'                     columnas = c("quest", "tecnico", "copiade",
#'                                  "cx", "cy", "informante"),
#'                     host = Sys.getenv("host"), user = "eddy",
#'                     dbname = "bd", password = Sys.getenv("pwd"))}
get_data_cspro <- function(tab_dict = character(), dat_dict,
                           columnas = character(),
                           clase_col = character(),
                           sin_borrado = TRUE, nreg = -1, ...) {
    COLDAT <- "questionnaire"
    ## -- vale argumentos --
    ## ninguno "vacío"
    ## length(loncam) <= length(columnas)
    ## excepto los anteriores, los demás son escalares
    w <- leer_campo_cspro(tab_dict, COLDAT, nreg = nreg, ...)
    if (is.null(w)) return(w)

    if (sin_borrado) {
        w %<>% filter(deleted == 0)
    }
    w <- w[, COLDAT]

    ## preproceso registros caracteres incompletos
    loncam <- longitud_variables(columnas, dat_dict)
    nc <- sum(abs(loncam))

    nn <- vapply(w, nchar, 1L, USE.NAMES = FALSE)
    nr <- (nc - nn) %>%
        is_greater_than(0) %>%
        which()

    if ( length(nr) > 0 ) {
        sp <- rep(" ", length.out = nc) %>%
            paste0(collapse = "")

        for ( k in nr ) {
            w[k] <- paste0(w[k], substring(sp, nn[k] + 1L, nc))
        }
    }

    if ( !length(clase_col) ) {
        clase_col <- NA
    }

    # alternativa es utilizar substring para extraer las variables
    tf <- tempfile()
    cat(w, file = tf, sep = "\n")
    colnm <- ordenar_conforme(columnas, dat_dict$variable)
    w <- read.fwf(tf, widths = loncam, col.names = colnm,
                  colClasses = clase_col,
                  comment.char = "", strip.white = TRUE,
                  na.strings = "", stringsAsFactors = FALSE)
    w <- w[, columnas]
    unlink(tf)

    invisible(w)
}
