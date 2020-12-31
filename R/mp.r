# -*- coding: utf-8 -*-

##--- funciones procesamiento de datos ---

#' @import foobj

#' @export
set_odb <- function(ob, ...) UseMethod("set_odb")
#' @export
odb_check <- function(ob, ...) UseMethod("odb_check")
#' @export
get_data <- function(ob, ...) UseMethod("get_data")
#' @export
get_atr <- function(x, atr) UseMethod("get_atr")
#' @export
db_constr <- function(ob) UseMethod("db_constr")
#' @export
db_open <- function(ob) UseMethod("db_open")
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
#'     'abrir' la base de datos para mandar ejecutar las instrucciones
#'     de consulta.
#'     
#'     Para cada servidor de base de datos (SQL server, Excel, Access,
#'     MySQL, SQLite, etc.) hay una 'connection string' específica
#'     según la aplicación que sirve de intermediario con el servidor.
#'
#'     Con las librería RODBC y RODBCDBI se usan 'driver' o
#'     controladores ODBC, administrados por la aplicación
#'     (\url{c:/windows/system32/odbcad32.exe}) administrador de datos
#'     ODBC. El menú 'controladores' muestra los controladores ODBC
#'     instalados en el equipo. Si no aparece el indicado hay que
#'     descargar e instalar el 'software' que lo instala.
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
#' @export
is_odb <- function(x) {
    inherits(x, "odb")
}

#' print.odb
#' @description Muestra los datos. Palabra clave (nombre 'pwd') se
#'     oculta.
#' @param x objeto odb
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
#' @description Ver la estructura de objeto odb. Si hay palabra clave,
#'     se oculta
#' @param x objeto odb
#' @export
str.odb <- function(x) {
    print(x)
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
#' @export
set_odb.odb <- function(x, ...) {
    cc <- names(x)
    xx <- list(...)
    zz <- c(x[setdiff(cc, names(xx))], list(...))
    class(zz) <- class(x)
    invisible(zz)
}

#' Get odb property
#' @description devuelve una propiedad
#' @param x objeto odb
#' @param atr character: atributo o propiedad
#' @return valor atributo o NA
#' @export
get_atr.odb <- function(x, atr){
    stopifnot("arg atr inadmisible" = filled_char(atr))
    ifelse(is.element(atr, names(x)), x[[atr]], NA_character_)
}

#' Check - odb
#' @description Comprueba los atributos de objeto odb
#' @param x objeto odb
#' @return nada
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
#' @return NULL
db_close.RODBC <- function(x) {
    ww <- tryCatch(RODBC::odbcClose(x),
                   error = function(e) e)
    invisible(NULL)
}

#' Validar RODBC
#' @description Es objeto RODBC?.
#' @param x objeto RODBC
#' @return logical
#' @export
is_rodbc <- function(x) {
    inherits(x, "RODBC")
}

#' Canal abierto
#' @description Habilitado el acceso a la base de datos?
#' @param x objeto RODBC
#' @return logical
#' @export
is_open.RODBC <- function(x) {
    !inherits(db_info(x), "try-error")
}

#' Database info
#' @description Información básica sobre la base de datos y el
#'     "driver"
#' @param x objeto RODBC
#' @return character
db_info.RODBC <- function(x) {
    ww <- tryCatch(RODBC::odbcGetInfo(x),
                   error = function(e) e)
    invisible(ww)
}

#' Tablas base de datos
#' @description Nombres de las tablas en la base de datos.
#' @details Simplifica el resultado de llamar la función
#'     \code{sqlTables}
#' @param x objeto RODBC
#' @param msg cuántas tablas? TRUE por defecto
#' @return character o NULL
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
#' @export
tabla_exists.RODBC <- function(x, tb = character()) {
    stopifnot("arg. tb inadmisible" = filled_char(tb) &&
              nzchar(tb))
    is.element(tolower(tb), tolower(db_tablas(x)))
}

#' Nombre de columnas
#' @description Nombre de las columnas de una tabla en base de datos
#' @details Simplifica el resultado que devuelve la función
#'     \code{sqlColumns}
#' @param x objeto RODBC
#' @param tb character: nombre de la tabla
#' @return character o NULL
#' @export
db_columnas.RODBC <- function(x, tb = character()) {
    stopifnot("arg. tb inadmisible" = filled_char(tb) && nzchar(tb))
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
#' @export
db_qry.RODBC <- function(x, xs = character(), strfac = FALSE,
                         max = 0L, ...) {
    stopifnot("arg. xs inadmisible" = filled_char(xs) &&
                  is_scalar(xs) && nzchar(xs))
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
#' @export
db_fetch.RODBC <- function(x, tb = character(), strfac = FALSE,
                           max = 0L, ...) {
    stopifnot("arg. tb inadmisible" = filled_char(tb) &&
                  is_scalar(tb) && nzchar(tb))
    
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
#'     datos, mediante \code{sqlSave} de RODBC
#' @param x ojeto RODBC
#' @param df data.frame
#' @param tb character: nombre de la tabla
#' @param rn logical: agrega nombre de filas a la tabla? FALSE por
#'     defecto
#' @param ... adicionales para \code{sqlSave}
#' @return NULL o mensaje de error
#' @export
db_save.RODBC <- function(x, df, tb = character(),
                          rn = FALSE, ...) {
    stopifnot("arg. df inadmisible" = inherits(df, "data.frame"),
              "arg. tb inadmisible" = filled_char(tb) &&
                  is_scalar(tb) && nzchar(tb))
    
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
#' @export
db_drop.RODBC <- function(x, tb = character()) {
    stopifnot("arg. tb inadmisible" = filled_char(tb) &&
                  is_scalar(tb) && nzchar(tb))
    
    ww <- tryCatch(RODBC::sqlDrop(x, df),
                   error = function(e) e)
    if (inherits(ww, "try-error")){
        message("\n!!!... ERROR")
    }
    invisible(ww)
}

#' save-excel
#' @description Guarda data.frame en una tabla excel
#' @details Un libro de excel funcionalmente es una base de datos, en
#'     el que las hojas o pestañas, y los rangos de celdas con nombre,
#'     juegan el papel de tablas de la base de datos. El argumento
#'     "tabla" debe hacer referencia a una hoja (los datos se guardan
#'     a partir de la primera fila y columna) o a un rango con nombre
#'     previamente definido en el libro excel.
#'
#'     El archivo debe existir y no debe estar abierto cuando se
#'     ejecute la función.
#' @param x data.frame
#' @param tabla character: nombre de la tabla excel
#' @param file character: nombre del archivo excel
#' @param xv7 versión 7 de excel (xslx)? TRUE por defecto
#' @return NULL
#' @seealso save_cel_xcl
#' @export
#' @author eddy castellón
save_xcl <- function(x, tabla = character(), file = character(),
                     xv7 = TRUE) {
    stopifnot("arg. x inadmisible" = inherits(x, "data.frame"),
              "arg. tabla inadmisible" = filled_char(tabla) &&
                  is_scalar(tabla) && nzchar(tabla),
              "arg. file inadmisible" = filled_char(file) &&
                  is_scalar(file) && file.exists(file))
    
    oo <- db_xcl(file, version7 = xv7, ronly = FALSE)
    kk <- db_open(oo)
    if (is_rodbc(kk)){
        db_save(kk, df, tabla)
        odbcClose(kk)
    } else {
        message("error conexión\n")
    }
    NULL
}

#' read-excel
#' @description Leer tabla o rango con nombre de excel
#' @param x character: nombre del rango
#' @param file character: nombre del archivo
#' @param xv7 versión 7 de excel (xlsx)? TRUE por defecto
#' @return data.frame o NULL
#' @seealso save_xcl
#' @export
#' @author eddy castellón
read_xcl <- function(x, file, xv7 = TRUE) {
    stopifnot("arg. x inadmisible" = filled_char(x) &&
                  is_scalar(x) && nzchar(x),
              "arg. file inadmisible" = filled_char(file) &&
                  is_scalar(file) && file.exists(file))
    oo <- db_xcl(file, version7 = xv7, ronly = FALSE)
    kk <- db_open(oo)
    if (is_rodbc(kk)){
        uu <- db_fetch(kk, x)
        odbcClose(kk)
    } else {
        uu <- NULL
    }
    invisible(uu)
}

#' save-excel
#' @description Almacena data.frame en celdas de un libro excel.
#' @details Utiliza funciones de librería «XLConnect» para guardar los
#'     datos de un data.frame en un rango de celdas que inicia en la
#'     fila y columna indicadas en los argumentos, o la referencia a
#'     la celda en la notación que se acostumbra en excel; eg. "A$1" o
#'     "J$20". Si no se utiliza la referencia se deben indicar la fila
#'     y columna; pero si se indica la referencia, los argumentos en
#'     de fila y columna son ignorados. De acuerdo a la documentación,
#'     el número máximo permitido de fila es 1048576, y 16384 el de
#'     columna.
#'
#'     Si no existe el archivo excel, este es creado; si ya existe y
#'     hay datos en el rango indicado, estos son remplazados. Así
#'     mismo, si la hoja no existe, es creada; y si no se especifica,
#'     asume que es la primera. Contrario a la función \code{save_xcl}
#'     el rango no tiene que haber sido creado previamente.
#'
#'     La primera vez que se utiliza esta función, «XLConnect» crea
#'     una "máquina virtual" para ejecutar una aplicación del lenguaje
#'     Java. Para esto, debe estar instalado el
#'     "Java Runtime Environment" o "JRE" en el sistema (generalmente
#'     lo está, pero hay que verificar la versión). El parámetro
#'     "free" (FALSE por defecto) es para pedir que se libere la
#'     memoria utilizada por la máquina virtual. Lea la documentación
#'     de «XLConnect» y los requerimientos del sistema con
#'     \code{packageDescription("XLConnect")}.
#' @param x data.frame
#' @param file character: nombre del archivo
#' @param hoja nombre (character) o número (numeric) de la hoja o
#'     pestaña que recibirá los datos
#' @param rfc character: referencia a celda excel.
#' @param col integer: número de la columna; igual a 1 por omisión
#' @param fila integer: número de la fila; igual a 1 por omisión
#' @param free logical: liberar memoria (TRUE); FALSE por omisión
#' @return logical
#' @seealso save_xcl
#' @export
save_cel_xcl <- function(x, file = character(), hoja = 1L,
                         rfc = character(),
                         col = 1L, fila = 1L,
                         free = FALSE) {
    stopifnot(exprs = {
        inherits(x, "data.frame")
        filled_char(file) && is_scalar(file)
        ok_fname(file) #existe o puede crearse

        (filled_num(hoja) || filled_char(hoja)) && is_scalar(hoja)

        ifelse(is_vacuo(rfc), is.character(rfc),
               is_scalar(rfc) && filled_char(rfc) &&
               grepl("[A-Z]+\\$[0-9]+", rfc))

        is_scalar(fila) && filled_num(fila) &&
            as.integer(fila) >= 1 && fila < 1048576

        is_scalar(col) && filled_num(col) &&
            as.integer(col) >= 1 && col < 16384
    })

    ##referencia; C3 o C$3; ÔjÔ no es absoluta como $C$3 p.ej
    if (filled_char(rfc)) {
        mm <- XLConnect::cref2idx(rfc)
        fila  <- mm[1, 1]
        col  <- mm[1, 2]
    } else {
        fila <- as.integer(fila)
        col <- as.integer(col)
    }

    fe <- file.exists(file)
    wb <- try(XLConnect::loadWorkbook(file, create = !fe),
              silent = TRUE)
    stopifnot("!!! ERROR libro" = inherits(wb, "workbook"))
    
    if (fe) {
        sh <- XLConnect::getSheets(wb)
    } else {
        sh <- character()
    }

    ## hoja int. -> char.
    if (is.numeric(hoja)) {
        hoja <- as.integer(hoja)
        if (hoja == 0L) hoja <- 1L
        if (hoja <= length(sh)) {
            hoja <- sh[hoja]
        } else { #file no exist. u hoja > num.hojas
            hoja <- tempfile("", "") %>%
                substr(3, 7) %>%
                paste0("H", hoja, .) #confianza 8 carac. no exista
        }
    }
    if (!is.element(hoja, sh)) {
        XLConnect::createSheet(wb, hoja)
    }
        
    ## capturar errores
    tr <- try({XLConnect::writeWorksheet(wb, x, sheet = hoja,
                                  startRow = fila, startCol = col,
                                  header = TRUE)
                                  XLConnect::saveWorkbook(wb)},
              silent = TRUE)

    if (free) {
        xlcFreeMemory()
    }

    ko <- inherits(tr, "try-error")
    if (ko) {
        warning("\n... Error escribir o guardar archivo !!!")
    }
    
    !ko
}

#' Leer-excel
#' @description Lee datos en un rango de celdas de libro excel
#' @details Ver explicación en la ayuda de la función
#'     \code{save_cel_xcl}
#' @param file character: nombre del archivo
#' @param hoja nombre (character) o número (numeric) de la hoja
#' @param rf1 character: referencia de la celda superior izquierda
#'     del rango; por omisión, "A$1"
#' @param rf2 character: referencia de la celda inferior derecha del
#'     rango
#' @param free logical: TRUE para liberar memoria; FALSE por omisión
#' @return data.frame o NULL
#' @seealso save_cel_xcl
#' @export
read_cel_xcl <- function(file = character(), hoja = 1L,
                         rf1 = "A$1", rf2 = character(),
                         free = FALSE) {
    stopifnot("arg. file inadmisible" = filled_char(file) &&
                  is_scalar(file) && file.exists(file),

              "arg. hoja inadmisible" = (filled_num(hoja) ||
                  filled_char(hoja)) && is_scalar(hoja),

              "arg. rf1 inadmisible" = filled_char(rf1) &&
                  is_scalar(rf1) && nzchar(rf1) &&
                  grepl("[A-Z]+\\$[0-9]+", rf1),

              "arg. rf2 inadmisible" = filled_char(rf2) &&
                  is_scalar(rf2) && nzchar(rf2) &&
                  grepl("[A-Z]+\\$[0-9]+", rf2)
              )

    wb <- try(XLConnect::loadWorkbook(file), silent = TRUE)
    stopifnot("error lectura" = inherits(wb, "workbook"))

    if (is.numeric(hoja)) hoja <- as.integer(hoja)
    sh <- XLConnect::getSheets(wb)
    stopifnot("arg. hoja inadmisible" = ifelse(is.character(hoja),
               is.element(hoja, sh),
               hoja >= 1 && hoja <= length(sh))
        )
    
    mm <- XLConnect::cref2idx(rf1)
    r1  <- mm[1, 1]
    c1  <- mm[1, 2]

    mm <- XLConnect::cref2idx(rf2)
    r2 <- mm[1, 1]
    c2 <- mm[1, 2]

    df <- try(XLConnect::readWorksheet(wb, hoja, r1, c1, r2, c2),
              silent = TRUE)
    if (!inherits(df, "data.frame")) {
        df <- NULL
        warning("\n... Error de lectura !!!")
    }

    if (free) {
        xlcFreeMemory()
    }

    invisible(df)
}

##--- expresiones SQL ---

## ListOfListOfCharacter,Character,Character,Character Character ->
## Character

#' SQL-expresión
#' @description Construye expresión SQL
#' @details Los nombres (y sus alias) de las tablas involucradas en la
#' expresión, así como los campos y su nombre en el resultado
#' (cláusula «as»), se especifican en una lista de listas (una para
#' cada tabla). La lista de cada tabla compuesta de 3 vectores tipo
#' character: uno nombrado "a", con el nombre de la
#' tabla y su alias; otro de nombre "k" a partir de listas con
#'     estructura list(a=c(TABLA, alias), k=c(campos), as=c(alias de
#'     campos)) y, si se requiere, clausulas where, inner join,
#'     left(right) join, order by y group by. Si es un join, se
#'     requieren dos tablas y la cláusula on completada por whr
#'     (where). El tipo de join se puede indicar sólo con las
#'     primeras letras; e.g in(nner)
#' @param lak lista con vectores tabla-alias, campos de la tabla y
#'     nombres de columnas del data.frame que resulte de la consulta
#' @param whr cláusula where
#' @param ord cláusula order by
#' @param gby cláusula group by
#' @param joi cláusula join
#' @return expresión de consulta SQL
#' @examples
#' xsql(list(list(a=c("pria", "a"), k=c("c1", "c2"), as=c("a", "b")),
#'           list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a2", "b2"))),
#'      whr="a.c1=b.c1")
#'   select a.c1 as a,a.c2 as b,b.c1 as a2,b.c2 as b2 from pria a,prib b
#'       where a.c1=b.c1
#'
#' xsql(list(list(a=c("pria", "a"), k=c("c1", "c2"), as=c("a", "b")),
#'           list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a2", "b2"))),
#'      whr="a.c1=b.c1", joi="le")
#'   select a.c1 as a,a.c2 as b,b.c1 as a2,b.c2 as b2 from pria a left
#'       join prib b on a.c1=b.c1
#'
#' xsql(list(a=c("prib", "b"), k=c("c1", "c2"), as=c("a", "b")))
#'   select b.c1 as a,b.c2 as b from prib b
#' @export
#' @import magrittr
#' @importFrom assertthat assert_that
xsql <- function(lak = NULL, whr = "", ord = "", gby = "", joi = ""){
    assert_that(!is.null(lak))
    if (!is.list(lak[[1]])) lak <- list(lak)
    una_t <- length(lak) == 1
    ## más de una tabla es un join
    assert_that((nzchar(whr) || nzchar(joi)) == !una_t,
                msg="where o join si más de una tabla")
    joins <- c("left join", "inner join", "right join")
    if(nzchar(joi)){
        joi <- pmatch(joi, joins)
        assert_that(!is.na(joi), nzchar(whr),
                    length(lak) == 2,
                    msg = "join correcto entre 2 tablas")
    }
        
    ## lista de campos, alias y tablas correctos
    assert_that(
        all(vapply(lak, function(x)length(x[["k"]]) > 0,
                   TRUE)),
        all(vapply(lak, function(x)length(x[["a"]]) == 2,
                   TRUE)),
        all(vapply(lak, function(x) is.null(x[["as"]]) |
                                    length(x[["k"]]) ==
                                    length(x[["as"]]), TRUE)))
    sq <- vapply(lak,
                 function(x){
                     if (!is.null(x[["as"]])){
                         x[["k"]] <- paste(x[["k"]], x[["as"]],
                                           sep=" as ")
                     }
                     sp <- ifelse(nzchar(x[["a"]][2]), ".", "")
                     paste(x[["a"]][2], x[["k"]], sep=sp,
                           collapse=",")
                 }, "", USE.NAMES=FALSE) %>%
        paste(collapse=",") %>% paste("select", ., "from",
                                      paste(lak[[1]][["a"]],
                                            collapse=" "))
    ## join
    if(!una_t){
        if(nzchar(whr)){
            sq <- vapply(lak[-1], function(y)paste(y[["a"]], collapse=" "),
                         "", USE.NAMES=FALSE) %>% paste(collapse=",") %>%
                paste(sq, ., sep=",") %>%
                paste("where", whr)
        } else{
            if(!is.na(joi)){
                sq <- paste(sq, joins[joi],
                            paste(lak[[2]][["a"]], collapse=" "),
                            "on", whr)
            }
        }
    }
  
    ## TODO validar expresiones
    if(nzchar(ord)) sq <- paste(sq, "order by", ord)
    if(nzchar(gby)) sq <- paste(sq, "group by", gby)
    sq
}

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
#'     los correspondientes parámetros.
#'
#'     Si la expresión contiene un «join», se requieren dos tablas, y
#'     la cláusula «on» es completada por el argumento al parámetro
#'     "whr". El tipo de «join» se puede indicar sólo con las primeras
#'     letras; e.g in(nner)
#' @param lak character o lista de lista de character
#' @param whr character: cláusula where
#' @param ord character: cláusula order by
#' @param gby character: cláusula group by
#' @param joi character: cláusula join
#' @return character
#' @export
#' @examples
#' xsql(list(list("tab", c("cm1", "cm2"))))
#' #-> "select cm1, cm2 from tab"
#' xsql(list(list("tab", c(x = "cm1", y = "cm2"))))
#' #-> "select cm1 as x, cm2 as y from tab"
#' xsql(list(list(a = "tab", c("cm1", "cm2"))))
#' #-> "select a.cm1 as x, a.cm2 as y from tab a"
xsql <- function(x = list(), whr = character(), ord = character(),
                 qby = character(), joi = character()) {
    stopifnot(exprs = {
        "arg. x inadmisible" = (is.list(x) && length(x) &&
                                all(sapply(x, length) == 2)) ||
            (filled_char(x) && length(x) == 2)
        
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
        joi <- jn[pmatch(joi, cc)]
        stopifnot("join 2 tablas" = is.list(x) && length(x) == 2,
                  "arg. joi inadmisible" = !is.na(joi))
    }

    tbc <- function(x) {
        nx <- names(x)
        cm <- x[[2]]
        tb <- x[[1]]
        if (filled(nx)) {
            cm <- paste(nx[1], cm, sep = ".")
            tb <- paste(tb, nx[1])
        }
        x[[1]] <- tb
        x[[2]] <- paste(cm, collapse = ",")
        x
    }

    ## Reduce(function(x,y)c(paste(x[[1]], y[[1]], sep = ","),
    ##                       paste(c(x[[2]], y[[2]]), collapse = ",")),
    ##        list(list("a ee", c("ee.a", "ee.b")),
    ##             list("b ee", c("ee.c", "ee.d"))),
    ##        init=list("", "")) %>%
    ##     substring(2)
    
    ## Reduce(function(x,y)c(paste(x[[1]], y[[1]], sep = ","),
    ##                       paste(c(x[[2]], y[[2]]), collapse = ",")),
    ##        list(list("a ee", c("ee.a", "ee.b"))),
    ##        init=list("", "")) %>%
    ##     substring(2)

    x <- lapply(x, tbc) %>%
        Reduce(function(x, y)c(paste(x[[1]], y[[1]], sep = ","),
                               paste(c(x[[2]], y[[2]]), collapse = ",")),
               ., init = list("", "")) %>%
        substring(2)
    
    ss <- paste("select", x[2], "from", x[1])
    ss
}

## Character, VectorOfCharacter, VectorOfCharacter -> Character
##' Versión simplificada de xsql
#' @description devuelve expresión SQL a partir de TABLA, CAMPOS y
#'     ALIAS de campos
#' @param tabla nombre de tabla (entre comillas)
#' @param campos vector de caracteres con nombres de campos
#' @param as vector de caracteres con los alias de campos
#' @examples
#' xsql_s("pri", c("a","b"), c("x", "y"))
#'   "select a.a as x,a.b as y from pri a"
#' xsql_s("pri", c("a","b")) "select a.a,a.b from pri a"
#' @export
#' @importFrom assertthat assert_that
xsql_s <- function(tabla, campos, as = NULL, alias){

    assert_that(!(missing(tabla) || missing(campos)),
                ok_nombre(tabla),
                is.character(campos) && all(nzchar(campos)),
                msg = "cuáles campos de cuál tabla?")

    if (!is.null(as)){
        assert_that(is.character(as) && all(nzchar(as)) &&
                    length(campos) == length(as),
                    msg = "algún nombre de campo no es válido\n")
    }

    if (missing(alias)){
        alias <- ""
    } else {
        assert_that(ok_nombre(alias),
                    msg = "alias de tabla no es válido\n")
    }
    xsql(list(a=c(tabla, alias), k=campos, as=as))
}

#' Parámetro lista de \code{xsql}
#' @description facilita construir lista para llamar a \code{xsql}
#' @param db nombre de tabla (entre comillas)
#' @param km vector de caracteres con nombre de campos
#' @param al nombre de alias de la tabla
#' @param as vector de caracteres con nombre de columnas
#' @return lista tabla-campos apta para llamar a función \code{xsql}
#' @export
#' @importFrom assertthat assert_that
lxs <- function(db, km, al="a", as=NULL){
    if(!is.null(as)){
        assertthat::assert_that(is.character(as),
                                length(km) == length(as))
    }
    list(list(a=c(db, al), k=km, as=as))
}

## VectorOfCharacter -> character ' SQL union
#' SQL union
#' @description Construye la expresión \code{union} de dos o más
#'     expresiones SQL. \code{union} incluye sólo una vez cada
#'     registro en el resultado de la consulta; \code{union all} los
#'     incluye a todos. Para identificar los registros que resulten
#'     de mandar a ejecutar las subconsultas, a estas se le puede
#'     agregar un campo que devuelva una "constante". En el
#'     resultado, la columna tendrá el nombre \code{nomlab}.
#' @param x vector de caracteres cuyos elementos son expresiones SQL
#' @param all \code{union all}?; TRUE por defecto
#' @param nomcol nombre de la columna si se quiere identificar las
#'     subconsultas
#' @param idcon vector de caracteres o de enteros que servirán para
#'     etiquetar las subconsultas
#' @return expresión SQL
#' @export
#' @importFrom assertthat assert_that
#' @examples
#' xsql_u(c("xsqla", "xsqlb", "xsqlc")) ->
#'        "xsqla union all (xsqlb union all (xsqlc))"
xsql_u <- function(x, all = TRUE, nomcol = character(),
                   idcon = character()){
    nn <- length(x)
    assert_that(is.character(x),  nn > 1,
                msg = "más de una expresión SQL")

    ## nombra las subconsultas para auxiliar la identificación de los
    ## registros resultantes de cada una
    if (length(nomcol)){
        assert_that(ok_nombre(nomcol),
                    msg = "falta nombre de índices de consulta")
        if (!length(idcon)){
            idcon <- seq_len(nn)
        } else {
            assert_that(length(idcon) == nn,
                        msg = "número de nombres y de subconsultas")
            ## para darles nombres
            if (is.character(idcon)){
                idcon <- sQuote(idcon)
            }
        }
        ## se agregan a la expresión como constantes
        x <- vapply(seq_along(x),
                     function(z){
                         cc <- paste0("\\1,", nomcol, "=",
                                      idcon[z], "\\2")
                         sub("(.+)(\\s+from.+)", cc, x[z], perl = TRUE)
                     },
                     "a", USE.NAMES = FALSE)
    }

    nest_str(c(paste(x[-nn],
                    paste0("union", ifelse(all, " all", ""))),
              x[nn]))
}

#' Expresion SQL encuestas MAG
#' @description leer cuadros con estructura de códigos
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
#'     donde las columnas son las variables
#'
#'     Cada fila del cuadro corresponde a una consulta SQL, las que
#'     son combinadas en una sola mediante la cláusula 'union
#'     all'. A cada consulta se le agrega un campo 'constante' que
#'     indica a cuál fila del cuadro corresponde, lo que permitirá,
#'     después de mandar a ejecutar la consulta, identificar los
#'     registros extraídos por cada subconsulta.
#' @param nn vector de enteros o de códigos; si es tipo entero, debe
#'     incluirse el parámetro con la función que construye códigos
#' @param tab nombre de la tabla que se va a consultar
#' @param nomvar nombre de las variables
#' @param idr nombre del campo de la tabla con el nombre de los
#'     registros
#' @param nomidr nombre de la columna que traerá los nombres de
#'     registros de donde se extrae la consulta
#' @param uall union all? TRUE por defecto
#' @param nomcol nombre de la columna que llevará la etiqueta de
#'     consulta
#' @param idcon nombres que identifican los registros de cada
#'     consulta; por omisión, los enteros de 1 hasta el número de
#'     consultas
#' @param por_fila secuencia de códigos por fila? TRUE por defecto
#' @param ... parámetros adicionales para matrix_cod
#' @return expresión SQL
#' @export
#' @importFrom assertthat assert_that
xsql_t <- function(nn, tabla, nomvar = character(),
                   idr = NULL, nomidr = "quest", uall = TRUE,
                   nomcol = NULL, idcon = NULL, ...){

    assert_that(!missing(tabla), ok_nombre(tabla),
                ok_chr(nomvar) && all(nzchar(nomvar)),
                msg = "falta nombre de tabla o de variables")

    nc <- length(nn)
    nvar <- length(nomvar)
    ##nvar <- ifelse(!nv, nc, nv)

    nr <- nc %/% nvar
    assert_that(nc == nvar * nr,
                msg = paste("número de elementos no es múltiplo",
                            "de número de variables"))

    if (is.null(idcon)){
        idcon <- seq_len(nr)
    } else {
        assert_that(length(idcon) == nr,
                    msg = paste("número de etiqueta no es igual",
                                "al número de consultas"))
    }
    
    ## if (!nv){
    ##     nomvar <- paste0("V_", seq_len(nvar))
    ## }
    
    ##mk <- matrix_cod(nn, ncol = nvar, nomcol = nomvar, ...)
    mk <- matrix_cod(nn, nomcol = nomvar, ...)
    
    if (!is.null(idr)){
        mk <- cbind(idr, mk)
        colnames(mk)[1] <- nomidr
    }
    
    ## expresiones de consulta
    ## TODO incorporar where
    nc <- colnames(mk)
    ss <- vapply(split(mk, seq_len(nr)),
                 function(x){
                     xsql(lxs(tabla, al = "", km = x, as = nc))
                 },
                 "a", USE.NAMES=FALSE)
    
    xsql_u(ss, uall, nomcol, idcon)
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
#' @export
get_data.odb <- function(x, qstr = character(), meta = character(),
                         sin_0 = FALSE, na_0 = FALSE, max = 0, ...) {
    stopifnot("arg. qstr inadmisible" = filled_char(qstr) &&
                  is_scalar(qstr) && nzchar(qstr),
              "arg. meta inadmisible" = is.character(),
              "arg. max inadmisible" = filled_num(max) && is_scalar(max)
              )

    kk <- db_open(x)
    if (ok <- is_rodbc(kk)) {
        ww <- db_qry(kk, qstr, max = max)
        odbcClose(kk)
    
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
