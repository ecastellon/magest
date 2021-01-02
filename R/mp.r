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
#' #-> "select cm1, cm2 from tab"
#' xsql(list(list("tab", c(x = "cm1", y = "cm2"))))
#' #-> "select cm1 as x, cm2 as y from tab"
#' xsql(list(list(a = "tab", c("cm1", "cm2"))))
#' #-> "select a.cm1 as x, a.cm2 as y from tab a"
#' tt <- list(list(a = "ta", c(w = "a", x = "b")),
#'            list(b = "tb", c(y = "c", z = "d")))
#' xsql(tt, joi = "in", whr = "a.a=b.c")
#' #-> "select a.a as w,a.b as x, b.c as y, b.d as z ...
#'      from ta a, tb b inner join on a.a=b.c"
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

#' SQL expresión
#' @description Expresión SQL sencilla
#' @param x character: nombre de la tabla
#' @param cm character: nombre de los campos. Si los elementos con
#'     nombre, estos son puestos en la cláusula «as». Por omisión es
#'     "*".
#' @return character
#' @export
#' @examples
#' xsql_s("a", c(x = "a", y = "b"))
#' #-> "select a as x,b as y from a"
#' xsql_s("a")
#' #-> "select * from a"
xsql_s <- function(x = character(), cm = "*") {
    stopifnot("arg. inadmisibles" = filled_char(x) &&
                  is_scalar(x) && nzchar(x) &&
                  filled_char(cm))

    if (!is_scalar(cm)) {
        nm <- names(cm)
        if (filled(nm)) {
            cm <- paste(cm, nm, sep = " as ")
        }
        cm <- paste(cm, collapse = ",")
    }
    paste("select", cm, "from", x)
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
                                            filled_char(cid) &&
                                            is_scalar(cid) &&
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
#' @seealso xsql_u
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
            ((filled_num(idc) || is_name(idc)) && filled(cid) &&
             length(cam) == length(nvb) * length(idc))
        "arg. idr inadmisible" = is_scalar_name(idr)
    })
    
    cn <- c(idr, nvb)
    mk <- matrix(cam, ncol = length(nvb), byrow = xfi) %>%
        cbind(idr, .) %>%
        set_names(cn) %>%
        apply(1, function(z) xsql_s(x, cm = setNames(z, cn)))


    #ss <- split(mk, seq_len(nrow(mk))) %>%
     #ss <- apply(mk, 1, function(z)xsql_s(x, cm = setNames(z, cn)))
    #           USE.NAMES = FALSE)

    xsql_u(mk, idc, cid, all)
}

#' matriz de códigos
#' @description construye una matriz de códigos a partir de un vector
#'     de códigos, o a partir de un vector de enteros si la función
#'     recibe la función que los construye.
#' @param nn vector de enteros o de caracteres (códigos)
#' @param nomcol nombre de las columnas de la matriz
#' @param nomfi nombre de las filas
#' @param por_fila la matriz se llenará¡ por fila por fila? TRUE por
#'     defecto
#' @param cod función que generará los códigos si el parámetro nn es
#'     un vector de enteros
#' @return matriz de códigos
#' @export
#' @examples
#' ff <- codigo_fac(di = 3)
#' xx <- matriz_cod(1:6, nomcol = c("x", "y", "z"), cod = ff)
#' @importFrom assertthat assert_that
matrix_cod <- function(nn, nomcol = NULL,
                       nomfi = NULL, por_fila = TRUE, cod = NULL){
    assert_that((is.numeric(nn) && is.function(cod)) ||
                is.character(nn),
                msg = "si entero falta funcion de codigos")

    if (is.numeric(nn)){
        nn <- cod(as.integer(nn))
    }
    
    ncol <- length(nomcol)
    if (!length(ncol)){
        ncol <- length(nn)
    }

    ## if (!is.null(nomcol)){
    ##     assert_that(length(nomcol) == ncol,
    ##                 msg = "columnas inconsistente con nombres")
    ## }

    matrix(nn, ncol = ncol, byrow = por_fila,
           dimnames = list(nomfi, nomcol))
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
