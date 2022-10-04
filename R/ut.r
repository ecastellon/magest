# -*- coding: utf-8 -*-

## --- utilidad general --
#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_vacuo <- function(x) {
    length(x) == 0L
}

#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_empty <- function(x) {
    length(x) == 0L
}

#' Escalar
#' @description Es vector con un elemento?
#' @param x vector
#' @return logical
#' @keywords internal
is_scalar <- function(x) {
    length(x) == 1L
}

#' Escalar
#' @description Es vector vacío o con un elemento?
#' @param x vector
#' @return logical
#' @keywords internal
is_scalar0 <- function(x) {
    length(x) <= 1L
}

#' no na
#' @description No es NA
#' @param x vector
#' @return logical
#' @export
no_na <- function(x) !is.na(x)

#' ningún NA
#' @description Ningún elemento es NA
#' @param x vector
#' @return logical
#' @export
ningun_na <- function(x) !anyNA(x)

#' length
#' @description vector has length greater than zero?
#' @param x vector
#' @return logical
#' @keywords internal
#' @author eddy castellón
filled <- function(x) {
    length(x) > 0L
}

#' character type
#' @description vector is of character type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_char <- function(x) {
    is.character(x) && length(x)
}

#' numeric mode
#' @description vector is of numeric mode and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_num <- function(x) {
    is.numeric(x) && length(x)
}

#' integer type
#' @description vector is of integer type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_int <- function(x) {
    is.integer(x) && length(x)
}

#' logical type
#' @description vector is of logical type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_log <- function(x) {
    is.logical(x) && length(x)
}

#' list-no-vac
#' @description vector is of logical type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_list <- function(x) {
    is.list(x) && length(x)
}

#' valida-name
#' @description Valida nombres de variables
#' @param x vector
#' @return logical
#' @keywords internal
is_name <- function(x) {
    length(x) && identical(x, make.names(x))
}

#' nombre-scalar
#' @description Valida nombre escalar
#' @param x vector
#' @return logical
#' @keywords internal
is_scalar_name <- function(x) {
    length(x) == 1L && identical(x, make.names(x))
}

#' Suma
#' @description Suma que excluye datos NA
#' @return function
#' @export
suma <- purrr::partial(sum, na.rm = TRUE)

#' Suma ponderada
#' @description Suma pondera que excluye los datos y las ponderaciones NA
#' @param x numeric: los datos
#' @param w numeric: las ponderaciones
#' @return numeric
#' @export
suma_pon <- function(x = numeric(), w = numeric()) {
    stopifnot("arg.x inválido" = filled_num(x),
              "arg.w inválido" = filled_num(w) &&
                                 length(x) == length(w))
    suma(x * w)
}

#' Media ponderada
#' @description Media ponderada que excluye datos NA
#' @return numeric
#' @export
media_pon <- purrr::partial(weighted.mean, na.rm = TRUE)

## --- data.frame ---
#' @export
quitar_0 <- function(x, excepto) UseMethod("quitar_0")

#' quitar ceros
#' @description Elimina las filas de un data.frame que tienen cero en
#'     todas las columnas de modo numeric, excepto en las columnas
#'     indicadas.
#' @param df data.frame
#' @param excepto nombre (character) o posición (numeric) de las
#'     columnas de modo numeric que serán ignoradas
#' @return data.frame
#' @examples
#' aa <- data.frame(
#'     x = 1:5, y = c(0, 0, 0, 1, 1),
#'     z = c(5, 0, 0, 2, 0)
#' )
#' (quitar_0(aa, excepto = "x"))
#' @export
#' @author eddy castellón
quitar_0.data.frame <- function(df, excepto = character()) {
    stopifnot(
        "arg. df inadmisible" = inherits(df, "data.frame"),
        "arg. excepto inadmisible" = is.character(excepto) ||
            is.numeric(excepto)
    )

    ii <- vapply(df, is.numeric, TRUE, USE.NAMES = FALSE)
    if (any(ii)) {
        nm <- names(df)
        if (filled_num(excepto)) { # supone en rango
            excepto <- as.integer(excepto) %>%
                intersect(seq_along(nm))
            excepto <- nm[excepto]
            if (is_vacuo(excepto)) {
                warning("\n... no filtrar ... arg. excep. fuera rango",
                    call. = FALSE
                )
                excepto <- nm # no filtrar datos
            }
        }

        ss <- nm[ii]
        if (filled_char(excepto)) {
            ss <- setdiff(ss, excepto)
        }

        if (filled(ss)) {
            df <- dplyr::filter_at(df, vars(ss), any_vars(. > 0))
        }
    }

    invisible(df)
}

#' @export
anexar_suma_cols <- function(df, cols) UseMethod("anexar_suma_cols")

#' Anexar suma columnas
#' @description Anexar al data.frame la suma de las columnas tipo numeric o
#'     logical.
#' @details Suma todas las columnas de tipo numérico o lógico, o las
#'     especificadas por posición (parámetro «cols»), del data.frame pasado
#'     como argumento, y lo regresa con las sumas anexadas después de la
#'     última fila. La suma descarta los NA. Las columnas tipo character, o
#'     las no especificadas en «cols», son puestas a NA. Si todas las columnas
#'     especificadas son de tipo character, devuelve el data.frame sin
#'     modificar.
#' @param df data.frame
#' @param cols numeric: columnas que se van a sumar. Es opcional.
#' @return data.frame
#' @examples
#' \dontrun{
#'    x <- data.frame(x = 1:3, y = letters[1:3], z = 2.0)
#'    anexar_suma_cols(x)
#'    anexar_suma_cols(x, 3)
#' }
#' @export
anexar_suma_cols.data.frame <- function(df, cols) {
    es_num_log <- purrr::map_lgl(df,
                                 ~ is.numeric(.x) | is.logical(.x))

    if ( any(es_num_log) ) {
        col_sum <- which(es_num_log)
        if ( !missing(cols) ) {
            if ( all(es_num_log[cols]) ) {
                col_sum <- cols
            } else {
                warning("... algunas columnas NO son numéricas",
                        call. = FALSE)
                col_sum <- intersect(col_sum, cols)
            }
        }

        if ( filled(col_sum) ) {
            suma_cols <- colSums(df[, col_sum, drop = FALSE],
                                 na.rm = TRUE)

            df <- as.list(suma_cols) %>%
                as.data.frame() %>%
                bind_rows(df, .)
        }
    }

    invisible(df)
}

#' @export
normalizar_data <- function(df, cols) UseMethod("normalizar_data")

#' Normalizar data.frame
#' @description Normaliza un data.frame que tiene datos de la misma
#'     variable en diferentes columnas
#' @details En la documentación de la librería «tidyr» y en el
#'     artículo de H.Wickham ahí referido, se discuten varios casos de
#'     tablas de datos no normalizadas. Uno que es frecuente, es
#'     cuando en la misma unidad de información (unidad de muestreo,
#'     en las encuestas) se mide la misma variable en diferentes
#'     fechas o en distintas partes de la unidad, como es el caso, en
#'     las encuestas agrícolas, cuando en la misma finca se anotan las
#'     superficies sembradas de diferentes cultivos. En tales casos se
#'     puede terminar con una tabla de datos como la siguiente:
#' 
#'     quest c231 c234 c451 c454
#'     10500    1    2    2    3
#'     10510    0    0    2    6
#' 
#'     donde la columna quest trae los datos del «id» de la finca,
#'     c234 y c451 los datos del cultivo, y c234 y c454 las manzanas
#'     sembradas de los cultivos en cuestión. En realidad sólo hay dos
#'     variables: «cultivo» y «sembrada». Uno de los problemas con la
#'     falta de normalización es que la tabla puede llevar muchos
#'     datos «basura», como son (en el ejemplo) las dos primeras
#'     columnas de la segunda fila.
#'
#'     La tabla normalizada luciría como se muestra adelante. Ahí ya
#'     se ve la economía de almacenamiento: 9 datos en lugar de los 10
#'     en la no normalizada
#' 
#'     quest cultivo sembrada
#'     10500       1        2
#'     10500       2        3
#'     10510       2        6
#'
#'     En la librería ya existe una función que devuelve la expresión
#'     SQL que produce una tabla normalizada cuando se leen los
#'     datos: «xsql_t»
#' @seealso xsql_t
#' @param df data.frame
#' @param col_id numeric: columna con el índice (id) de los datos
#' @param vbl character: nombre de las variables del data.frame
#'     resultado
#' @return data.frame
#' @export
#' @examples
#' x <- data.frame(q = 1:4, a = sample(1:4), b = sample(2 * (1:4)),
#'                 d = sample(3 * 1:4), e = sample(2 * (1:4)))
#' normalizar_data(x, 1, c("xx", "yy"))
normalizar_data.data.frame <- function(df, col_id, vbl = character()) {

    nm <- names(df)
    
    y <- tidyr::pivot_longer(df, nm[-col_id], names_to = "vb",
                             values_to = "y")

    ng <- (length(nm) - 1L) %/% length(vbl)

    y["vb"] <- rep(vbl, ng * nrow(df))
    y["id"] <- rep(seq_len(nrow(df) * ng), each = ng)

    z <- tidyr::pivot_wider(y, id_cols = "id", names_from = "vb",
                            values_from = "y")

    z[nm[col_id]] <- rep(df[[col_id]], each = ng)

    z
}

## --- strings ---

#' factor a caracter
#' @description transforma vector tipo factor a caracter
#' @param x factor
#' @return character
#' @keywords internal
fac2char <- function(x) {
    if (is.factor(x)) {
        ww <- levels(x)[x]
    } else {
        message("argumento no es factor...")
        ww <- x
    }
    ww
}

#' Separar palabras
#' @description Produce un vector con las palabras (token) que se
#'     encuentran en una ristra de caracteres, separadas unas de otras
#'     por espacios o coma
#' @param str character: palabras encerradas por comillas separadas
#'     por coma o espacios
#' @return character
#' @export
#' @examples
#' tok_str("aa bb,cc") #-> c("aa", "bb", "cc")
#' @author eddy castellón
tok_str <- function(str) {
    strsplit(str, split = "[[:space:],]+")[[1L]]
}

#' Podar espacios
#' @description Quita los espacios antes y después de una frase
#' @param x character
#' @export
#' @examples
#' podar_str("  poda   extremos") #-> "poda   extremos"
podar_str <- function(x = character()) {
    stopifnot("arg. x inválido" = filled_char(x))

    ii <- is.na(x)
    x[ii] <- ""

    r <- regexpr("\\b.*\\b", x, perl = TRUE)
    w <- vector("character", length = length(x))
    ## is.na(x) -> is.na(r) y error en asig. con índice
    w[r > 0] <- regmatches(x, r)

    w[ii] <- NA_character_
    w
}

#' Sustituir espacios
#' @description Sustituye una ristra de espacios por uno solo
#' @param x character
#' @export
#' @examples
#' sin_ristra_sp("   a   veces ") #-> " a veces "
sin_ristra_sp <- function(x = character()) {
    stopifnot("arg. x inválido" = filled_char(x))
    gsub("[[:space:]]+", " ", x)
}

#' Primera mayúscula
#' @description En mayúscula la primera letra de una palabra
#' @param x character
#' @examples
#' a_propio('juan calero') #-> 'Juan Calero'
#' @export
a_propio <- function(x = character()) {
    stopifnot("arg. x inválido" = filled_char(x))
    gsub("\\b([a-z])","\\U\\1", tolower(x), perl = TRUE)
}

#' Nombre propio
#' @description Ajusta las palabras a la forma de un nombre propio
#' @details Poda el texto, sustituye ristra de espacios y deja la
#'     primera letra en mayúsculas
#' @param x character
#' @return character
#' @export
#' @examples
#' a_propio('  juan   calero  ') #-> 'Juan Calero'
nombre_propio <- function(x = character()) {
    stopifnot("arg. x inválido" = filled_char(x))
    podar_str(x) %>% sin_ristra_sp() %>% a_propio()
}

#' fabrica código
#' @description Produce una función que genera palabras con un prefijo
#'     seguido de enteros.
#' @details Crea una función que produce un objeto character con
#'     prefijo único indicado en parámetro "prf" (por omisión "c"),
#'     seguido por un entero (parámetro "x") antecedido por
#'     suficientes ceros para llenar tantos espacios como se indique
#'     en el parámetro "di"
#' @param x integer: el entero que complementa el prefijo
#' @param prf character: prefijo; "c" por defecto
#' @param di integer: dígitos que componen el código; 3 por defecto
#' @return función
#' @export
#' @examples
#' ff <- codigo_fac(di = 4)
#' ff(4) #-> "c0004"
#' ff(c(20, 100)) #-> c("c0020", "c0100")
codigo_fac <- function(x, prf = "c", di = 3) {
    function(x) sprintf(paste0("%s%0", di, "i"), prf, x)
}

## --- files ---

#' file name
#' @description Valida nombre de archivo.
#' @details Usa la función \code{file.create}. Para que el nombre sea
#' válido, cualquier directorio en la ruta de acceso, debe existir previamente.
#' @param x character: nombre del archivo
#' @return logical
#' @author eddy castellón
#' @keywords internal
ok_fname <- function(x = character()) {
    ok <- filled_char(x)

    if (ok) {
        ok <- file.exists(x)
        if (!ok) {
            ok <- file.create(x)
            if (ok) {
                unlink(x)
            }
        }
    }

    return(ok)
}

## --- time ---

#' Hoy-Date
#' @description La fecha en el sistema, convertida en objeto de la
#'     clase Date
#' @return Date
#' @export
hoy_date <- function() {
    Sys.time() %>% as.Date()
}

#' Hoy
#' @description Fecha en el formato "año-mes-día"
#' @param sep character: caracter que separa elementos; por omisión
#'     "-"
#' @return character
#' @examples
#' año_mes_dia() #-> 2022-09-26
#' @export
año_mes_dia <- function(sep = "-") {
    paste("%Y", "%m", "%d", sep = sep) %>%
        format(Sys.time(), .)
}

#' Día
#' @description Fecha actual en el formato
#'     "día semana.día mes.mes.año"
#' @return character
#' @examples dia_mes_año() #-> lun.26.sep.2022
#' @export
dia_mes_año <- function() format(Sys.time(), "%a%d.%b%Y")

#' Día-hora
#' @description Día, mes, año, hora en reloj del sistema
#' @return character
#' @examples
#' dia_hora() #-> 26.sep.2022:19h
#' @export
dia_hora <- function() format(Sys.time(), "%d.%b%Y:%Hh")

#' Día-hora-minutos
#' @description Día, mes, año, hora, minutos en reloj del sistema
#' @param sep character: separador de elementos; por omisión, "."
#' @return character
#' @examples
#' dia_hora_min() #-> 26.sep.2022:19:43
dia_hora_min <- function(sep = ".") {
    fmt <- ifelse(sep == ".", "%d.%b%Y:%H:%M",
                  paste("%d", "%b", "%Y","%H:%M", sep = sep))
    Sys.time() %>% format(fmt)
}

## --- misc ---

#' Par
#' @description Es número par
#' @param x numeric
#' @return logical
#' @keywords internal
es_par <- function(x) {
    stopifnot("arg.x no es numérico" = filled_num(x))
    x %% 2 == 0
}

#' Aparear
#' @description Match de dos o más vectores
#' @details Para encontrar el "match" de una pareja de vectores con
#'     otra pareja, lo común es primero construir vectores adicionales
#'     con la función "interaction" y luego hacer el "match" con los
#'     vectores resultantes.
#'
#'     Esta función automatiza ese proceso. Recibe un número par de
#'     vectores y construye la interacción de una mitad y de la otra
#'     mitad y luego hace el "match". Además, de manera opcional
#'     informa cuántos elementos de la primera mitad de vectores no
#'     existen en la otra mitad (no hicieron "match"), y también, de
#'     manera opcional, devuelve en el atributo "sinpar" el
#'     correspondiente vector de índices de los que no tienen pareja.
#'
#' @param ... dos o más vectores
#' @param msg logical: si \code{TRUE} manda mensaje de cuántos no
#'     hacen "match"
#' @param sinpar logical: si \code{TRUE} agrega el atributo "sinpar"
#'     al resultado; \code{FALSE} por defecto.
#' @return integer
#' @examples
#' casar(x, y, msg = FALSE)
#' casar(w, x, y, z)
#' @export
casar <- function(..., msg = TRUE, sinpar = FALSE) {
    ##x <- eval(substitute(alist(...)))
    x <- list(...)

    n <- length(x)
    if ( n == 1 ) {
        return(seq_along(x[[1]]))
    }


    if (n > 2) {
        if (!es_par(n)) {
            warning("... casar(...) número de argumentos no es par !!!",
                    call. = FALSE)
        }
        m <- seq_len(n)
        k <- n %/% 2
        m1 <- head(m, k)
        m2 <- tail(m, n - k)
        x <- list(x = Reduce(interaction, x[m1]),
                  table = Reduce(interaction, x[m2]))
    } else {
        names(x) <- c("x", "table")
    }

    m <- do.call("match", x)
    o <- is.na(m)
    if (any(o)) {
        if (msg) {
            message("... sin pareja ", sum(o), " de ", length(m), " !!!")
        }
        if (sinpar) {
            attr(m, "sinpar") <- which(o, useNames = FALSE)
        }
    }
    m
}

#' Aparear
#' @description Alias de la función \code{match}
#' @param x character o numeric
#' @param y character o numeric
#' @return integer
#' @seealso \code{match}, \code{casar}
#' @export
parear <- function(x, y) {
    casar(x, y)
}

#' Alias %in%
#' @description Operador infijo %in% como función
#' @param x vector
#' @param y vector
#' @return logical
#' @export
en <- function(x, y) match(x, y, nomatch = 0) > 0

#' NA a cero
#' @description Convierte a 0 los elementos NA de un vector de modo
#'     numérico
#' @param x numeric
#' @return numeric o NULL
#' @examples
#' na0(c(1:3, NA_integer_))
#' @export
na0 <- function(x) {
    stopifnot("arg. x inadmisible" = filled_num(x))

    x[is.na(x)] <- ifelse(typeof(x) == "integer", 0L, 0.0)

    return(x)
}

#' Cero - NA
#' @description Convierte a NA los datos igual a cero
#' @param x numeric
#' @return numeric
#' @export
cero_na <- function(x) {
    stopifnot("arg. x inadmisible" = filled_num(x))
    na <- ifelse(typeof(x) == "integer", NA_integer_, NA_real_)
    x[x == 0] <- na
    return(x)
}

#' NA a character
#' @description Convierte los NA_character_ al caracter indicado
#' @param x character
#' @param a character: caracter que sustituye a NA
#' @return character
#' @export
na_char <- function(x, a = "") {
    if ( is.character(x) ) {
        x[is.na(x)] <- a
    }
    return(x)
}

#' Número-entre
#' @description Comprueba si un número está entre los límites de un
#'     intervalo
#' @details La diferencia con la función \code{between} de la librería
#'     dplyr, es que los datos NA se consideran fuera del intervalo, y
#'     que el parámetro «inclusive» determina si el intervalo incluye
#'     o no, a los límites.
#' @param x numeric
#' @param x1 numeric: límite inferior
#' @param x2 numeric: límite superior
#' @param inclusive logical: con igualdad a uno de los límites?; FALSE
#'     por omisión
#' @return logical
#' @examples
#' num_entre(2, 1, 2, TRUE)
#' num_entre(2, 1, 2)
#' @export
num_entre <- function(x, x1 = numeric(), x2 = numeric(),
                      inclusive = FALSE) {
    n <- length(x)
    n1 <- length(x1)
    n2 <- length(x2)
    stopifnot(exprs = {
        "arg. x inválido" = filled_num(x)
        "arg. x1 inválido" = filled_num(x1)
        "arg. x2 inválido" = filled_num(x2)
        "arg. x incomp. x1,x2" = (n >= 1L & n1 == 1L & n2 == 1L) ||
            (n > 1 & n == n1 & (n2 == 1L | n2 == n1)) ||
            (n > 1 & n == n2 & n1 == 1L)
    })

    x[is.na(x)] <- x2 + 1

    if (inclusive) {
        tf <- x >= x1 & x <= x2
    } else {
        tf <- x > x1 & x < x2
    }

    tf
}

#' División
#' @description Calcula el cociente y redondea.
#' @details Los argumentos a los parámetros deben contener el mismo
#'     número de elementos o, si difieren, uno (numerador o
#'     denominador) debe ser un escalar. Donde el resultado sea
#'     infinito, devuelve NA.
#' @param x numeric: numerador
#' @param y numeric: denominador
#' @param dec integer o NA: número de decimales del resultado; si NA
#'     (valor por omisión), no redondea
#' @return double o NA
#' @examples
#' dividir(2, 3)
#' dividir(1:3, 3)
#' dividir(1:3, 3:1)
#' \dontrun{
#' dividir(1:3, 1:2) #-> error
#' }
#' @export
#' @author eddy castellón
dividir <- function(x = double(), y = double(), dec = NA_integer_) {
    stopifnot(
        "arg. no numerico" = filled_num(x) && filled_num(y),
        "arg. difiere longitud" = length(x) == length(y) ||
            (length(x) > 1 && length(y) == 1) ||
            (length(x) == 1 && length(y) > 1),
        "arg. dec no válido" = is_scalar(dec) && is.numeric(dec)
    )

    r <- x / y
    r[is.infinite(r)] <- NA_real_

    if (!is.na(dec)) {
        r <- round(r, dec)
    }

    r
}

#' Porcentaje
#' @description Porcentaje c.r.a base
#' @param x numeric
#' @param base numeric: base del porcentaje; por omisión, la suma de
#'     los datos ignorando los NA
#' @param dec integer: número de decimales; por omisión, cero
#' @param x100 logical: resultado es dado multiplicado por 100 (TRUE)
#'     o por 1 (FALSE). Por omisión, TRUE.
#' @return numeric o NA
#' @examples
#' pct(2, 3)
#' pct(1:3, 3)
#' pct(1:3)
#' pct(1:3, 1:3)
#' \dontrun{
#' pct(1:3, 3:2) #-> error
#' }
#' @export
#' @author eddy castellón
pct <- function(x = numeric(), base = numeric(), dec = 0L,
                x100 = TRUE) {
    stopifnot("arg. inadmisible" = filled_num(x) &&
        is.numeric(base) && is.numeric(dec) && is.logical(x100))

    if (is_vacuo(base)) {
        base <- sum(x, na.rm = TRUE)
    }

    factor <- ifelse(x100, 100L, 1L)
    if (all(base == 0 | is.na(base))) {
        pp <- vector("numeric", length(x)) + NA_real_
        warning("base es igual a cero o NA", call. = FALSE)
    } else {
        pp <- round(factor * dividir(x, base, NA), dec)
    }
    pp
}

#' Porcentaje-grupos
#' @description Contribución al total del grupo
#' @param x numeric: datos
#' @param by numeric o character o factor: variable agrupamiento; por
#'     omisión, sin agrupar
#' @param dec integer: decimales; 0 por omisión
#' @return list
#' @examples
#' pct_grupo(1:4, c("a", "a", "b", "b"))
#' @export
pct_grupo <- function(x = numeric(), by = numeric(), dec = 0L) {
    stopifnot(
        "arg. inadmisible" = filled_num(x),
        "arg. inadmisible" = is_scalar0(by) ||
            length(by) == length(x),
        "arg. inadmisible" = filled_num(dec) &&
            is_scalar(dec)
    )

    if (is_scalar0(by)) {
        list(pct(x, dec = dec))
    } else {
        tapply(x, by, pct, dec = dec, simplify = FALSE)
    }
}

#' buscar-remplazar
#' @description Busca elementos de un vector en otro, y remplaza con
#'     otro donde haya un match.
#' @details Hace un match del arg. 'busca' en el arg. 'buscaen'. Los
#'     elementos del arg. 'remplazo' donde la función match no
#'     devuelva NA, remplazan los correspondientes del arg. 'x'. El
#'     número de elementos del arg. 'x' debe ser igual al del
#'     arg. 'busca', y los del arg. 'buscaen' a los del
#'     arg. 'remplazo'. El modo del arg. 'x' debe ser igual al de
#'     'remplazo' (excepto cuando arg. 'x' es objeto NULL), y el modo
#'     del arg. 'busca' al de 'buscaen'.
#'
#'     El arg. 'x' es NULL por omisión. En este caso arg. 'x' se
#'     inicializa a vector con igual número de elementos de
#'     arg. 'busca' y mismo modo que arg. 'remplazo'. Los elementos de
#'     arg. 'x' son ceros o NA, según lo diga el arg. 'toNA'. Son NA
#'     si arg. 'toNA' es TRUE (por omisión).
#' @param x vector o NULL (por omisión)
#' @param busca vector con los elementos a buscar
#' @param buscaen vector donde se buscan los elementos
#' @param remplazo vector con los elementos que remplazarán los
#'     correspondientes en 'x'
#' @param msg TRUE por omisión; FALSE suprime mensajes de advertencia
#' @param toNA logical: TRUE por omisión.
#' @return vector
#' @examples
#' x <- letters[1:4]
#' y <- 8:1
#' z <- letters[1:8]
#' (remplazar(busca = x, buscaen = z, remplazo = y))
#' w <- 1:4
#' (remplazar(w, x, z, y))
#' @export
#' @author eddy castellón
remplazar <- function(x = NULL, busca, buscaen, remplazo,
                      msg = TRUE, toNA = TRUE) {
    stopifnot(exprs = {
        "arg. incompat." <- filled(buscaen) && filled(remplazo) &&
            length(buscaen) == length(remplazo)
        "arg. incompat." <- filled(busca) &&
            mode(busca) == mode(buscaen)
        "arg. x inadmisible" <- is.null(x) ||
            (length(x) == length(busca) &&
                mode(x) == mode(remplazo))
    })

    if (is.null(x)) {
        x <- vector(mode(remplazo), length(busca))
        if (toNA) {
            is.na(x) <- seq_along(x)
        }
    }

    mm <- match(busca, buscaen)

    ii <- !is.na(mm)
    if (any(ii)) {
        x[ii] <- remplazo[mm[ii]]
        if (msg) {
            message("... ", sum(ii), " remplazos !!!")
        }
    } else {
        if (msg) {
            message("... ningún remplazo !!!")
        }
    }

    invisible(x)
}

#' Crear vector
#' @description Crear vector atómico con elementos tomados de otro
#'     vector
#' @details Aplica la función remplazar con el param. x igual a NULL,
#'     para crear un vector del mismo tipo que el del param.
#'     remplazo, y lo llena con elementos de él cuando hay un match de
#'     los param. busca y buscaen. Si quedaran datos NA porque
#'     elementos de "busca" no se encuentran en "buscaen", los
#'     sustituye por el valor pasado en el parámetro si_na.
#' @seealso remplazar
#' @param busca character o numeric: datos a buscar
#' @param buscaen character o numeric: donde buscar
#' @param remplazo vector atómico: remplazo de los encontrados
#' @export
#' @examples
#' iniciar_vec(c(1, 3, 2), c(2, 1), c(10, 20), -1) #-> c(20, -1, 10)
iniciar_vec <- function(busca, buscaen, remplazo, si_na = NA) {
    w <- remplazar(NULL, busca, buscaen, remplazo, msg = FALSE)
    if ( !is.na(si_na) ) {
        w[is.na(w)] <- si_na
    }
    w
}

#' Orden conforme
#' @description Pone los elementos de un vector en el mismo orden de
#'     los elementos de otro vector
#' @param x vector atómico
#' @param y vector atómico que contiene a los elementos en x
#' @return vector
#' @export
#' @examples
#' ordenar_conforme(c(2, 3, 1), c(1, 2, 3, 4)) #-> c(1, 2, 3)
#' ordenar_conforme(c(2, 3, 1), c(3, 1, 2, 4)) #-> c(3, 1, 2)
#' ordenar_conforme(c(2, 0, 1), c(1, 2, 3)) #-> c(1, 2)
ordenar_conforme <- function(x, y) {
    m <- match(y, x) %>% Filter(Negate(is.na), .)
    if (length(m) < length(x)) {
        warning("\n... algunas variables NO ESTÁN en la referencia !!!",
                call. = FALSE)
    }
    x[m]
}

#' Redondear
#' @description Redondear un vector de números de suerte que su suma
#'     sea igual a un número dado
#' @details Implementa algoritmo de Dorfleitner & Klein (Statistical
#'     Papers 40:143-157; 1999)
#' @param x numeric: números a redondear
#' @param suma numeric: la suma de los números redondeados (1 por
#'     omisión)
#' @param metodo character: uno de "webster" (default), "adams",
#'     "jefferson" (suficiente la primera letra)
#' @param q numeric: número entre 0 y 1. Es opcional. Si \code{q = 0}
#'     es lo mismo que método "adams"; q = 1 es "jefferson"; q = 0.5
#'     es "webster"
#' @param eps numeric: márgen de error (entre 0 y 1) del resultado;
#'     por omisión, 0.001.
#' @return integer
#' @export
#' @examples
#' redondear(c(1.7, 1.5, 1.0, 2.6), 8) # -> [1] 2 2 1 3
#' redondear(c(1.7, 1.5, 1.0, 2.6), 7) # -> [1] 2 1 1 3
#' redondear(c(1.7, 1.5, 1.0, 2.6), 7, q = 0.3) # -> [1] 2 1 1 3
#' redondear(c(1.7, 1.5, 1.0, 2.6), 7, q = 0.7) # -> [1] 2 2 1 2
redondear <- function(x, suma = 100, metodo = "webs",
                      q = double(), eps = 1e-3){

    ## arithmetic-mean methods
    ##     Adams: q = 0
    ##   Webster: q = 0.5
    ## Jefferson: q = 1
    ## 0 < q < 1
    if (!length(q)) {
        q <- switch(metodo, webs = 0.5,
                    jeff = 1.0,
                    adam = 0.0)
    }

    n <- length(x)
    wi <- x / sum(x)

    ## num. iter. = n / 2
    nu <- suma + n * (q - 0.5) ## multiplier "óptimo"
    ni <- nu * wi
    sp <- seq(floor(min(ni)) - 1L, ceiling(max(ni))) + q

    ## sign post
    sp  <- seq(min(ni) - 1L, max(ceiling(ni)) + 1L) + q
    ni <- floor(ni + 1.0 - q)

    zi <- integer(n)
    dt <- sum(ni) - suma
    while (abs(dt) <= eps) {
        if (dt <= 0) {
            dd <- ni / wi
            sm <- min(dd)
            if (dt == 0) {
                ##it <- mxit
                sx <- max(dd)
                if (sx == sm) {
                    zi <- zi + 1L * (dd == sm) - 1L * (dd == sx)
                }
            } else {# dt<0
                nn <- which(dd == sm)
                ni[nn] <- ni[nn] + 1L
            }
        } else {
            dd <- (ni - q) / wi
            sx <- max(dd)
            nn <- which(dd == sx)
            ni[nn] <- ni[nn] - 1L
        }
        dt <- sum(ni) - suma
    }
    ni
}

#' Cuantiles
#' @description Cuantiles
#' @details Es una especialización de la función quantile. Las
#'     probabilidades corren al intervalo fijo indicado en el
#'     parámetro «cuan», y permite incluir en el cálculo sólo datos
#'     mayores que 0 y no NA (parámetro «mayor_que_0»).
#' @seealso quantile, deciles, quintiles
#' @param x numeric y con más de un elemento
#' @param cuan numeric: intervalo fijo
#' @param mayor_que_0 logical: excluir datos menor o igual a cero?. TRUE por
#'     defecto.
#' @param ... adicionales pasados a función quantile
#' @return NA o numeric
#' @export
#' @examples
#' cuantiles(sample(1:10, 100, replace = TRUE), cuan = 0.1)
cuantiles <- function(x, cuan = 0.25, mayor_que_0 = TRUE, ...) {
    stopifnot("arg. x no válido" = (!is_scalar0(x)) && is.numeric(x))

    if ( mayor_que_0 ) {
        x <- x[es_pos(x)]
    }

    if ( !is_empty(x) ) {
        q <- quantile(x, probs = seq(0, 1, cuan), ...)
    } else {
        q <- NA_real_
    }
    return(q)
}

#' Deciles
#' @description Deciles de una variable numérica
#' @param x numeric
#' @param ... parámetros adicionales pasados a \code{cuantiles}
#' @seealso cuantiles
#' @return numeric o NA_real_
#' @export
deciles <- purrr::partial(cuantiles, cuan = 0.1)

#' Quintiles
#' @description Quintiles de una variable numérica
#' @param x numeric
#' @param ... parámetros adicionales pasados a \code{cuantiles}
#' @seealso cuantiles
#' @return numeric o NA_real_
#' @export
quintiles <- purrr::partial(cuantiles, cuan = 0.2)

## -- validación

#' Positivo
#' @description Es mayor que cero
#' @param x numeric
#' @return logical
#' @export
es_pos <- function(x) {
    na0(x) > 0
}

#' Si-entonces
#' @description Si los datos de una variable son mayor que cero, los
#'     correspondientes de otra también lo son (condicional)
#' @param x numeric: antecedente
#' @param y numeric: consecuente
#' @return logical
#' @export
ypos_si_xpos <- function(x, y) {
    es_pos(y) | !es_pos(x)
}

#' equivalencia
#' @description Si los datos de una variable son mayor que cero, los
#'     correspondientes de otra también los son, y viceversa
#' @param x numeric
#' @param y numeric
#' @return logical
#' @export
ypos_ssi_xpos <- function(x, y) {
    ypos_si_xpos(x, y) & ypos_si_xpos(y, x)
}

#' Datos fuera de rango
#' @description Identifica los datos que están fuera del rango especificado
#'     por los límites «inf» y «sup». Los límites no son incluidos en el rango.
#' @details Los límites del intervalo, si no son pasados como argumentos, se
#'     construyen multiplicando una referencia (parámetro «ref») por un factor
#'     dependiente de una fracción (parámetro «frac»). El factor para calcular
#'     el límite superior es (1 + frac), y para el inferior, (1 - frac). Los
#'     datos NA se excluyen y se devuelven como tales, de modo que al usar el
#'     resultado debe tomarse en cuenta esa posibilidad.
#' @param x numeric: los datos
#' @param ref numeric: la referencia o punto central; por omisión, el promedio
#'     de los datos
#' @param frac numeric: valor mayor que cero; por omisión, 0.1
#' @param inf numeric: límite inferior del rango
#' @param sup numeric: límite superior del rango
#' @return logical
#' @export
#' @examples
#' x <- 1:5
#' fuera_de_rango(x, inf = 2, sup = 4)
#' fuera_de_rango(x, inf = 1, ref = 2)
#' fuera_de_rango(x, ref = 2, frac = 0.9)
#' fuera_de_rango(x, ref = median(x), frac = 1.5)
#' fuera_de_rango(x, ref = mean(x, na.rm = TRUE),
#'                sup = ref + 2 * sd(x, na.rm = TRUE)))
fuera_de_rango <- function(x, ref = mean(x, na.rm = TRUE), frac = 0.1,
                           inf = ref * (1.0 - frac),
                           sup = ref * (1.0 + frac)) {
    stopifnot("arg. x inadmisible" = filled_num(x) &&
                                     is.numeric(ref) && is.numeric(sup) &&
                                     is.numeric(inf))
    na <- is.na(x)
    x[na] <- inf + (sup - inf) / 2.0
    ne <- !num_entre(x, inf, sup)
    ne[na] <- NA
    return(ne)
}
