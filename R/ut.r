# -*- coding: utf-8 -*-

##--- utilidad general --
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
#' @param x
#' @return logical
#' @keywords internal
is_scalar <- function(x) {
    length(x) == 1L
}

#' length
#' @description vector has length greater than zero?
#' @param x vector
#' @return logical
#' @keywords internal
#' @author eddy castellón
filled <- function(x) {
    length(x) > 0
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

## --- data.frame ---
#' @export
quitar_0 <- function(x, excepto) UseMethod("quitar_0")

#' quitar ceros
#' @description Elimina las filas de un data.frame que tienen cero en
#'     todas las columnas de modo numeric (excepto las indicadas).
#' @param df data.frame
#' @param excepto nombre (character) o posición (numeric) de las
#'     columnas de modo numeric que serán ignoradas
#' @return data.frame
#' @examples
#' quitar_0(ww, excepto="quest") -> data.frame sin los registros donde
#'     todas las columnas (exceptuando la nombrada "quest") llevan
#'     dato = 0
#' @export
#' @author eddy castellón
quitar_0.data.frame <- function(df, excepto = character()) {
    stopifnot("arg. df inadmisible" = inherits(df, "data.frame"),
              "arg. excepto inadmisible" = is.character(excepto) ||
                  is.numeric(excepto))

    ii <- vapply(df, is.numeric, TRUE, USE.NAMES = FALSE)
    if (any(ii)) {
        nm <- names(df)
        if (filled_num(excepto)) {#supone en rango
            excepto <- as.integer(excepto) %>%
                intersect(seq_along(nm))
            excepto <- nm[excepto]
            if (is_vacuo(excepto)) {
                warning("\n... no filtrar ... arg. excep. fuera rango",
                        call. = FALSE)
                excepto <- nm #no filtrar datos
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

##--- strings ---

#' fabrica código
#' @description Produce función que genera nombres de variables
#'     encuesta
#' @param x parámetro para la parte entera del código
#' @param di numeric: dígitos que componen el código; 3 por defecto
#' @return función
#' @seealso codigo
#' @export
#' @examples
#' ff <- codigo_fac(di = 4)
#' ff(4) #-> "c0004"
#' ff(c(20, 100)) #-> c("c0020", "c0100")
codigo_fac <- function(x, di = 3){
    function(x) sprintf(paste0("%s%0", di, "i"), "c", x)
}

#' Variable-nombre
#' @description Produce nombre genérico de variable usadas en encuesta
#' @details Crea un objeto character con prefico "c", seguido por un
#'     entero (parámetro "x") antecedido por suficientes ceros para
#'     llenar tantos espacios como indique el parámetro "di"
#' @param x integer: parte entera del código
#' @param di integer: número de dígitos en el código; 3 por defecto
#' @return character
#' @export
#' @examples
#' codigo(100, 4) -> "c0100"
codigo <- function(x, di = 3){
    sprintf(paste0("%s%0", di, "i"), "c", x)
}

##--- files ---

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

##--- misc ---

#' NA a cero
#' @description Convierte a 0 los elementos NA de un vector de modo
#'     numérico
#' @param x numeric
#' @return numeric
#' @export
na0 <- function(x) {
    if (is.numeric(x)) {
        x[is.na(x)] <- ifelse(typeof(x) == "integer", 0L, 0.0)
    }
    invisible(x)
}

