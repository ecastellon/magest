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

#' Nombre propio
#' @description pone en mayúsculas las primeras letras de las palabras
#' @param x palabra
#' @examples
#' a_propio('juan calero') -> 'Juan Calero'
#' @export
a_propio <- function(x = character()) {
    stopifnot("arg. x inválido" = filled_char(x))
    gsub("\\b([a-z])","\\U\\1", tolower(x), perl = TRUE)
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
#' @param msg logical: si \code{TRUE} produce mensaje de cuántos no
#'     hacen "match"
#' @param sinpar logical: si \code{TRUE} agrega el atributo "sinpar"
#'     al resultado; \code{FALSE} por defecto.
#' @return integer
#' @examples
#' casar(x, y, msg = FALSE)
#' casar(w, x, y, z)
casar <- function(..., msg = TRUE, sinpar = FALSE) {
    x <- eval(substitute(alist(...)))

    n <- length(x)
    if (n > 2) {
        if (!es_par(n)) {
            warning("... número de argumentos no es par !!!",
                    call. = FALSE)
        }
        m <- seq_len(n)
        k <- n %/% 2
        m1 <- head(m, k)
        m2 <- tail(n, n - k)
        x <- list(x = Reduce(interaction, x[m1]),
                  table = Reduce(interaction, x[m2]))
    } else {
        if (n == 2) {
            names(x) <- c("x", "table")
        } else {
            x <- list(x = x[[1]], table = x[[1]])
        }
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
#' @description Operado infijo %in% como función
#' @param x vector
#' @param y vector
#' @return logical
#' @export
en <- function(x, y) match(x, y, nomatch = 0) > 0

#' NA a cero
#' @description Convierte a 0 los elementos NA de un vector de modo
#'     numérico
#' @param x numeric
#' @return numeric
#' @examples
#' na0(c(1:3, NA_integer_))
#' @export
na0 <- function(x) {
    if (is.numeric(x)) {
        x[is.na(x)] <- ifelse(typeof(x) == "integer", 0L, 0.0)
    }
    invisible(x)
}

#' Cero - NA
#' @description Convierte a NA los datos igual a cero
#' @param x numeric
#' @return numeric
#' @export
cero_na <- function(x) {
    stopifnot("arg. x inválido" = is.numeric(x))
    na <- ifelse(typeof(x) == "integer", NA_integer_, NA_real_)
    x[x == 0] <- na
    invisible(x)
}

#' Número-entre
#' @description Comprueba si un número está entre los límites de un
#'     intervalo
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
    stopifnot(
        "arg. x inválido" = filled_num(x),
        "arg. x1 inválido" = filled_num(x1),
        "arg. x2 inválido" = filled_num(x2),
        "args. x, x1 incomp" = length(x) == length(x1),
        "args. x, x2 incomp" = length(x) == length(x2)
    )

    tf <- x > x1 & x < x2
    if (inclusive) {
        tf <- tf | x == x1 | x == x2
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
dividir <- function(x = double(), y = double(), dec = NA) {
    stopifnot(
        "arg. no numerico" = filled_num(x) && filled_num(y),
        "arg. difiere longitud" = length(x) == length(y) ||
            (length(x) > 1 && length(y) == 1) ||
            (length(x) == 1 && length(y) > 1)
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
#' @param base numeric: base del porcentaje; por omisión,
#'     la suma de los datos ignorando los NA
#' @param dec integer: número de decimales; por omisión, cero
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
pct <- function(x = numeric(), base = numeric(), dec = 0L) {
    stopifnot("arg. inadmisible" = filled_num(x) &&
        is.numeric(base))

    if (is_vacuo(base)) {
        base <- sum(x, na.rm = TRUE)
    }

    if (all(base == 0 || is.na(base))) {
        pp <- vector("numeric", length(x)) + NA_real_
        warning("base es igual a cero o NA")
    } else {
        pp <- round(100 * dividir(x, base, NA), dec)
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
#' @return integer
#' @export
#' @examples
#' redondear(c(1.7, 1.5, 1.0, 2.6), 8) # -> [1] 2 2 1 3
#' redondear(c(1.7, 1.5, 1.0, 2.6), 7) # -> [1] 2 1 1 3
#' redondear(c(1.7, 1.5, 1.0, 2.6), 7, q = 0.3) # -> [1] 2 1 1 3
#' redondear(c(1.7, 1.5, 1.0, 2.6), 7, q = 0.7) # -> [1] 2 2 1 2
redondear <- function(x, suma = 100, metodo = "webs",
                      q = double()){

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
    while (dt != 0) {
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
