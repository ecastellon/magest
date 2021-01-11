# -*- coding: utf-8 -*-

##--- funciones para hacer estimaciones --

#' factores
#' @description Cálculo del factor de expansión
#' @details El data.frame asociado al parámetro "x" tiene las columnas
#'     que corresponden a las observaciones (cuestionarios): el
#'     departamento (integer) y estrato (integer) a los que está
#'     asignada la unidad de producción, y la superficie (double o
#'     numeric). Las columnas deben aparecer en ese orden: código de
#'     departamento seguido de el del estrato, y después la columna
#'     con la superficie. Si no están en ese orden, se utiliza el
#'     parámetro "cob" para indicar su posición. Por ejemplo, cob =
#'     c(2, 5, 1) indicaría que el código de departamento está en la
#'     segunda columna, la del estrato en la quinta, y la superficie
#'     en la primera.
#'
#'     El data.frame vinculado al parámetro "dfe" trae los datos
#'     de los estratos y el tamaño de la muestra asignada a cada
#'     uno. La posición de las columnas en el orden siguiente: la
#'     primera con la identificación del departamento (integer),
#'     seguida de la del estrato (integer), la superficie del estrato
#'     (double o numeric) y el tamaño de la muestra (integer). Si no
#'     están ese orden, se utiliza el parámetro "ces" para indicarlo,
#'     como en el caso del data.frame de las observaciones.
#'
#'     El parámetro "tamu" es para calcular el factor cuando no se ha
#'     terminado de levantar toda la muestra, considerando el número
#'     de las presentes como el tamaño de muestra. El resultado es
#'     estrictamente válido sólo cuando las presentes (o las que
#'     faltan) sea una muestra aleatoria de la muestra original. La
#'     alternativa es considerar las que faltan como unidades
#'     "no contactadas", y ponderar el factor por no respuesta. Pero
#'     de nuevo, el factor que resulte sería válido sólo si los
#'     "no contactos" estuvieran distribuidos aleatoriamente.
#' 
#' @param x data.frame con los datos de cuestionarios
#' @param dfe data.frame con los datos de los estratos
#' @param cob integer con los números de las columnas del data.frame x
#'     utilizadas en el cálculo: departamento, estrato, superficie;
#'     por omisión c(1, 2, 3).
#' @param ces integer con los números de las columnas del data.frame
#'     dfs, utilizadas en el cálculo: departamento, estrato,
#'     sup. estrato, puntos en estrato; por omisión c(1, 2, 3, 4)
#' @param tamu logical: calcular el número de observaciones en los
#'     estratos y utilizarlos como el tamaño de la muestra?; FALSE por
#'     omisión
#' @param dec integer: número de decimales de los resultados
#' @return double o NULL
#' @examples
#' \dontrun{w_pr(dfcues, dfestratos)}
#' @export
w_pr <- function(x, dfe, cob = seq.int(3), ces = seq.int(4),
                     tamu = FALSE, dec = 6L) {
    
    ce <- c("dpt", "est", "sup", "tam")
    co <- c("dpt", "est", "sup")
    
    nx <- length(x)
    nd <- length(dfe)

    ## test argumentos
    ok <- length(ces) == length(ce) &&
        length(cob) == length(co) &&
        min(cob) > 0 && max(cob) <= nx &&
        min(ces) > 0 && max(ces) <= nd &&
        anyDuplicated(cob) == 0 && anyDuplicated(ces) == 0 &&
        nx >= length(co) && nd >= length(ce)

    if (ok) {
        ces <- setNames(ces, ce)
        cob <- setNames(cob, co)

        ix <- interaction(x[[cob["dpt"]]], x[[cob["est"]]])
        ie <- interaction(dfe[[ces["dpt"]]], dfe[[ces["est"]]])
        mm <- match(ix, ie)
        ok  <- !is.na(mm)
    } else {
        message("\n... inconsistencias en los argumentos !!!")
    }
    
    fx  <- NULL

    if (ok) {
        if (tamu) {
            nn <- table(ix)
            ##?? si ceros en algunas celdas
            dfe[[ces["tam"]]] <- remplazar(integer(length(ie)),
                                           ie, names(nn),
                                           as.vector(nn))
        }
        
        pp <- dfe[[ces["sup"]]] / dfe[[ces["tam"]]] #prom por punto
        fx <- round(pp[mm] / x[[cob["sup"]]], dec) #factor
        if (any(ii <- !is.finite(fx))) {
            warning(paste("\n... sin factor", sum(ii)), call. = FALSE)
            fx[ii] <- NA_real_
        }
    } else {
        message(paste("\n... inconsistencia de código dpto o estrato en",
                      "los data.frame !!!"))
    }
    
    return(fx)
}

#' no respuesta
#' @description Calcula ponderación por no respuesta.
#' @details Es el resultado de dividir el número de unidades de
#'     muestreo que son elegibles y que respondieron o no la
#'     entrevista, entre el número de estas que respondieron. La
#'     función también se puede utilizar para calcular la ponderación
#'     por "elegilibilidad desconocida". Para este caso, el parámetro
#'     "qnr" lleva los códigos de "elegilibilidad desconocida" (o
#'     "no contacto") y "qsr" los de "elegilibilidad conocida"
#'     (respondieron + no respondieron + no elegible conocido).
#'
#'     Una unidad es "elegible" si es miembro de la población
#'     muestreada (en una encuesta agrícola por ejemplo, las unidades
#'     de producción no agrícola serían "no elegibles"). Es posible
#'     que al finalizar la encuesta, la condición de elegilibilidad de
#'     algunas unidades no sea conocida (generalmente porque no se
#'     pudo "hacer contacto" con ella) lo que las coloca en la
#'     categoría de "elegilibilidad desconocida".
#'
#'     Estas ponderaciones son una aproximación al inverso de la
#'     probabilidad de respuesta por el que se deben multiplicar los
#'     factores de expansión de la encuesta, a fin de reducir el sesgo
#'     de las estimaciones debido a ese problema. La aproximación
#'     supone que esa probabilidad es igual para todas las unidades
#'     miembros de una misma clase (por ejemplo, todas las fincas en
#'     un mismo municipio; o todos los productores con tales o cuales
#'     características).
#' 
#' @param x numérico o caracter: códigos de control del llenado del
#'     cuestionario
#' @param cnr numérico o caracter: clases para agrupar los datos
#' @param qsr mismo tipo de x: códigos tipo "sí responde"
#' @param qnr mismo tipo de x: códigos tipo "rechazo" o "no responde"
#' @param dec integer: número de decimales al que se redondea el
#'     resultado (4 por omisión).
#' @return vector real o NULL
#' @examples
#' clase <- c(1, 2, 1, 2, 2, 2)
#' ccues <- c(1, 1, 2, 1, 1, 1)
#' w_nr(ccues, clase, qsr = 1, qnr = 2)
#' @export
#' @author eddy castellón
w_nr <- function(x, cnr, qsr, qnr, dec = 4L) {

    qq <- c(qnr, qsr)
    cq <- unique(x)

    ## no exigir all(cq %in% qq) deja oportunidad otros
    ## códigos de control; p.ej. elegibles no contactados
    ok <- length(x) == length(cnr) && length(qsr) &&
        length(qnr) && all(qq %in% cq) &&
        anyDuplicated(qq) == 0

    if (ok) {
        cn <- factor(cnr)
        nr <- tapply(x %in% qnr, cn, sum) #no resp
        sr <- tapply(x %in% qsr, cn, sum) #reponden
        ii <- sr > 0
        fc <- round(nr[ii] / sr[ii], dec)
        
        wr <- 1.0 + remplazar(double(length(x)), fac2char(cn),
                              names(fc), fc, msg = FALSE)
    } else {
        wr <- NULL
        message("\n... inconsistencias en los argumentos !!!")
    }

    return(wr)
}

#' Puntos UP
#' @description Prepara el data.frame con los datos de departamento,
#'     municipio y estrato de los puntos en la muestra
#' @details Hay un archivo «maestro» construido durante el diseño de
#'     la muestra (arg. parám. "dfp") con los datos de todos los
#'     puntos de la encuesta; este contiene: «nombre» o «id» del
#'     punto, departamento, municipio y estrato al cual está asignado
#'     cada punto. Si hubiera algún un esquema de rotación de la
#'     muestra y no todos los puntos forman parte de la actual
#'     selección, el arg. al parám. "dfp" debe haber sido previamente
#'     filtrado para que contenga sólo los que correspondan. Esto
#'     permite validar los datos por esa variable.
#' 
#'     Esta función lo que hace es adjuntar a los registros del
#'     data.frame que se pasa como arg. al parám. "dfq" los datos de
#'     departamento, municipio y estrato que están en el
#'     arg. parám. "dfp".
#' @param dfq data.frame con los datos de las observaciones leídas de
#'     la base de datos
#' @param dfp data.frame "maestro" con los datos de la ubicación y el
#'     estrato al cual están asignados los puntos de la muestra actual
#'     (la del mes).
#' @param cues character: columna con los números de cuestionario o
#'     punto. El tipo de datos en la columna debe ser integer.
#' @param cdpt character: nombre columna con los códigos del
#'     departamento
#' @param cmun character: nombre columna con el código del municipio
#' @param cest character: nombre columna con el código del estrato
#' @return data.frame
#' @examples
#' \dontrun{
#' df_pto(dfcues, dfpuntos)
#' }
#' @export
df_pto <- function(dfq, dfp, cues = "quest", cdpt = "dpt",
                   cmun = "mun", cest = "estrato") {
    stopifnot(exprs = {
        inherits(dfq, "data.frame")
        inherits(dfp, "data.frame")
        all(is.element(c(cues, cdpt, cmun, cest), names(dfp)))
        is.element(cues, names(dfq))
        typeof(dfq[[cues]]) == typeof(dfp[[cues]])
    })
    
    cc <- names(dfq)
    nn <- nrow(dfq)

    dfq %<>% inner_join(dfp, by = cues)
    nr <- nrow(dfq)
    stopifnot("sin datos" = nr > 0)

    nn <- nn - nr
    if (nn > 0) {
        warning(paste("... diferencia de", nn,
                      "registros; cuestionarios no válidos !!!"),
                call. = FALSE)
    }

    message("\n... cuestionarios válidos: ", nr)
    ## originales más las que no estaban en dfq
    ## cc <- c(cc, setdiff(c(cdpt, cmun, cest), names(dfq)))
    invisible(select(dfq, one_of(c(cues, cdpt, cmun, cest))))
}

#' Superficie UP
#' @description Prepara el data.frame con datos de uso de la tierra y
#'     control de cuestionario.
#' @details La función produce el data.frame con los datos de
#'     superficie y control de cuestionario, y los registros
#'     debidamente duplicados en el caso de cuestionarios "copia". El
#'     data.frame de cuestionarios es el referido con el parámetro
#'     "dfq". Si este no contiene los datos de superficie, estos son
#'     tomados del data.frame referido con el parámetro "dft", que es
#'     opcional.
#' @param dfq data.frame con los datos de cuestionario, de origen de
#'     duplicados, control de cuestionario y, si acaso, superficie
#' @param cues character: nombre columna con los números de
#'     cuestionario
#' @param cdup character: nombre columna con el origen de las
#'     duplicadas
#' @param ccon character: nombre columna con el código de control
#' @param qres códigos de control de los cuestionarios con datos de
#'     superficie (p.ej. de cuestionario completo o incompleto)
#' @param csup character: nombre columna con los datos de superficie
#' @param dft data.frame con los datos de cuestionario y superficie,
#'     si es que ese dato no está en el data.frame referenciado con el
#'     parámetro "dfq"
#' @return data.frame
#' @examples
#' \dontrun{
#' df_sup(x, "quest", "duplicada", "c040", "c5000", c(1, 3), y)
#' df_sup(x, cues = "quest", cdup = "duplicada", csup = "c040",
#'              ccon = "c5000", qres = c(1, 3))}
#' @export
df_sup <- function(dfq, cues = "quest", cdup = "copiade",
                   ccon = "c5000", qres, csup, dft) {

    stopifnot(exprs = {
        inherits(dfq, "data.frame")
        all(is.element(c(cues, cdup, ccon), names(dfq)))
        
        ifelse(missing(dft), is.element(csup, names(dfq)),
               inherits(dft, "data.frame") &&
               all(is.element(c(cues, csup), names(dft))))
    })
    
    ## trasladar a dfq las columnas de dft que no están
    if (missing(dft)) {#asegurar todos los datos en dfq
        cc <- setdiff(c(csup, cues, cdup), names(dfq))
        stopifnot("faltan columnas" = filled(cc))
    } else {
        cc <- setdiff(names(dft), names(dfq))
        nn <- nrow(dfq)
        dfq  <- select(dft, one_of(c(cues, cc))) %>%
            inner_join(dfq, ., by = cues)
        
        if (nn <- abs(nn - nrow(dfq)) != 0) {
            warning(paste("\n... los data.frames difieren en", nn,
                          " filas !!!"), call. = FALSE)
            stopifnot("sin datos dfq" = nn > 0)
        }
    }

    ## copiar sup y cod. control cuest., de origen a duplicada
    dfq[[csup]] <- duplicar_v(dfq[[csup]], dfq[[cues]], dfq[[cdup]])
    dfq[[ccon]] <- duplicar_v(dfq[[ccon]], dfq[[cues]], dfq[[cdup]])

    ## poner sup a cero en las no respuesta y no elegibles
    ii <- dfq[[ccon]] %in% qres
    dfq[!ii, csup] <- 0.0

    invisible(select(dfq, one_of(c(cues, csup))))
}

#' Factor data
#' @description Prepara data.frame para calcular factor de expansión
#' @details Prepara el data.frame con los datos necesarios para llamar
#'     las funciones w_pr y w_nr que calculan el factor de expansión y
#'     los ajustes por no respuesta. El data.frame que sirve de base
#'     es el referido con el parámetro "dfq". Los arg. a los
#'     parámetros "dft" y "dfp" son opcionales si es que arg. "dfq"
#'     contiene todas las columnas en la lista de parámetros. Si se
#'     pasa arg. al parámetro "dfp", este debe corresponder a los
#'     puntos de la muestra actual; es decir, debe haberse filtrado
#'     antes si hubiera algún esquema de rotación de la muestra.
#'
#'     La función llama las funciones preparar_pto y preparar_sup.
#' @param dfq data.frame con los datos de código de cuestionario,
#'     control de llenado y control de copia
#' @param dfp data.frame con los datos de departamento, municipio y
#'     estrato a los que están asignados los puntos de la muestra. Es
#'     opcional (vea detalles).
#' @param qres character o numeric: códigos de control de llenado de
#'     los cuestionarios que pueden tener datos de superficie mayor
#'     que cero; p.ej. los con código "completo" o "incompleto"
#' @param ccon character: nombre de la columna con los datos de código
#'     de control de llenado. Por omisión "c5000".
#' @param cues character: nombre de columna con el número de
#'     cuestionario; por omisión, "quest"
#' @param cdup character: nombre de columna con el número de
#'     cuestionario "origen". El dato debe ser NA si el cuestionario
#'     no es "copia", o el número del cuestionario "origen" en caso
#'     contrario. Por omisión, "copiade".
#' @param cdpt character: nombre de la columna con los datos de los
#'     departamentos a los que están asignados los cuestionarios. Por
#'     omisión "dpt".
#' @param cmun character: nombre de la columna con el código del
#'     municipio al que está asignado el cuestionario. Por omisión,
#'     "mun"
#' @param cest character: nombre de la columna con el código del
#'     estrato. Por omisión, "est".
#' @param csup numeric: nombre de la columna con los datos de
#'     superficie de la unidad de producción. Por omisión, "sup"
#' @param dft data.frame con los datos de superficie de la unidad de
#'     producción. Es opcional (vea detalles)
#' @return data.frame
#' @seealso preparar_pto, preparar_sup
#' @examples
#' \dontrun{
#' df_fxp(dfobs, dfpun, c("completo", "incompleto"))
#' df_fxp(dfobs, dfpun, c(1, 3))
#' }
#' @export
#' 
df_fxp <- function(dfq, dfp, qres, ccon = "c5000", cues = "quest",
                   cdup = "copiade", cdpt = "dpt", cmun = "mun",
                   cest = "est", csup = "sup", dft) {
    ## revisión
    stopifnot(exprs = {
        inherits(dfq, "data.frame")
        all(is.element(c(cues, cdup, ccon), names(dfq)))
        (filled_num(qres) || filled_char(qres)) &&
            all(is.element(qres, unique(dfq[[ccon]])))
        
        ifelse(missing(dfp),
               all(is.element(c(cdpt, cmun, cest), names(dfq))),
               inherits(dfp, "data.frame") &&
               all(is.element(c(cues, cdpt, cmun, cest), names(dfp))))
        
        ifelse(missing(dft), is.element(csup, names(dfq)),
               inherits(dft, "data.frame") &&
               all(is.element(c(cues, csup), names(dft))))
    })
    
    nc <- names(dfq)

    cc <- c(cues, cdpt, cmun, cest)
    if (all(is.element(cc, nc))) {
        wp <- select(dfq, one_of(cc))
    } else {
        wp <- df_pto(dfq, dfp, cues, cdpt, cmun, cest)
    }

    cc <- c(cues, csup)
    if (all(is.element(cc, nc))) {
        ws <- df_sup(dfq, cues, cdup, ccon, qres, csup)
    } else {
        ws <- df_sup(dfq, cues, cdup, ccon, qres, csup, dft)
    }

    wq <- select(dfq, one_of(c(cues, ccon))) %>%
        inner_join(wp, by = cues) %>%
        inner_join(ws, by = cues)

    message("\n... registros para cálculo de factor: ", nrow(wq))
    invisible(wq)
}

## -- estimados --

#' calibración
#' @description Factor para ajustar factores de expansión.
#' @details La función calcula la división que tiene por numerador un
#'     valor determinado (se supone que un total conocido de la
#'     población) y por denominador la suma (descartando los NA) de
#'     los datos (arg. "x") ponderados por las correspondientes
#'     ponderaciones (arg. "factor"). Si los factores se multiplican
#'     por este cociente, la suma de los datos ponderados por las
#'     factores así ajustados, será igual (dentro de los límites de
#'     precisión que caben) al valor utilizado como constante de
#'     calibración.
#' @param x numeric: los datos de la variable por la cual se calibra
#' @param factor numeric: el factor de expansión
#' @param totpob numeric: escalar con la constante c.r.a la que se
#'     calibra
#' @param dec integer: número de decimales del resultado; 6 por
#'     omisión
#' @return numeric escalar
#' @export
#' @author eddy castellón
w_calibra <- function(x, factor, totpob = numeric(), dec = 6L) {
    stopifnot("arg. inadmisible" = filled_num(x) &&
                  filled_num(factor),
              "arg. inadmisible" = is_scalar(totpob) &&
                  filled_num(totpob))

    te <- totpob / sum(x * factor, na.rm = TRUE)
    if (!is.finite(te)) {
        te <- 1.0
    }
    round(te, dec)
}

#' calibra - estrato
#' @description Calibra las ponderaciones por grupo.
#' @details Aplica la función \code{w_calibra} a datos agrupados. Por
#'     ejemplo para calibrar las ponderaciones por superficie de los
#'     estratos de un departamento, los grupos serían los estratos.
#'
#'     El arg. "dfo" debe tener las columnas con los datos de la
#'     variable de calibración (p.ej. superficie), del grupo al que
#'     pertenece el dato (p.ej. estrato) y de las ponderaciones
#'     iniciales. La posición o nombre de esas columnas se pasa en el
#'     arg. "cob" en el orden: variable calibración, grupo y
#'     ponderación. El arg. "dfg" es el data frame con los datos de
#'     los grupos: una columna con el dato que identifica al grupo
#'     (p.ej. estrato) y otra con el valor correspondiente al grupo
#'     (p.ej. superficie del estrato). La posición o nombre de las
#'     columnas se pasa en el arg. "cgr" en el orden: grupo, valor del
#'     grupo.
#'
#'     La función devuelve el arg. "dfo" con la columna adicional «wc»
#'     en la que están los factores de calibración.
#' @seealso w_calibra
#' @param dfo data.frame: datos de las observaciones (vea detalles)
#' @param dfg data.frame: datos de los grupos (vea detalles)
#' @param cob numeric o character: posición (integer) o nombre
#'     (character) de las columnas de datos (vea detalles); por
#'     omisión \code{1:3}.
#' @param cgr numeric o character: posición (integer) o nombre
--#'     (character) de la columna que identifica al grupo y el valor
#'     correspondiente al grupo (vea detalles); por omisión \code{1:2}
#' @param dec integer: número de decimales en la ponderación; 6 por
#'     defecto
#' @return data.frame
#' @export
#' @author eddy castellón
w_estrato <- function(dfo, dfg, cob = 1:3, cgr = 1:2, dec = 6L) {

    stopifnot(exprs = {
        "arg. inadmisible" = inherits(dfo, "data.frame") &&
            inherits(dfg, "data.frame")
        "arg. inadmisible" = nrow(dfg) > 0 && nrow(dfo) > nrow(dfg)
        "arg. inadmisible" = ncol(dfo) > 2 && ncol(dfg) > 1

        "arg. inadmisible" = length(cob) == 3 &&
            ((filled_char(cob) && all(is.element(cob, names(dfo)))) ||
            (filled_num(cob) && all(cob <= ncol(dfo))))
        
        "arg. inadmisible" = length(cgr) == 2 &&
            ((filled_char(cgr) && all(is.element(cgr, names(dfg)))) ||
            (filled_num(cgr) && all(cgr <= ncol(dfg))))
        
        "arg. inadmisible" = is_scalar(dec) && filled_num(dec)
    })

    if (is.character(cob)) {
        cob <- which(is.element(names(dfo), cob))
    }
    if (is.character(cgr)) {
        cgr <- which(is.element(names(dfg), cgr))
    }

    go <- dfo[[cob[2]]]
    gr <- dfg[[cgr[1]]]
    stopifnot("grupos incomp." = all(is.element(go, gr)))

    ww <- split(dfo, go, drop = TRUE)
    xx <- split(dfg, gr, drop = TRUE)
    ff <- function(x, y) {
        w_calibra(x[cob[1]], x[cob[3]], y[cgr[2]], dec) 
    }
    wc <- Map(ff, ww, xx) %>% c(recursive = TRUE)

    nn <- table(go)
    cbind(dfo, wc = rep(wc, nn))
}

## -- outliers --

#' outlier-estimado
#' @description Identifica las observaciones cuyo aporte al total es
#'     «extremo» de acuerdo a la cota arg. "pct".
#' @details Devuelve un data.frame con los datos «extremos» y el
#'     número de observaciones involucradas en el cálculo del total.
#' @param x numeric: las observaciones
#' @param id numeric o character: «id» de las observaciones
#' @param by numeric, character o factor: variable de agrupamiento
#' @param cota numeric escalar: cota superior al porcentaje de la
#'     contribución; por omisión, 10
#' @return lista de data.frame
#' @examples
#' aa <- data.frame(x = 1:5, y = c(2000, 1, 2, 2, 1000),
#'                  z = c("a", "a", "a", "b", "b"))
#' aporte_extremo(aa$y, aa$x, aa$z)
#' @export
aporte_extremo <- function(x, id, by = integer(), cota = 10L) {
    stopifnot("arg. inadmisible" = filled_num(x),
              "arg. inadmisible" = filled(id) &&
                  length(id) == length(x),
              "arg. inadmisible" = is_scalar0(by) ||
                  length(by) == length(x),
              "arg. inadmisible" = is_scalar(cota) && cota < 100)

    ap <- aporte(x, by)
    
    if (is_scalar0(by)) {
        nn <- list(length(x))
        ob <- list(id)
    } else {
        nn <- tapply(x, by, length, simplify = FALSE)
        ob <- tapply(id, by, identity, simplify = FALSE)
    }

    ae <- Map(function(x, y) {
        ii <- x > cota
        data.frame(id = y[ii], pct = x[ii])}, ap, ob)
    
    Map(function(x, y) cbind(x, n = y), ae, nn)
}

#' outlier
#' @description Remplaza outlier
#' @details Sustituye los datos que son mayores (menores) que
#'     arg. "cota", por los producidos por arg. "fun". La alternativa
#'     mayor (menor) que arg. "cota" la determina el arg. "mayor". La
#'     función no verifica los argumentos pasados a la función en
#'     pmt. "..."; cualquier error lo identifica la función en
#'     cuestión, y en tal caso, los datos no son modificados.
#' @param x numeric: los datos
#' @param cota numeric escalar: cota
#' @param fun function: función que devuelve sustituto de outlier
#' @param mayor logical: cota es cota superior? TRUE por omisión.
#' @param msj logical: un mensaje con indicadores?; FALSE por omisión
#' @param ... argumentos pasados a fun
#' @return numeric, invisible
#' @examples
#' (remplazar_outlier(c(200, 1:5, 1000), 100, mean, trim=0.1))
#' @export
remplazar_outlier <- function(x, cota = 0.0, fun, mayor = TRUE,
                              msj = FALSE, ...) {
    stopifnot("arg. inadmisible" = filled_num(x),
              "arg. inadmisible" = is.function(fun),
              "arg. inadmisible" = filled_num(cota) &&
                  is_scalar(cota))

    if (mayor) {
        ii <- x > cota
    } else {
        ii <- x < cota
    }
    
    if (any(ii)) {
        if (msj) {
            nn <- sum(ii, na.rm = TRUE)
            message("\n !!!extremos: ", nn, " pct.: ",
                    round(100 * nn / sum(!is.na(x)), 1L))
        }
        rr <- try(fun(x, ...))
        if (!inherits(rr, "try-error")) {
            x[which(ii)] <- rr
        }
    }
    
    invisible(x)
}

