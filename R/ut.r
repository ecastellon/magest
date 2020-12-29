# -*- coding: utf-8 -*-

##--- utilidad general --
#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_vacuo <- function(x) {
    length(x) == 0
}

#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_empty <- function(x) {
    length(x) == 0
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

