#' Line
#' 
#' Add a line chart.
#' 
#' @param c An object of class \code{charter} as returned by \code{\link{c_hart}}.
#' @param ... Any other options.
#' @param label Label of the serie, if \code{NULL} then the \code{serie} is used.
#' @param inherit_caes Whether to inherit \code{\link{caes}} from \code{\link{c_hart}}.
#' @param data If \code{NULL} inherits data from \code{\link{c_hart}}.
#' 
#' @examples 
#' c_hart(cars, caes(speed, dist)) %>% 
#'  c_line()
#' 
#' @export
c_line <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_line")

#' @export 
#' @method c_line charter
c_line.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "line", ...)
}
