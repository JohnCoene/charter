#' Serie
#' 
#' Add a series to chart.
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
#' mtcars %>% 
#'  c_hart(caes(qsec, mpg, group = cyl)) %>% 
#'  c_scatter()
#' 
#' mtcars %>% 
#'  c_hart(caes(qsec, mpg, group = cyl)) %>% 
#'  c_bubble(caes(size = drat))
#' 
#' @name series
#' @export
c_line <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_line")

#' @export 
#' @method c_line charter
c_line.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "line", ...)
}

#' @rdname series
#' @export
c_scatter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_scatter")

#' @export 
#' @method c_scatter charter
c_scatter.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "scatter", ...)
}

#' @rdname series
#' @export
c_bubble <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_bubble")

#' @export 
#' @method c_bubble charter
c_bubble.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "bubble", ...)
}

#' @rdname series
#' @export
c_bar <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_bar")

#' @export 
#' @method c_bar charter
c_bar.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "bar", ...)
}

#' @rdname series
#' @export
c_radar <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_radar")

#' @export 
#' @method c_radar charter
c_radar.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "radar", ...)
}

#' @rdname series
#' @export
c_pie <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_pie")

#' @export 
#' @method c_pie charter
c_pie.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "c_pie", ...)
}

#' @rdname series
#' @export
c_doughnut <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_doughnut")

#' @export 
#' @method c_doughnut charter
c_doughnut.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "c_doughnut", ...)
}

#' @rdname series
#' @export
c_polar_area <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_polar_area")

#' @export 
#' @method c_polar_area charter
c_polar_area.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  generate_serie(c, data, label, inherit_caes, type = "c_polar_area", ...)
}
