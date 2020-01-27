#' Serie
#' 
#' Add a series to chart.
#' 
#' @param c An object of class \code{charter} as returned by \code{\link{c_hart}}.
#' @param ... Any other options.
#' @param label Label of the serie, if \code{NULL} then the \code{serie} is used.
#' @param inherit_caes Whether to inherit \code{\link{caes}} from \code{\link{c_hart}}.
#' @param data If \code{NULL} inherits data from \code{\link{c_hart}}.
#' @param orient Orientation of box, violin, and plots.
#' @param type Type of error bar to apply.
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
#' mtcars %>% 
#'  c_hart(caes(mpg, qsec, group = cyl)) %>% 
#'  c_violin()
#' 
#' df <- data.frame(
#'  x = factor(c(1, 2)),
#'  y = c(1, 5),
#'  upper = c(1.1, 5.3),
#'  lower = c(0.8, 4.3)
#' )
#' 
#' c_hart(df, caes(x, y, xmin = lower, ymax = upper)) %>% 
#'  c_error_bar(type = "bar")
#' 
#' @name series
#' @export
c_line <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_line")

#' @export 
#' @method c_line charter
c_line.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  type <- "line"
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @export 
#' @method c_line charterProxy
c_line.charterProxy <- function(c, ..., label = NULL, data = NULL){
  assert_that(not_null(data))
  type <- "line"

  serie <- make_serie(
    list(), 
    NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = type, 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie)
  c$session$sendCustomMessage("c-update", msg)
  return(c)
}

#' @rdname series
#' @export
c_scatter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_scatter")

#' @export 
#' @method c_scatter charter
c_scatter.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  type <- "scatter"
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_bubble <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_bubble")

#' @export 
#' @method c_bubble charter
c_bubble.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  type <- "bubble"
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_bar <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, orient = c("vertical", "horizontal")) UseMethod("c_bar")

#' @export 
#' @method c_bar charter
c_bar.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, orient = c("vertical", "horizontal")){
  type <- "bar"
  # type
  orient <- match.arg(orient)
  if(orient == "vertical")
    type <- "bar"
  else
    type <- "horizontalBar"
  
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_radar <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_radar")

#' @export 
#' @method c_radar charter
c_radar.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  type <- "radar"
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_pie <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_pie")

#' @export 
#' @method c_pie charter
c_pie.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  type <- "pie"
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_doughnut <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_doughnut")

#' @export 
#' @method c_doughnut charter
c_doughnut.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  type <- "doughnut"
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_polar_area <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL) UseMethod("c_polar_area")

#' @export 
#' @method c_polar_area charter
c_polar_area.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL){
  type <- "polarArea"
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_boxplot <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, orient = c("vertical", "horizontal")) UseMethod("c_boxplot")

#' @export 
#' @method c_boxplot charter
c_boxplot.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, orient = c("vertical", "horizontal")){
  
  # type
  orient <- match.arg(orient)
  if(orient == "vertical")
    type <- "boxplot"
  else
    type <- "horizontalBoxplot"

  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ...)
}

#' @rdname series
#' @export
c_violin <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, orient = c("vertical", "horizontal")) UseMethod("c_violin")

#' @export 
#' @method c_violin charter
c_violin.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, orient = c("vertical", "horizontal")){
  
  # type
  orient <- match.arg(orient)
  if(orient == "vertical")
    type <- "violin"
  else
    type <- "horizontalViolin"
  
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ..., valid_caes = "y")
}

#' @rdname series
#' @export
c_error_bar <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, type = NULL) UseMethod("c_error_bar")

#' @export 
#' @method c_error_bar charter
c_error_bar.charter <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, 
  type = c("bar", "horizontal_bar", "line", "scatter", "polar_area")){
  
  type <- match.arg(type)
  type <- error_bar_type(type)
  valid_caes <- error_bar_caes(type)
  
  c$x$opts$type <- type
  generate_serie(c, data, label, inherit_caes, type = type, ..., valid_caes = valid_caes)
}
