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
#' chart(cars, caes(speed, dist)) %>% 
#'  c_line()
#' 
#' mtcars %>% 
#'  chart(caes(qsec, mpg, group = cyl)) %>% 
#'  c_scatter(color = "red")
#' 
#' mtcars %>% 
#'  chart(caes(qsec, mpg, group = cyl)) %>% 
#'  c_bubble(caes(size = drat)) %>% 
#'  c_color_scheme("brewer.Oranges3")
#' 
#' chart(mtcars, caes(y = qsec, group = cyl)) %>% 
#'  c_violin() %>% 
#'  c_colors()
#' 
#' df <- data.frame(
#'  x = factor(c(1, 2)),
#'  y = c(1, 5),
#'  upper = c(1.1, 5.3),
#'  lower = c(0.8, 4.3)
#' )
#' 
#' chart(df, caes(x, y, xmin = lower, ymax = upper)) %>% 
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
c_line.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "line", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_scatter charterProxy
c_scatter.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "scatter", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_bubble charterProxy
c_bubble.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "bubble", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_bar charterProxy
c_bar.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "bar", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_radar charterProxy
c_radar.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "radar", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_pie charterProxy
c_pie.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "pie", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_doughnut charterProxy
c_doughnut.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "doughnut", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_polar_area charterProxy
c_polar_area.charterProxy <- function(c, ..., label = NULL, data = NULL, update = TRUE){
  assert_that(not_null(data))

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "polarArea", 
    label = label, 
    ...
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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
  generate_serie(
    c, data, label, 
    inherit_caes, 
    type = type, ..., 
    valid_caes = c("x", "y"),
    x_as_list = TRUE
  )
}

#' @export 
#' @method c_boxplot charterProxy
c_boxplot.charterProxy <- function(c, ..., label = NULL, data = NULL, orient = c("vertical", "horizontal"), update = TRUE){
  assert_that(not_null(data))

  # type
  orient <- match.arg(orient)
  if(orient == "vertical")
    type <- "boxplot"
  else
    type <- "horizontalBoxplot"

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = type, 
    label = label, 
    ...,
    valid_caes = c("x", "y"),
    x_as_list = TRUE
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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
  generate_serie(
    c, data, label, 
    inherit_caes, type = type, 
    ..., valid_caes = c("x", "y"),
    x_as_list = TRUE
  )
}

#' @export 
#' @method c_violin charterProxy
c_violin.charterProxy <- function(c, ..., label = NULL, data = NULL, orient = c("vertical", "horizontal"), update = TRUE){
  assert_that(not_null(data))

  # type
  orient <- match.arg(orient)
  if(orient == "vertical")
    type <- "violin"
  else
    type <- "horizontalViolin"

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = type, 
    label = label, 
    ...,
    valid_caes = c("x", "y"),
    x_as_list = TRUE
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
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

#' @export 
#' @method c_error_bar charterProxy
c_error_bar.charterProxy <- function(c, ..., label = NULL, inherit_caes = TRUE, data = NULL, 
  type = c("bar", "horizontal_bar", "line", "scatter", "polar_area"), update = TRUE){
  
  assert_that(not_null(data))

  type <- match.arg(type)
  type <- error_bar_type(type)
  valid_caes <- error_bar_caes(type)

  serie <- make_serie(
    main_caes = list(), 
    main_data = NULL, 
    data = data, 
    inherit_caes = FALSE, 
    type = "polarArea", 
    label = label, 
    ...,
    valid_caes = valid_caes
  )

  msg <- list(id = c$id, serie = serie, update = update)
  c$session$sendCustomMessage("charter-add", msg)

  invisible(c)
}