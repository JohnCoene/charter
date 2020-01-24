#' Line
#' 
#' Add a line chart.
#' 
#' @param c An object of class \code{charter} as returned by \code{\link{c_hart}}.
#' @param serie The bare serie to plot on the y axis.
#' @param ... Any other options.
#' @param label Label of the serie, if \code{NULL} then the \code{serie} is used.
#' 
#' @examples 
#' cars %>% 
#'  c_hart(dist) %>% 
#'  c_line(speed)
#' 
#' @export
c_line <- function(c, serie, ..., label = NULL) UseMethod("c_line")

#' @export 
#' @method c_line charter
c_line.charter <- function(c, serie, ..., label = NULL){
  # check that serie is passed
  assert_that(not_missing(serie))
  serie_enquo <- enquo(serie)

  serie <- .make_serie(
    c$x$data, 
    serie_enquo, ..., 
    label = label,
    type = "line"
  )

  c$x$opts$data$datasets <- append(c$x$opts$data$datasets, list(serie))
  return(c)
}

#' Bar
#' 
#' Add a bar chart.
#' 
#' @inheritParams c_line
#' 
#' @examples 
#' df <- data.frame(
#'  x = letters,
#'  y = runif(26)
#' )
#' 
#' df %>% 
#'  c_hart(x) %>% 
#'  c_bar(y)
#' 
#' @export
c_bar <- function(c, serie, ..., label = NULL) UseMethod("c_bar")

#' @export 
#' @method c_bar charter
c_bar.charter <- function(c, serie, ..., label = NULL){
  # check that serie is passed
  assert_that(not_missing(serie))
  serie_enquo <- enquo(serie)

  serie <- .make_serie(
    c$x$data, 
    serie_enquo, ..., 
    label = label,
    type = "bar"
  )

  c$x$opts$data$datasets <- append(c$x$opts$data$datasets, list(serie))
  return(c)
}

#' Radar
#' 
#' Add a radar chart.
#' 
#' @inheritParams c_line
#' 
#' @examples 
#' df <- data.frame(
#'  x = letters,
#'  y = runif(26)
#' )
#' 
#' df %>% 
#'  c_hart(x) %>% 
#'  c_radar(y)
#' 
#' @export
c_radar <- function(c, serie, ..., label = NULL) UseMethod("c_radar")

#' @export 
#' @method c_radar charter
c_radar.charter <- function(c, serie, ..., label = NULL){
  # check that serie is passed
  assert_that(not_missing(serie))
  serie_enquo <- enquo(serie)

  serie <- .make_serie(
    c$x$data, 
    serie_enquo, ..., 
    label = label,
    type = "radar"
  )

  c$x$opts$type <- "radar"

  c$x$opts$data$datasets <- append(c$x$opts$data$datasets, list(serie))
  return(c)
}

#' Pie
#' 
#' Add a pie chart.
#' 
#' @inheritParams c_line
#' 
#' @examples 
#' df <- data.frame(
#'  x = letters[1:5],
#'  y = runif(5, 1, 5)
#' )
#' 
#' df %>% 
#'  c_hart(x) %>% 
#'  c_pie(y)
#' 
#' @export
c_pie <- function(c, serie, ..., label = NULL) UseMethod("c_pie")

#' @export 
#' @method c_pie charter
c_pie.charter <- function(c, serie, ..., label = NULL){
  # check that serie is passed
  assert_that(not_missing(serie))
  serie_enquo <- enquo(serie)

  serie <- .make_serie(
    c$x$data, 
    serie_enquo, ..., 
    label = label,
    type = "pie"
  )

  c$x$opts$type <- "pie"

  c$x$opts$data$datasets <- append(c$x$opts$data$datasets, list(serie))
  return(c)
}

#' Doughnut
#' 
#' Add a doughnut chart.
#' 
#' @inheritParams c_line
#' 
#' @examples 
#' df <- data.frame(
#'  x = letters[1:5],
#'  y = runif(5, 1, 5)
#' )
#' 
#' df %>% 
#'  c_hart(x) %>% 
#'  c_doughnut(y)
#' 
#' @export
c_doughnut <- function(c, serie, ..., label = NULL) UseMethod("c_doughnut")

#' @export 
#' @method c_doughnut charter
c_doughnut.charter <- function(c, serie, ..., label = NULL){
  # check that serie is passed
  assert_that(not_missing(serie))
  serie_enquo <- enquo(serie)

  serie <- .make_serie(
    c$x$data, 
    serie_enquo, ..., 
    label = label,
    type = "doughnut"
  )

  c$x$opts$type <- "doughnut"

  c$x$opts$data$datasets <- append(c$x$opts$data$datasets, list(serie))
  return(c)
}