#' Events
#' 
#' Capture how users interact with the chart.
#' 
#' @inheritParams series
#' 
#' @export
c_events <- function(c) UseMethod("c_events")

#' @export 
#' @method c_events charter
c_events.charter <- function(c){
  return(c)
}