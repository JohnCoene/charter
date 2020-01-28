#' Events
#' 
#' Capture how users interact with the chart.
#' 
#' @inheritParams series
#' @param events Events to register in the tooltip (\code{c_events})
#' or events to capture in Shiny server \code{c_events_capture}.
#' 
#' @examples 
#' library(shiny)
#' ui <- fluidPage(
#'   charterOutput("chart"),
#'  verbatimTextOutput("ev")
#' )
#' 
#' server <- function(input, output){
#' 
#'  output$chart <- render_charter({
#'    chart(cars, caes(speed, dist)) %>% 
#'      c_scatter() %>% 
#'      c_events_capture("hover")
#'  })
#' 
#'  output$ev <- renderPrint({
#'    input$chart_hovered
#'  })
#' }
#' 
#' if(interactive())
#'  shinyApp(ui, server)
#' 
#' @name events
#' @export
c_events <- function(c, events = c("mousemove", "mouseout", "click", "touchstart", "touchmove")) UseMethod("c_events")

#' @export 
#' @method c_events charter
c_events.charter <- function(c, events = c("mousemove", "mouseout", "click", "touchstart", "touchmove")){
  c$x$opts$options$events <- events
  return(c)
}

#' @rdname events
#' @export
c_events_capture <- function(c, events = c("click", "hover")) UseMethod("c_events_capture")

#' @export 
#' @method c_events_capture charter
c_events_capture.charter <- function(c, events = c("click", "hover")){
  c$x$events_capture <- as.list(events)
  return(c)
}