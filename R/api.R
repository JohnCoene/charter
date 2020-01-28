#' Update
#' 
#' Update a chart.
#' 
#' @param c A proxy for a chart as returned by \code{charter_proxy}.
#' @param duration Time for the animation of the redraw in milliseconds.
#' @param lazy If \code{TRUE} the animation can be interrupted by other animations.
#' @param easing The animation easing function. See 
#' \href{https://www.chartjs.org/docs/latest/configuration/animations.html#easing}{Animation Easing} 
#' for possible values.
#' 
#' @examples 
#' library(shiny)
#' 
#' ui <- fluidPage(
#'  actionButton("add", "Add z serie"),
#'  actionButton("update", "Update chart"),
#'  charterOutput("chart")
#' )
#' 
#' server <- function(input, output){
#' 
#'  df <- data.frame(
#'    x = 1:100,
#'    y = runif(100),
#'    z = runif(100)
#'  )
#' 
#'  output$chart <- render_charter({
#'    chart(df, caes(x, y)) %>% 
#'      c_scatter()
#'  })
#' 
#'  observeEvent(input$add, {
#'    charter_proxy("chart") %>% 
#'      c_line(data = df, caes(x, z), update = FALSE)
#'  })
#' 
#'  observeEvent(input$update, {
#'    charter_proxy("chart") %>% 
#'      c_update()
#'  })
#' 
#' }
#' 
#' if(interactive())
#'  shinyApp(ui, server)
#' 
#' @export
c_update <- function(c, duration = NULL, lazy = FALSE, easing = NULL) UseMethod("c_update")

#' @export
#' @method c_update charterProxy
c_update.charterProxy <- function(c, duration = NULL, lazy = FALSE, easing = NULL){

  opts <- list(
    duration = duration,
    lazy = lazy,
    easing = easing
  ) %>% 
    discard(is.null)

  msg <- list(id = c$id)
  msg$opts <- opts

  c$session$sendCustomMessage("charter-update", msg)

  invisible(c)
}
