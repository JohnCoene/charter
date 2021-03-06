% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{c_update}
\alias{c_update}
\title{Update}
\usage{
c_update(c, duration = NULL, lazy = FALSE, easing = NULL)
}
\arguments{
\item{c}{A proxy for a chart as returned by \code{charter_proxy}.}

\item{duration}{Time for the animation of the redraw in milliseconds.}

\item{lazy}{If \code{TRUE} the animation can be interrupted by other animations.}

\item{easing}{The animation easing function. See
\href{https://www.chartjs.org/docs/latest/configuration/animations.html#easing}{Animation Easing}
for possible values.}
}
\description{
Update a chart.
}
\examples{
library(shiny)

ui <- fluidPage(
 actionButton("add", "Add z serie"),
 actionButton("update", "Update chart"),
 charterOutput("chart")
)

server <- function(input, output){

 df <- data.frame(
   x = 1:100,
   y = runif(100),
   z = runif(100)
 )

 output$chart <- render_charter({
   chart(df, caes(x, y)) \%>\% 
     c_scatter()
 })

 observeEvent(input$add, {
   charter_proxy("chart") \%>\% 
     c_line(data = df, caes(x, z), update = FALSE)
 })

 observeEvent(input$update, {
   charter_proxy("chart") \%>\% 
     c_update()
 })

}

if(interactive())
 shinyApp(ui, server)

}
