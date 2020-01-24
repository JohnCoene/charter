#' Initialise
#'
#' Initialise a chart.js chart
#' 
#' @param data Data.frame holding data to plot.
#' @param x The bare column containing x values.
#' @param ... Any other options.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Valid CSS id of chart to create.
#'
#' @import htmlwidgets
#' @import assertthat
#' @importFrom dplyr enquo pull
#'
#' @export
c_hart <- function(data, x, ..., width = NULL, height = NULL, elementId = NULL) {

  assert_that(has_data(data))

  if(!missing(x)){
    x_enquo <- enquo(x)
    labels <- pull(data, !!x_enquo)
  }

  # forward options using x
  x = list(
    data = data,
    opts = list(
      responsive = TRUE,
      type = "line",
      data = list(
        labels = labels,
        datasets = list()
      )
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'charter',
    x,
    width = width,
    height = height,
    package = 'charter',
    preRenderHook = .build_chart,
    elementId = elementId
  )
}

charter_html <- function(id, class, ...){
  shiny::tags$div(
    id = paste0(id, "-wrapper"),
    shiny::tags$canvas(
      id = id, class = class, ...
    )
  )
}

.build_chart <- function(c){
  c$x$data <- NULL
  return(c)
}

#' Shiny bindings for charter
#'
#' Output and render functions for using charter within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a charter
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name charter-shiny
#'
#' @export
charterOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'charter', width, height, package = 'charter')
}

#' @rdname charter-shiny
#' @export
renderCharter <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, charterOutput, env, quoted = TRUE)
}
