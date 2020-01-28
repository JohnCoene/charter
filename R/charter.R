#' Initialise
#'
#' Initialise a chart.js chart
#' 
#' @param data Data.frame holding data to plot.
#' @param ... Chart aesthetics, see \code{\link{caes}}.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Valid CSS id of chart to create.
#'
#' @import htmlwidgets
#' @import assertthat
#' @importFrom dplyr enquo pull select group_split pull
#' @importFrom purrr map keep discard
#'
#' @name c_hart 
#' @export
c_hart <- function(data = NULL, ..., width = "100%", height = NULL, elementId = NULL) {

  # preprocess inputs
  data <- process_data(data)
  main_caes <- get_caes(...)

  # scales
  scales <- handle_scales(
    data = data, 
    caes = main_caes
  )

  # get labels for empty chart
  if(!is.null(data))
    labels <- handle_labels(
      labels = NULL, 
      main_caes = list(),
      main_data = NULL,
      data, 
      inherit_caes = FALSE,
      ...
    )

  # forward options using x
  x = list(
    main_data = data,
    main_caes = main_caes,
    opts = list(
      responsive = TRUE,
      maintainAspectRatio = FALSE,
      type = "line",
      data = list(
        labels = labels,
        datasets = list()
      ),
      options = list(
        scales = scales
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

#' @rdname c_hart
#' @export
chart <- c_hart

charter_html <- function(id, class, ...){
  shiny::tags$div(
    id = id,
    class = class, ...,
    shiny::tags$canvas(
      id = paste0(id, "-canvas")
    )
  )
}

.build_chart <- function(c){
  c$x$main_data <- NULL
  c$x$main_caes <- NULL
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
#' @param id Id of chart to build a proxy from.
#' @param session A valid Shiny session.
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

#' @rdname charter-shiny
#' @export
render_charter <- renderCharter

#' @rdname charter-shiny
#' @export
charterProxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  
  proxy <- list(id = id, session = session)
  
  structure(proxy, class = c("charterProxy", class(proxy)))
}

#' @rdname charter-shiny
#' @export
charter_proxy <- charterProxy

#' @export
print.charterProxy <- function(x, ...){
  cat("A proxy for a charter plot\n")
}