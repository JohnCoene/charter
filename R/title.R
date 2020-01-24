#' Title
#' 
#' Add a title to the chart.
#' 
#' @inheritParams c_line
#' @param title Title to display, if a \code{list} or a \code{vector} of 
#' length > 1 text is rendered on multiple lines.
#' @param font_size Font size of text in pixels.
#' @param font_style Font style.
#' @param font_family Font family to use.
#' @param position The position of the title.
#' @param font_color Color of text.
#' @param display Whether to display the title.
#' @param padding Padding, in pixels, between rows of boxes.
#' @param line_height Height of an individual line of text.
#' 
#' @examples 
#' cars %>% 
#'  c_hart(speed) %>% 
#'  c_line(dist) %>% 
#'  c_title(
#'    c("Title", "Subtitle")
#'  )
#' 
#' @export
c_title <- function(c, title = "", position = c("top", "left", "bottom", "right"),
  font_size = 12, font_family = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
  font_color = "#666", font_style = "bold", padding = 10, line_height = 1.2, display = TRUE){
  
  UseMethod("c_title")
}

#' @export 
#' @method c_title charter
c_title.charter <- function(c, title = "", position = c("top", "left", "bottom", "right"),
  font_size = 12, font_family = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
  font_color = "#666", font_style = "bold", padding = 10, line_height = 1.2, display = TRUE){
  
  position <- match.arg(position)

  opts <- list(
    title = title, 
    position = position,
    fontSize = font_size, 
    fontFamily = font_family,
    fontColor = font_color, 
    fontStyle = font_style, 
    padding = padding, 
    lineHeight = line_height,
    display = display
  )

  c$x$opts$options$title <- opts
  return(c)
}