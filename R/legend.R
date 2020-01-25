#' Legend
#' 
#' Customise the legend.
#' 
#' @inheritParams c_line
#' @param display Whether to show the legend.
#' @param position The position of the legend.
#' @param align Alignment of the legend.
#' @param full_width Marks that this box should take the 
#' full width of the canvas (pushing down other boxes).
#' @param reverse Legend will show datasets in reverse order.
#' @param rtl Renders legend from right to left.
#' 
#' @examples 
#' cars %>% 
#'  c_hart(speed) %>% 
#'  c_line(dist) %>% 
#'  c_legend(position = "left")
#' 
#' @export 
c_legend <- function(c, display = TRUE, position = c("top", "left", "bottom", "right"),
  align = c("center", "start", "end"), full_width = TRUE, reverse = FALSE, rtl = FALSE){
  UseMethod("c_legend")
}

#' @export
#' @method c_legend charter
c_legend.charter <- function(c, display = TRUE, position = c("top", "left", "bottom", "right"),
  align = c("center", "start", "end"), full_width = TRUE, reverse = FALSE, rtl = FALSE){
  position <- match.arg(position)
  align <- match.arg(align)

  opts <- list(
    position = position,
    align = align,
    display = display,
    fullWidth = full_width,
    reverse = reverse,
    rtl = rtl
  )

  c$x$opts$options$legend <- opts
  return(c)
}

#' Legend Label
#' 
#' Customise the legend labels.
#' 
#' @inheritParams c_line
#' @param box_width Width of colored box in pixels.
#' @param font_size Font size of text in pixels.
#' @param font_style Font style.
#' @param font_family Font family to use.
#' @param padding Padding, in pixels, between rows of boxes.
#' @param use_point_style Label style will match corresponding point 
#' style (size is based on the mimimum value between boxWidth and fontSize).
#' 
#' @examples 
#' cars %>% 
#'  c_hart(speed) %>% 
#'  c_line(dist) %>% 
#'  c_legend(position = "bottom") %>% 
#'  c_legend_label(box_width = 100)
#' 
#' @export 
c_legend_label <- function(c, box_width = 40, font_size = 12, font_style = "normal",
  font_family = helvetica(), padding = 10, use_point_style = FALSE){

  UseMethod("c_legend_label")
}

#' @export 
#' @method c_legend_label charter
c_legend_label.charter <- function(c, box_width = 40, font_size = 12, font_style = "normal",
  font_family = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif", padding = 10,
  use_point_style = FALSE){

  opts <- list(
    boxWidth = box_width,
    fontSize = font_size,
    fontStyle = font_style,
    fontFamily = font_family,
    padding = padding,
    usePointStyle = use_point_style
  )

  c$x$opts$options$legend$label <- opts
  return(c)
}