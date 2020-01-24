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