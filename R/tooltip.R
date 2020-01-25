#' Tooltip
#' 
#' Customise the tooltip.
#' 
#' @inheritParams c_line
#' @param enabled Whether to enable tooltips.
#' @param mode Defines which elements appear on the tooltip.
#' @param intersect If code{TRUE}, the tooltip mode applies only when the 
#' mouse position intersects with an element. If \code{FALSE}, the mode will 
#' be applied at all times.
#' @param position Where to position the tooltip.
#' @param bg_color Background color of the tooltip.
#' @param title_font_family Font of title.
#' @param title_font_size Font size of title.
#' @param title_font_style Font style of title.
#' @param title_font_color Font color of title.
#' @param title_align Horizontal alignment of the title text lines.
#' @param title_spacing Spacing to add to top and bottom of each title line.
#' @param title_margin_bottom Margin to add on bottom of title section.
#' @param body_font_family Body line font.
#' @param body_font_size Body font size.
#' @param body_font_style Body font style.
#' @param body_font_color Body font color.
#' @param body_align Horizontal alignment of the body text lines.
#' @param body_spacing Spacing to add to top and bottom of each tooltip item.
#' @param footer_font_family Footer font.
#' @param footer_font_size Footer font size.
#' @param footer_font_style Footer font style.
#' @param footer_font_color Footer font color.
#' @param footer_align Horizontal alignment of the footer text lines.
#' @param footer_spacing Spacing to add to top and bottom of each footer line.
#' @param footer_margin_top Margin to add before drawing the footer.
#' @param x_padding,y_padding Padding to add on left and right of tooltip.
#' @param caret_padding Extra distance to move the end of the tooltip arrow 
#' away from the tooltip point.
#' @param caret_size Size, in px, of the tooltip arrow.
#' @param corner_radius Radius of tooltip corner curves.
#' @param multi_key_bg Color to draw behind the colored boxes when multiple 
#' items are in the tooltip.
#' @param display_colors If \code{TRUE}, color boxes are shown in the tooltip.
#' @param border_color Color of the border.
#' @param border_width Size of the border.
#' @param rtl Renders the tooltip left to right.
#' 
#' @examples 
#' cars %>% 
#'  c_hart(speed) %>% 
#'  c_scatter(dist) %>% 
#'  c_tooltip(FALSE)
#' 
#' @export
c_tooltip <- function(c, enabled = TRUE, mode = c("nearest", "point", "index", "dataset", "x", "y"),
  intersect = TRUE, position = c("average", "nearest"), bg_color = "rgba(0, 0, 0, 0.8)", 
  title_font_family = helvetica(), title_font_size = 12, title_font_style = "bold", 
  title_font_color = "#fff", title_align = c("left", "right", "center"), title_spacing = 2,
  title_margin_bottom = 6, body_font_family = helvetica(), body_font_size = 12, body_font_style = "normal",
  body_font_color = "#fff", body_align = title_align, body_spacing = 2, footer_font_family = helvetica(),
  footer_font_size = 12, footer_font_style = "bold", footer_font_color = "#fff", footer_align = title_align,
  footer_spacing = 2, footer_margin_top = 6, x_padding = 6, y_padding = x_padding, caret_padding = 2,
  caret_size = 5, corner_radius = 6, multi_key_bg = "#fff", display_colors = TRUE, 
  border_color = "rgba(0, 0, 0, 0)", border_width = 0, rtl = FALSE){

  UseMethod("c_tooltip")
  
}

#' @export
#' @method c_tooltip charter
c_tooltip.charter <- function(c, enabled = TRUE, mode = c("nearest", "point", "index", "dataset", "x", "y"),
  intersect = TRUE, position = c("average", "nearest"), bg_color = "rgba(0, 0, 0, 0.8)", 
  title_font_family = helvetica(), title_font_size = 12, title_font_style = "bold", 
  title_font_color = "#fff", title_align = c("left", "right", "center"), title_spacing = 2,
  title_margin_bottom = 6, body_font_family = helvetica(), body_font_size = 12, body_font_style = "normal",
  body_font_color = "#fff", body_align = title_align, body_spacing = 2, footer_font_family = helvetica(),
  footer_font_size = 12, footer_font_style = "bold", footer_font_color = "#fff", footer_align = title_align,
  footer_spacing = 2, footer_margin_top = 6, x_padding = 6, y_padding = x_padding, caret_padding = 2,
  caret_size = 5, corner_radius = 6, multi_key_bg = "#fff", display_colors = TRUE, 
  border_color = "rgba(0, 0, 0, 0)", border_width = 0, rtl = FALSE){

  mode <- match.arg(mode)
  position <- match.arg(position)

  opts <- list(
    enabled = enabled, 
    mode = mode,
    intersect = intersect, 
    position = position, 
    backgroundColor = bg_color, 
    titleFontFamily = title_font_family, 
    titleFontSize = title_font_size, 
    titleFontStyle = title_font_style, 
    titleFontColor = title_font_color, 
    titleAlign = title_align, 
    titleSpacing = title_spacing,
    titleMarginBottom = title_margin_bottom, 
    bodyFontFamily = body_font_family, 
    bodyFontSize = body_font_size,
    bodyFontStyle = body_font_style,
    bodyFontColor = body_font_color, 
    bodyAlign = title_align, 
    bodySpacing = body_spacing, 
    footerFontFamily = footer_font_family,
    footerFontSize = footer_font_size, 
    footerFontStyle = footer_font_style, 
    footerFontColor = footer_font_color, 
    footerAlign = footer_align,
    footerSpacing = footer_spacing, 
    footerMarginTop = footer_margin_top, 
    xPadding = x_padding, 
    yPadding = x_padding, 
    caretPadding = caret_padding,
    caretSize = caret_size, 
    cornerRadius = corner_radius, 
    multiKeyBackground = multi_key_bg, 
    displayColors = display_colors, 
    borderColor = border_color, 
    borderWidth = border_width,
    rtl = rtl
  )
  
  c$x$opts$options$tooltips <- opts
  return(c)
}
