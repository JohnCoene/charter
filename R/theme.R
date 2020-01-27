#' Theme
#' 
#' Define chart theme.
#' 
#' @inheritParams series
#' @param point_radius Point radius.
#' @param point_style Point style, see section below.
#' @param point_rotation Point rotation in degrees.
#' @param point_color Point fill color.
#' @param point_border_color Point border color.
#' @param point_border_width Point stroke width.
#' @param point_hit_radius Extra radius added to point radius for hit detection.
#' @param point_hover_radius Point radius when hovered.
#' @param point_hover_border_width Stroke width when hovered.
#' @param line_tension Bézier curve tension (\code{0} for no Bézier curves).
#' @param line_color Line fill color.
#' @param line_border_width Line border width.
#' @param line_border_color Line stroke color.
#' @param line_border_cap_style Line cap style 
#' @param line_border_dash Line dash, see \href{https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setLineDash}{MDN}.
#' @param line_border_dash_offset Line dash offset value.
#' @param line_border_join_style Line join style.
#' @param line_cap_bezier Set to \code{TRUE} to keep Bézier control inside the 
#' chart, \code{FALSE} for no restriction.
#' @param line_cubic_interpolation Interpolation mode to apply.
#' @param line_fill How to fill the area under the line.
#' @param line_stepped Set to \code{TRUE} to show the line as a stepped 
#' line (\code{tension} will be ignored).
#' @param rect_color Bar fill color.
#' @param rect_border_width Bar stroke width.
#' @param rect_border_color Bar stroke color.
#' @param rect_border_skip Border to exclude.
#' @param arc_angle A numeric defining arc angle to cover, defaults to 
#' \eqn{circumference / (arc count)}.
#' @param arc_color Arc fill color.
#' @param arc_border_align Arc stroke alignment.
#' @param arc_border_color Arc border color.
#' @param arc_border_width Arc border width.
#' 
#' @section Point Styles:
#' \itemize{
#'  \item{\code{circle}}
#'  \item{\code{cross}}
#'  \item{\code{crossRot}}
#'  \item{\code{dash}}
#'  \item{\code{line}}
#'  \item{\code{rect}}
#'  \item{\code{rectRounded}}
#'  \item{\code{rectRot}}
#'  \item{\code{star}}
#'  \item{\code{triangle}}
#' }
#' 
#' @section Stepped Line:
#' \itemize{
#'  \item{A boolean \code{FALSE}, \code{TRUE}}
#'  \item{A character string, \code{before}, \code{after}, \code{middle}}
#' }
#' 
#' @section Line Fill Modes:
#' \itemize{
#'  \item{An aboslute index as integer e.g.: \code{1}.}
#'  \item{A relative index as string e.g.: \code{-1}}
#'  \item{Boundary one of \code{start}, \code{end}, or \code{origin}}
#'  \item{Logical, \code{TRUE} or \code{FALSE}}
#' }
#' 
#' @examples 
#' c_hart(cars, caes(speed, dist)) %>% 
#'  c_line() %>% 
#'  c_theme(point_radius = 8)
#' 
#' @export
c_theme <- function(c, point_radius = 3, point_style = "circle", point_rotation = 0, 
  point_color = "rgba(0, 0, 0, 0.1)", point_border_color = "rgba(0, 0, 0, 0.1)",
  point_border_width = 1, point_hit_radius = 1, point_hover_radius = 4, point_hover_border_width = 1,
  line_tension = .4, line_color = "rgba(0, 0, 0, 0.1)", line_border_width = 3, 
  line_border_color = "rgba(0, 0, 0, 0.1)", line_border_cap_style = c("butt", "round", "square"), 
  line_border_dash = NULL, line_border_dash_offset = 0, line_border_join_style = c("miter", "bevel", "round"),
  line_cap_bezier = TRUE, line_cubic_interpolation = c("default", "monotone"), line_fill = TRUE, 
  line_stepped = FALSE, rect_color = "rgba(0, 0, 0, 0.1)", rect_border_width = 0,
  rect_border_color = "rgba(0, 0, 0, 0.1)", rect_border_skip = c("bottom", "left", "top", "right"),
  arc_angle = NULL, arc_color = "rgba(0, 0, 0, 0.1)", arc_border_align = "center",
  arc_border_color = "#fff", arc_border_width = 2){
  UseMethod("c_theme")
}

#' @export 
#' @method c_theme charter
c_theme.charter <- function(c, point_radius = 3, point_style = "circle", point_rotation = 0, 
  point_color = "rgba(0, 0, 0, 0.1)", point_border_color = "rgba(0, 0, 0, 0.1)",
  point_border_width = 1, point_hit_radius = 1, point_hover_radius = 4, point_hover_border_width = 1,
  line_tension = .4, line_color = "rgba(0, 0, 0, 0.1)", line_border_width = 3, 
  line_border_color = "rgba(0, 0, 0, 0.1)", line_border_cap_style = c("butt", "round", "square"), 
  line_border_dash = NULL, line_border_dash_offset = 0, line_border_join_style = c("miter", "bevel", "round"),
  line_cap_bezier = TRUE, line_cubic_interpolation = c("default", "monotone"), line_fill = TRUE, 
  line_stepped = FALSE, rect_color = "rgba(0, 0, 0, 0.1)", rect_border_width = 0,
  rect_border_color = "rgba(0, 0, 0, 0.1)", rect_border_skip = c("bottom", "left", "top", "right"),
  arc_angle = NULL, arc_color = "rgba(0, 0, 0, 0.1)", arc_border_align = "center",
  arc_border_color = "#fff", arc_border_width = 2){

  line_cubic_interpolation <- match.arg(line_cubic_interpolation)
  line_border_cap_style <- match.arg(line_border_cap_style)
  rect_border_skip <- match.arg(rect_border_skip)
  line_border_join_style <- match.arg(line_border_join_style)

  point <- list(
    radius = point_radius, 
    pointStyle = point_style, 
    rotation = point_rotation, 
    backgroundColor = point_color, 
    borderColor = point_border_color,
    borderWidth = point_border_width, 
    hitRadius = point_hit_radius, 
    hoverRadius = point_hover_radius, 
    hoverBorderWidth = point_hover_border_width
  )

  line <- list(
    tension = line_tension, 
    backgroundColor = line_color, 
    borderWidth = line_border_width, 
    borderColor = line_border_color, 
    borderCapStyle = line_border_cap_style, 
    borderDashOffset = line_border_dash_offset, 
    borderJoinStyle = line_border_join_style,
    capBezierPoints = line_cap_bezier, 
    cubicInterpolationMode = line_cubic_interpolation, 
    fill = line_fill, 
    stepped = line_stepped
  )

  if(!is.null(line_border_dash))
  line$borderDash <- line_border_dash

  rectangle <- list(
    backgroundColor = rect_color, 
    borderWidth = rect_border_width,
    borderColor = rect_border_color, 
    borderSkipped = rect_border_skip
  )

  arc <- list(
    angle = arc_angle, 
    backgroundColor = arc_color, 
    borderAlign = arc_border_align,
    borderColor = arc_border_color, 
    borderWidth = arc_border_width
  )

  opts <- list(
    point = point,
    line = line,
    rectangle = rectangle,
    arc = arc
  )

  c$x$opts$options$elements <- opts
  return(c)
}