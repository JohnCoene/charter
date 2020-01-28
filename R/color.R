#' Color Scheme
#' 
#' Define a color scheme using the \href{https://nagix.github.io/chartjs-plugin-colorschemes/colorchart.html}{color-scheme plugin}.
#' 
#' @inheritParams series
#' @param scheme A valid scheme from the \href{https://nagix.github.io/chartjs-plugin-colorschemes/colorchart.html}{color-scheme plugin}.
#' @param alpha The transparency value for the line fill color. 
#' Must be a number between \code{0} (fully transparent) and \code{1} (no transparency).
#' @param reverse If set to \code{TRUE}, the order of the colors in the selected scheme is reversed.
#' @param override If set to \code{TRUE}, the specified color scheme will override the existing color 
#' options. If \code{FALSE}, it is only applied when no color setting exists.
#' @param palette Vector of colors, hex (\code{#fff}) or RGB as string \code{rgb(255,255,255)}.
#' @param rev Reverse the order in which to apply the colors.
#' 
#' @examples 
#' mtcars %>% 
#'  c_hart(caes(qsec, mpg, group = cyl)) %>% 
#'  c_scatter() %>% 
#'  c_color_scheme("brewer.DarkTwo8")
#' 
#' @name colors
#' @export
c_color_scheme <- function(c, scheme, alpha = .5, reverse = FALSE, override = FALSE) UseMethod("c_color_scheme")

#' @export
#' @method c_color_scheme charter 
c_color_scheme.charter <- function(c, scheme, alpha = .5, reverse = FALSE, override = FALSE){
  assert_that(not_missing(scheme))
  assert_that(valid_alpha(alpha))

  # get dependency
  path <- "htmlwidgets/lib/chartjs/plugins"
  file <- system.file(path, package = "charter")
  dep <- htmltools::htmlDependency(
    name = "colorschemes", 
    version = "0.4.0", 
    src = c(file = file),
    script = "colorschemes.min.js"
  ) 

  # add dep
  c$dependencies <- append(c$dependencies, list(dep))

  # add plugin options
  c$x$opts$options$plugins$colorschemes <- list(
    scheme = scheme,
    fillAlpha = alpha,
    reverse = reverse,
    override = override
  )

  # override colors
  c$x$main_colors <- NULL

  return(c)
}

#' @rdname colors
#' @export
c_colors <- function(c, palette = c("#ff6384", "#ff9f40", "#ffcd56", "#4bc0c0", "#36a2eb", "#9966ff", "#c9cbcf"), rev = FALSE) UseMethod("c_colors")

#' @method c_colors charter
#' @export
c_colors.charter <- function(c, palette = c("#ff6384", "#ff9f40", "#ffcd56", "#4bc0c0", "#36a2eb", "#9966ff", "#c9cbcf"), rev = FALSE){
  assert_that(not_missing(palette))

  if(rev)
    palette <- rev(palette)
  
  c$x$main_colors <- palette
  return(c)
}