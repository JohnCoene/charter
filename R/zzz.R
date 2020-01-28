.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler("charterParse", function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)
}