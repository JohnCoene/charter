is_data_frame <- function(x){
  inherits(x, "data.frame")
}

on_failure(is_data_frame) <- function(call, env) {
  paste0(
    "`", crayon::red(deparse(call$x)), 
    "` must be a data.frame"
  )
}

has_data <- function(x) {
  !is.null(x)
}

on_failure(has_data) <- function(call, env) {
  paste0(
    "Missing `",
    crayon::red("data"),
    "`."
  )
}

has_caes <- function(x) {
  length(x) > 0
}

on_failure(has_caes) <- function(call, env) {
  paste0(
    "Missing ",
    crayon::red("aesthetics"),
    ", see `", 
    crayon::cyan("caes")
    ,"`."
  )
}