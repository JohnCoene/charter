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
    ,"`"
  )
}

not_missing <- function(x) {
  !missing(x)
}

on_failure(not_missing) <- function(call, env) {
  paste0(
    "Missing `",
    crayon::red(deparse(call$x)),
    "`"
  )
}

not_null <- function(x) {
  !is.null(x)
}

on_failure(not_null) <- function(call, env) {
  paste0(
    "Must specify `",
    crayon::red(deparse(call$x)),
    "`"
  )
}

valid_alpha <- function(alpha) {
  alpha >= 0 & alpha <= 1
}

on_failure(valid_alpha) <- function(call, env) {
  paste0(
    "Invalid `",
    crayon::red(deparse(call$alpha)),
    "`, must be between",
    crayon::cyan("0"),
    " and ",
    crayon::cyan("1")
  )
}
