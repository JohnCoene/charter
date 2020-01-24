not_missing <- function(x) {
  !missing(x)
}

on_failure(not_missing) <- function(call, env) {
  paste0(
    "Missing `",
    crayon::red(deparse(call$x)),
    "`."
  )
}

has_data <- function(x){
  if(missing(x))
    return(FALSE)
  !is.null(x)
}

on_failure(has_data) <- function(call, env) {
  "Missing data."
}
