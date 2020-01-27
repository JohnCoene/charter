#' Aesthetics
#' 
#' Aesthetics to use to draw series on chart.
#' 
#' @param x,y,... List of name value pairs giving aesthetics to map to
#'  variables. The names for x and y aesthetics are typically omitted because
#'  they are so common; all other aspects must be named.
#' 
#' @export
caes <- function(x, y, ...) {
  exprs <- rlang::enquos(x = x, y = y, ..., .ignore_empty = "all")
  aes <- new_aes(exprs, env = parent.frame())
  .construct_aesthetics(aes)
}

# construct aesthetics for re-use
.construct_aesthetics <- function(aes, cl = NULL){
  class <- "caes"
  if(!is.null(cl))
    class <- append(class, cl)
  structure(aes, class = c(class, class(aes)))
}

# Wrap symbolic objects in quosures but pull out constants out of
# quosures for backward-compatibility
new_aesthetic <- function(x, env = globalenv()) {
  if (rlang::is_quosure(x)) {
    if (!rlang::quo_is_symbolic(x)) {
      x <- rlang::quo_get_expr(x)
    }
    return(x)
  }

  if (rlang::is_symbolic(x)) {
    x <- rlang::new_quosure(x, env = env)
    return(x)
  }

  x
}

new_aes <- function(x, env = globalenv()) {
  stopifnot(is.list(x))
  x <- lapply(x, new_aesthetic, env = env)
  structure(x, class = c("uneval"))
}

#' @export
print.uneval <- function(x, ...) {
  cat("Aesthetics: \n")

  if (length(x) == 0) {
    cat("<empty>\n")
  } else {
    values <- vapply(x, rlang::quo_label, character(1))
    bullets <- paste0("* ", format(paste0("`", names(x), "`")), " -> ", values, "\n")

    cat(bullets, sep = "")
  }

  invisible(x)
}

#' @export
"[.uneval" <- function(x, i, ...) {
  new_aes(NextMethod())
}

# If necessary coerce replacements to quosures for compatibility
#' @export
"[[<-.uneval" <- function(x, i, value) {
  new_aes(NextMethod())
}
#' @export
"$<-.uneval" <- function(x, i, value) {
  # Can't use NextMethod() because of a bug in R 3.1
  x <- unclass(x)
  x[[i]] <- value
  new_aes(x)
}
#' @export
"[<-.uneval" <- function(x, i, value) {
  new_aes(NextMethod())
}

# is aesthetic?
is_caes <- function(x, cl = "caes"){
  aes <- FALSE
  if(inherits(x, cl))
    aes <- TRUE
  return(aes)
}

# retrieve aesthetics
get_caes <- function(...){
  caes <- list(...) %>% 
    keep(is_caes) 

  if(length(caes))
    caes <- caes[[1]]
  else
    caes <- list()

  rename_caes(caes)
}

remove_caes <- function(...){
  caes <- list(...) %>% 
    discard(is_caes)
}

rename_caes <- function(caes){
  if(!length(caes))
    return(caes)

  nms <- names(caes)
  nms <- gsub("size", "r", nms)
  names(caes) <- nms
  return(caes)
}

# combine mappings into main
combine_caes <- function(main_caes, caes, inherit_caes = TRUE){

  # return empty list if no aes
  if(!length(main_caes) && !length(caes))
    return(list())

  # if not inherited return caes
  if(!inherit_caes)
    return(caes)

  # if inherit override
  # no main return caes
  if(!length(main_caes))
    return(caes)

  # return main if nothing specifed
  if(!length(caes))
    return(main_caes)

  # override main
  for(i in 1:length(caes)){
    c <- names(caes)[[i]]
    main_caes[[c]] <- caes[[i]]
  }

  return(main_caes)
}
