.make_serie <- function(data, serie, ..., label = NULL, type = "line"){
  data <- pull(data, !!serie)
  
  if(is.null(label))
    label <- dplyr::as_label(serie)
  
  serie <- list(
    data = data,
    label = label,
    type = type
  )
}

.make_scatter_serie <- function(data, x, y, z, ..., label = NULL, type = "line"){
  data <- select(data, y = !!y, r = !!z)
  data$x <- x
  data <- apply(data, 1, as.list)
  
  if(is.null(label))
    label <- dplyr::as_label(y)
  
  serie <- list(
    data = data,
    label = label,
    type = type
  )
}
