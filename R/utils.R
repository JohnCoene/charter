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