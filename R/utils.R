globalVariables(
  c("group", "x")
)

ALL_CAES <- c("x", "y", "r", "group", "xMin", "yMin", "xMax", "yMax")

#' Preprocess
#' 
#' Preprocess data.frame.
#' 
#' @param data A data.frame.
#' 
#' @keywords internal
process_data <- function(data){
  if(is.null(data))
    return()

  assert_that(is_data_frame(data))
  
  row.names(data) <- NULL
  return(data)
}

#' Make Serie
#' 
#' Make Chart.js series from aesthetics and data.
#' 
#' @param main_caes Main aesthetics.
#' @param main_data,data Data.frames.
#' @param inherit_caes Whether aethetics are inherited.
#' @param type Type of serie to make.
#' @param label Serie label.
#' @param ... Addiitonal options and aesthetics.
#' @param valid_caes Valid aesthetics to keep on final plot.
#' @param x_as_list Whether to plot each x as a sublist.
#' 
#' @keywords internal
make_serie <- function(main_caes, main_data, data = NULL, inherit_caes = TRUE, 
  type = "line", label = NULL, ..., valid_caes = ALL_CAES, x_as_list = FALSE){

  # process aes
  caes <- get_caes(...)
  caes <- combine_caes(main_caes, caes, inherit_caes = inherit_caes)

  # remove to keep additional options
  opts <- remove_caes(...)

  # data
  if(is.null(data))
    data <- main_data
  else
    data <- process_data(data)

  # check
  assert_that(has_data(data))
  assert_that(has_caes(caes))

  data <- select(data, !!!caes)
  
  if(!is.null(caes$group))
    data <- group_split(data, group)
  else
    data <- list(data)

  purrr::map(
    data, 
    group_to_serie, 
    label = label, 
    opts = opts, 
    N = length(data), 
    type = type,
    caes = caes,
    valid_caes = valid_caes,
    x_as_list = x_as_list
  )
}

#' Listize
#' 
#' Turn a data.frame into a list
#' 
#' @param data A data.frame.
#' @param valid_caes Valid aesthetics.
#' @param x_as_list Whether to plot each x as a sublist.
#' 
#' @section X as list:
#' Default data format is 
#' [{x:1, y:2}, {x:1, y:3}, {x:2, y:3}, {x:2, y:5}]
#' with \code{x_as_list} set to \code{TRUE} each x is as a list
#' so the data looks like:
#' [[2,3],[3,5]]
#' Currently used for boxplot and violin plot.
#' 
#' @keywords internal
listize <- function(data, valid_caes, x_as_list = FALSE){
  if("group" %in% names(data))
    data <- select(data, -group) 

  data <- suppressWarnings(
    select(data, dplyr::one_of(valid_caes))
  )

  # if it's a special case & x exists
  if(x_as_list && "x" %in% names(data)){
    lst <- data %>% 
      group_split(x) %>% 
      map(function(x){
        unlist(x$y)
      })
    return(lst)
  }

  if(ncol(data) == 1)
    data %>% unlist() %>% unname() %>% list()
  else
    apply(data, 1, as.list)
}

#' Groups to Series
#' 
#' Maps groups to series.
#' 
#' @param group_data A data.frame.
#' @param label Label assigned by user.
#' @param opts Additional options.
#' @param N Number of series.
#' @param type type of chart to draw.
#' @param caes Aesthetics.
#' @param valid_caes Valid aesthetics to keep in dataset.
#' @param x_as_list Whether to plot each x as a sublist.
#' 
#' @keywords internal
group_to_serie <- function(group_data, label, opts, N, type, caes, 
  valid_caes = ALL_CAES, x_as_list = FALSE){
  # add based on y if only one group
  label <- get_label(group_data, label, caes, N)

  # remove uneeded group
  group_data <- listize(group_data, valid_caes, x_as_list)

  # basic serie
  serie <- list(
    type = type,
    label = label,
    data = group_data
  )

  # append options
  serie <- append(serie, opts)

  return(serie)
}

#' Get Label
#' 
#' Get label from of serie from data.
#' 
#' @param data Data.frame.
#' @param label Label set by user.
#' @param caes Aesthetics.
#' @param N Number of series (by group).
#' 
#' @keywords internal
get_label <- function(data, label, caes, N){

  if(is.null(label) && N == 1)
    return(rlang::as_label(caes$y))
  
  if(is.null(label) && N > 1)
    label <- pull(data, group) %>% 
      unique() %>% 
      as.character()

  return(label)
}

#' Generate serie
#' 
#' Wrapper to \code{handle_labels} and \code{make_serie}.
#' 
#' @param c charter object.
#' @param data A data.frame.
#' @param label Serie label defined by user.
#' @param inherit_caes Whether to inherit aesthetics.
#' @param type Series type.
#' @param ... Additional aesthetics and options.
#' 
#' @keywords internal
generate_serie <- function(c, data, label, inherit_caes, type = "line", ..., valid_caes = ALL_CAES){
  
  serie <- make_serie(
    c$x$main_caes, 
    c$x$main_data, 
    data = data, 
    inherit_caes = inherit_caes, 
    type = type, 
    label = label, 
    ...,
    valid_caes = valid_caes
  )

  c$x$opts$data$labels <- handle_labels(
    c$x$opts$data$labels, 
    c$x$main_caes,
    c$x$main_data,
    data, 
    inherit_caes = inherit_caes,
    ...
  )

  c$x$opts$data$datasets <- append(c$x$opts$data$datasets, serie)
  return(c)
}

#' Handle Labels
#' 
#' Handle global labels.
#' 
#' @param labels Current labels.
#' @param main_caes Aesthetics.
#' @param main_data,data Data.frames.
#' @param inherit_caes Whether to inherit aesthetics.
#' @param ... Additional aesthetics.
#' 
#' @keywords internal
handle_labels <- function(labels, main_caes, main_data, data, inherit_caes = TRUE, ...){

  if(!is.null(labels))
    return(labels)

  caes <- get_caes(...)
  caes <- combine_caes(main_caes, caes, inherit_caes = inherit_caes)

  # data
  if(is.null(data))
    data <- main_data

  # check that data present
  assert_that(has_data(data))

  if(!is.null(caes$x))
    data %>% 
      pull(!!caes$x) %>% 
      unique()
}

#' Get Aesthetics from Type
#' 
#' Get valid aesthetics from error bar type.
#' 
#' @param type An error bar type.
#' 
#' @keywords internal
error_bar_caes <- function(type){
  switch(
    type,
    "barWithErrorBars" = c("y", "yMin", "yMax"),
    "horizontalBarWithErrorBars" = c("x", "xMin", "xMax"),
    "lineWithErrorBars" = c("y", "yMin", "yMax"),
    "scatterWithErrorBars" = c("y", "x", "xMin", "xMax", "yMin", "yMax"),
    "polarAreaWithErrorBars" = c("y", "yMin", "yMax")
  )
}

#' Convert Error Type
#' 
#' Convert convenient user facing type to internal error type.
#' 
#' @param type An error bar type.
#' 
#' @keywords internal
error_bar_type <- function(type){

  type <- strsplit(type, "_")[[1]]

  # split at _
  if(length(type) == 2){
    type[2] <- tools::toTitleCase(type[2])
    type <- paste0(type, collapse = "")
  }

  # make error type
  type <- paste0(type, "WithErrorBars")

  return(type)

}