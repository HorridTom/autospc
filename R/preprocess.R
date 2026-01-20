
preprocess_inputs <- function(
    df,
    chart_type,
    title = NULL,
    subtitle = NULL,
    upper_annotation_sf = NULL,
    lower_annotation_sf = NULL,
    override_annotation_dist = NULL,
    override_annotation_dist_P = NULL) {
  
  validate_chart_type(chart_type)
  
  #get title from data
  if(is.null(title) & "title" %in% colnames(df)) {
    title <- df$title[1]
  }
  
  if(is.null(subtitle) & "subtitle" %in% colnames(df)) {
    subtitle <- df$subtitle[1]
  }
  
  #get type from x variable so that ggplot axes are correct
  #currently only accepting Date, numeric and integer as acceptable types
  xType <- class(df$x)
  if(all(xType != "Date") & 
     all(xType!= c("POSIXct", "POSIXt")) & 
     all(xType != "numeric") & 
     all(xType != "integer")) {
    warning(paste("Please make sure that your x column is a",
                  "'Date', 'POSIXct', 'numeric' or 'integer' type."))
  }
  
  if(chart_type == "MR") {
    mrs <- get_mrs(y = df$y)
    df <- df %>% dplyr::mutate(y = mrs)
  }
  
  # Check annotation arguments
  if(!is.null(override_annotation_dist) |
     !is.null(override_annotation_dist_P)) {
    
    lifecycle::deprecate_warn(
      when = "0.0.0.9010",
      what = I(paste0("autospc(override_annotation_dist,",
                      "override_annotation_dist_P)")),
      details = I(paste0("Please use `autospc(upper_annotation_sf, ",
                         "lower_annotation_sf)` instead. ",
                         "Note that equivalent new arguments can be obtained ",
                         "from the old by transforming as follows: 1+1/x. ",
                         "For example, override_annotation_dist = 10 is ",
                         "equivalent to upper_annotation_sf = 1.1."))
    )
    
    if(!is.null(override_annotation_dist_P) & startsWith(chart_type, "P")) {
      oad <- override_annotation_dist_P
    } else {
      oad <- override_annotation_dist
    }
    
    if(is.null(upper_annotation_sf)) {
      upper_annotation_sf <- 1 + 1/oad
    }
    
    if(is.null(lower_annotation_sf)) {
      lower_annotation_sf <- 1 - 1/oad
    }
    
  }
  
  if(is.null(upper_annotation_sf)) {
    upper_annotation_sf <- ifelse(startsWith(chart_type, "P"),
                                  1.04,
                                  1.1)
  }
  
  if(is.null(lower_annotation_sf)) {
    lower_annotation_sf <- 2 - upper_annotation_sf
  }
  
  
  return(list(
    df = df,
    chart_type = chart_type,
    title = title,
    subtitle = subtitle,
    xType = xType,
    upper_annotation_sf = upper_annotation_sf,
    lower_annotation_sf = lower_annotation_sf
  ))
  
}


# Function to rename columns
rename_columns <- function(df, x, y, n) {
  
  data_colnames <- colnames(df)
  
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  n <- rlang::enquo(n)
  
  # Rename columns to standard names
  if(!rlang::quo_is_missing(x)) {
    if("x" %in% data_colnames) {
      warning("x is present in the data and specified as an argument.
The column specified in the argument x will be used.")
    }
    df <- df %>% dplyr::rename(x = !!x)
  }
  
  if(!rlang::quo_is_missing(y)) {
    if("y" %in% data_colnames) {
      warning("y is present in the data and specified as an argument.
The column specified in the argument y will be used.")
    }
    df <- df %>% dplyr::rename(y = !!y)
  }
  
  if(!rlang::quo_is_missing(n)) {
    if("n" %in% data_colnames) {
      warning("n is present in the data and specified as an argument.
The column specified in the argument n will be used.")
    }
    df <- df %>% dplyr::rename(n = !!n)
  }
  
  return(df)
  
}


