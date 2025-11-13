
preprocess_inputs <- function(
    df,
    chartType,
    title = NULL,
    subtitle = NULL,
    upper_annotation_sf = NULL,
    lower_annotation_sf = NULL,
    override_annotation_dist = NULL,
    override_annotation_dist_P = NULL) {
  
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
  
  #decide whether the chart is C or P depending on data format if not specified 
  if(is.null(chartType)) {
    
    lifecycle::deprecate_warn(
      when = "0.0.0.9008",
      what = I("chartType  = NULL"),
      details = I("Please explicitly pass the desired chart type")
    )
    
    if(all(c("x", "y") %in% colnames(df))) {
      chartType <- "C'"
    } else if(all(c("x", "n", "y") %in% colnames(df))) {
      chartType <- "P'"
    } else {
      print(paste0("The data you have input is not in the correct format. ",
                   "For C charts, data must contain at least columns 'x' and ",
                   "'y'. For P charts data must contain at least 'x', 'n' and ",
                   "'y' columns."))
    }
  }
  
  if(chartType == "MR") {
    mrs <- get_mrs(y = df$y)
    df <- df %>% dplyr::mutate(y = mrs)
  }
  
  # Check annotation arguments
  if(!is.null(override_annotation_dist) |
     !is.null(override_annotation_dist_P)) {
    
    lifecycle::deprecate_warn(
      when = "0.0.0.9010",
      what = I(paste0("plot_auto_SPC(override_annotation_dist,",
                      "override_annotation_dist_P)")),
      details = I(paste0("Please use `plot_auto_SPC(upper_annotation_sf, ",
                         "lower_annotation_sf)` instead. ",
                         "Note that equivalent new arguments can be obtained ",
                         "from the old by transforming as follows: 1+1/x. ",
                         "For example, override_annotation_dist = 10 is ",
                         "equivalent to upper_annotation_sf = 1.1."))
    )
    
    if(!is.null(override_annotation_dist_P) & startsWith(chartType, "P")) {
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
    upper_annotation_sf <- ifelse(startsWith(chartType, "P"),
                                  1.04,
                                  1.1)
  }
  
  if(is.null(lower_annotation_sf)) {
    lower_annotation_sf <- 2 - upper_annotation_sf
  }
  
  
  return(list(
    df = df,
    chartType = chartType,
    title = title,
    subtitle = subtitle,
    xType = xType,
    upper_annotation_sf = upper_annotation_sf,
    lower_annotation_sf = lower_annotation_sf
  ))
  
}
