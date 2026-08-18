
preprocess_inputs <- function(
    df,
    chart_type,
    title = NULL,
    subtitle = NULL) {
  
  validate_chart_type(chart_type)
  
  df <- validate_data_column_spec(df = df,
                                  chart_type = chart_type)
  
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
  
  return(list(
    df = df,
    chart_type = chart_type,
    title = title,
    subtitle = subtitle,
    xType = xType
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


#' Order a chart's series by x
#'
#' The algorithm walks the data in row order, so the rows have to be in x order
#' before it runs, and before `prepare_data()` derives anything from their
#' order - an MR chart's moving ranges are differences between neighbouring
#' rows.
#'
#' `dplyr::arrange()` is stable, so rows sharing an x keep the order they
#' arrived in. Missing x values sort to the end.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
order_series <- function(chart) {

  chart$data <- chart$data %>%
    dplyr::arrange(x)

  return(chart)

}


