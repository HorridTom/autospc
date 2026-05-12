
#' Validate chart_type argument
#'
#' Checks that `chart_type` is a single, non-NULL character string corresponding
#' to a supported SPC chart type. Intended for internal use only.
#'
#' @param chart_type Character scalar specifying chart type.
#'
#' @return Invisibly returns TRUE if valid; otherwise errors.
#' @noRd
validate_chart_type <- function(chart_type) {
  
  allowed_chart_types <- c("XMR", "MR", "C", "C'", "P", "P'")
  
  # NULL check
  if (is.null(chart_type)) {
    
    lifecycle::deprecate_stop(
      when = "0.0.0.9008",
      what = I("chart_type  = NULL"),
      details = I(paste("Please explicitly pass the desired chart type.",
                        "Available chart types are: ",
                        paste(allowed_chart_types, collapse = ", "),
                        ".")))
  }
  
  # Length check
  if (length(chart_type) != 1) {
    stop(
      "chart_type must have length one. ",
      "Available chart types are: ",
      paste(allowed_chart_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  # Type check (defensive)
  if (!is.character(chart_type)) {
    stop(
      "chart_type must be a character string.",
      call. = FALSE
    )
  }
  
  # Value check
  if (!chart_type %in% allowed_chart_types) {
    stop(
      sprintf(
        "Invalid chart_type: '%s'. Available chart types are: %s.",
        chart_type,
        paste(allowed_chart_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


#' validate_data_column_spec
#'
#' Checks that the data columns specified by the user match the necessary input
#' data type(s) for the `chart_type`. Intended for internal use only.
#'
#' @param chart_type Character scalar specifying chart type.
#'
#' @return If criteria are met, returns df, modified if needed with a warning.
#' If criteria are not met, errors.
#' @noRd
validate_data_column_spec <- function(df,
                                      chart_type) {
  
  # valid_data_column_specs <- get_data_column_specs(chart_type)
  # if(nrow(valid_data_column_specs) == 0L) return(FALSE)
  
  df_cn <- colnames(df)
  data_col_types <- sapply(df %>%
                             dplyr::select(any_of(c("x","y","n"))),
                           typeof)
  
  y_present <- "y" %in% names(data_col_types)
  n_present <- "n" %in% names(data_col_types)
  y_type <- ifelse(y_present,
                   data_col_types[["y"]],
                   NA_character_)
  n_type <- ifelse(n_present,
                   data_col_types[["n"]],
                   NA_character_)
  
  criteria_met <- FALSE
  
  switch(chart_type,
         "P'" =,
         "P" = {
           
           if(!y_present) {
             stop(paste("y not specified. For P and P' charts, y must be",
                        "specified."),
                  call. = FALSE)
           }
           
           if(!n_present) {
             if(y_type == "logical") {
               criteria_met <- TRUE
             } else {
               stop(paste("n is not specified and y is not of type logical.",
                          "For P and P' charts, if n is not specified, y must",
                          "be of type logical."),
                    call. = FALSE)
             }
           } else {
             # n present
             
             if(!(y_type %in% c("integer", "double"))) {
               stop(
                 paste(
                   "For a P or P' chart with n specified, y must be of type",
                   "integer or double."),
                 call. = FALSE
               )
               
             } else if(y_type == "double" &&
                       any(!is.wholenumber(df$y),
                           na.rm = TRUE)) {
               df <- df %>%
                 dplyr::mutate(y = round(y))
               warning(paste("At least one element of y has non-zero",
                             "fractional part. Rounding to the nearest whole",
                             " number.\nP and P' charts with n specified",
                             "require y to be a count, i.e. whole numbers only."
               ),
               call. = FALSE)
               y_criteria_met <- TRUE
               
             } else {
               # y is integer or wholenumber double
               y_criteria_met <- TRUE
             }
             
             if(!(n_type %in% c("integer", "double"))) {
               stop(
                 paste(
                   "For a P or P' chart with n specified, n must be of type",
                   "integer or double."),
                 call. = FALSE
               )
               
             } else if(n_type == "double" &&
                       any(!is.wholenumber(df$n),
                           na.rm = TRUE)) {
               df <- df %>%
                 dplyr::mutate(n = round(n))
               warning(paste("At least one element of n has non-zero",
                             "fractional part. Rounding to the nearest whole",
                             " number.\nP and P' charts with n specified",
                             "require n to be a count, i.e. whole numbers only."
               ),
               call. = FALSE)
               n_criteria_met <- TRUE
               
             } else {
               # n is integer or wholenumber double
               n_criteria_met <- TRUE
             }
             
             criteria_met <- y_criteria_met && n_criteria_met
           }
           
         },
         "C'" =,
         "C" = {
           
           if(!y_present) {
             stop(paste("y not specified. For C and C' charts, y must be",
                        "specified."),
                  call. = FALSE)
           }
           
           if(!(y_type %in% c("integer", "double"))) {
             stop(
               paste(
                 "For a C or C' chart, y must be of type integer or double."),
               call. = FALSE
             )
             
           } else if(y_type == "double" &&
                     any(!is.wholenumber(df$y),
                         na.rm = TRUE)) {
             df <- df %>%
               dplyr::mutate(y = round(y))
             warning(paste("At least one element of y has non-zero fractional",
                           "part. Rounding to the nearest whole number.\n",
                           "C and C' charts are for count data, i.e. whole",
                           "numbers only."),
                     call. = FALSE)
             criteria_met <- TRUE
             
           } else {
             # y is integer or wholenumber double
             criteria_met <- TRUE
           }
         },
         "MR" =,
         "XMR" = {
           
           if(!y_present) {
             stop(paste("y not specified. For XMR charts, y must be",
                        "specified."),
                  call. = FALSE)
           }
           
           if(!(y_type %in% c("integer", "double"))) {
             stop(
               paste(
                 "For an XMR chart, y must be of type integer or double."),
               call. = FALSE
             )
           } else {
             criteria_met <- TRUE
           }
           
         }
  )
  
  if(!criteria_met) {
    stop(paste("One or more of the data columns do not meet the requirements",
               "for the specified chart type."),
         call. = FALSE)
  }
  
  return(df)
  
}


is.wholenumber <- function(x,
                           tol = .Machine$double.eps^0.5)  {
  return(abs(x - round(x)) < tol)
  
}
