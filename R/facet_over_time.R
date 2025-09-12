
#' facet_over_time
#' @inheritParams plot_auto_SPC
#' @param split_rows A vector of row numbers specifying the stages to display
#' results at
#' @param ... Arguments passed to [autospc::plot_auto_SPC()]
#'
#' @returns faceted plot showing results at different stages as specified by
#' split_rows
#' @export
#'
facet_over_time <- function(df,
                            split_rows,
                            ...) {
  
  dots_exprs <- rlang::exprs(...)
  dots_exprs$plotChart <- FALSE
  
  xType <- class(df$x)
  
  split_rows <- sort(split_rows)
  
  # Ensure the last split row is the end of the data
  if(split_rows[length(split_rows)] != nrow(df)) {
    split_rows <- c(split_rows,
                    nrow(df))
  }
  
  
  data_splits_list <- create_splits_list(df = df,
                                         split_rows = split_rows)
  
  
  # preprocess_inputs_args <- names(formals(autospc:::preprocess_inputs))
  # ppi_args <- dots_exprs[which(names(dots_exprs) %in% preprocess_inputs_args)]
  # 
  # # Preprocess inputs
  # preprocessed_vars <- eval(rlang::call2("preprocess_inputs",
  #                                        df = df,
  #                                        !!!ppi_args))
  # 
  # df                  <- preprocessed_vars$df
  # chartType           <- preprocessed_vars$chartType
  # title               <- preprocessed_vars$title
  # subtitle            <- preprocessed_vars$subtitle
  # xType               <- preprocessed_vars$xType
  # upper_annotation_sf <- preprocessed_vars$upper_annotation_sf
  # lower_annotation_sf <- preprocessed_vars$lower_annotation_sf
  
  results_splits_list <- lapply(
    data_splits_list,
    function(x) {
      eval(rlang::call2("plot_auto_SPC",
                        df = x,
                        !!!dots_exprs))
    }
  )
  
  results_data <- dplyr::bind_rows(
    results_splits_list,
    .id = "stage"
  )
  
  # postprocess_args <- names(formals(autospc:::postprocess_spc))
  # pp_args <- dots_exprs[which(names(dots_exprs) %in% postprocess_args)]
  # 
  # # Postprocess data
  # rdproc <- eval(rlang::call2("postprocess_spc",
  #                         df = results_data,
  #                         ylimhigh = 1000,
  #                         x_max = as.Date("2017-01-01"),
  #                         !!!pp_args))
  
  csp_args <- names(formals(autospc:::create_spc_plot))
  c_args <- dots_exprs[which(names(dots_exprs) %in% csp_args)]
  
  # csp
  sp <- eval(rlang::call2("create_spc_plot",
                          df = results_data,
                          split_rows = split_rows,
                          ylimlow = 0L,
                          ylimhigh = 17000,
                          xType = "Date",
                          x_max = as.Date("2024-05-31"),
                          start_x = as.Date("2015-05-31"),
                          end_x = as.Date("2024-05-31"),
                          num_non_missing_y = 200,
                          !!!c_args))
  
  return(sp)
  
}


create_splits_list <- function(df,
                               split_rows) {
  
  if(is.null(split_rows)) {
    
    data_splits <- list(df)
    
  } else {
    
    data_splits <- lapply(split_rows,
                          function(x) {
                            df %>%
                              dplyr::filter(dplyr::row_number() <= x)
                          })
  }
  
  return(data_splits)
  
}

