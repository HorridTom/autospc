
#' facet_stages
#' @inheritParams autospc
#' @param split_rows A vector of row numbers specifying the stages to display
#' results at. Names specify facet strip labels.
#' @param ... Arguments passed to [autospc::autospc()]
#'
#' @returns Faceted plot showing results of [autospc::autospc()] at
#' different stages as specified by split_rows
#'
#' @examples
#' # Show progression of C' chart for count of monthly attendances over time
#' facet_stages(
#'   ed_attendances_monthly,
#'   split_rows = c(30L, 60L, 90L),
#'   chartType = "C'",
#'   x = Month_Start,
#'   y = Att_All, 
#'   x_break = 365
#' )
#' 
#' @export  
facet_stages <- function(data,
                         split_rows,
                         plotChart = TRUE,
                         ...) {
  
  dots_exprs <- rlang::exprs(...)
  
  if(dots_exprs$chartType == "XMR") {
    if(!("showMR" %in% names(dots_exprs))) {
      
      dots_exprs$showMR <- FALSE
      
    } else if (dots_exprs$showMR) {
      warning(paste("`facet_stages()` does not support `showMR = TRUE`.",
                    "Setting `showMR` to `FALSE`. To facet an MR chart by",
                    "stages use `facet_stages()` with `chartType = MR`."))
      
      dots_exprs$showMR <- FALSE
    }
  }
  
  dots_exprs$plotChart <- FALSE
  
  xyn_exprs <- dots_exprs[which(names(dots_exprs) %in% c("x", "y", "n"))]
  
  df_rn <- eval(rlang::call2("rename_columns",
                             df = data,
                             !!!xyn_exprs))
  
  preprocess_inputs_args <- names(formals(autospc:::preprocess_inputs))
  ppi_args <- dots_exprs[which(names(dots_exprs) %in% preprocess_inputs_args)]
  
  # Preprocess inputs
  preprocessed_vars <- eval(rlang::call2("preprocess_inputs",
                                         df = df_rn,
                                         !!!ppi_args))
  
  chartType           <- preprocessed_vars$chartType
  title               <- preprocessed_vars$title
  subtitle            <- preprocessed_vars$subtitle
  xType               <- preprocessed_vars$xType
  upper_annotation_sf <- preprocessed_vars$upper_annotation_sf
  lower_annotation_sf <- preprocessed_vars$lower_annotation_sf
  
  split_rows <- sort(split_rows)
  
  # Ensure the last split row is the end of the data
  if(split_rows[length(split_rows)] != nrow(data)) {
    split_rows <- c(split_rows,
                    nrow(data))
  }
  
  
  data_splits_list <- create_splits_list(df = data,
                                         split_rows = split_rows)
  
  results_splits_list <- lapply(
    data_splits_list,
    function(x) {
      eval(rlang::call2("autospc",
                        data = x,
                        !!!dots_exprs))
    }
  )
  
  results_data <- dplyr::bind_rows(
    results_splits_list,
    .id = "stage"
  )
  
  if(!plotChart) {
    return(results_data)
  }
  
  postprocess_args <- names(formals(autospc:::postprocess))
  pp_args <- dots_exprs[which(names(dots_exprs) %in% postprocess_args)]
  
  # Postprocess data
  postprocessing_vars <- eval(rlang::call2("postprocess",
                                           df = results_data,
                                           xType = xType,
                                           !!!pp_args))
  
  override_x_title   <- postprocessing_vars$override_x_title
  override_y_title   <- postprocessing_vars$override_y_title
  num_non_missing_y  <- postprocessing_vars$num_non_missing_y
  start_x            <- postprocessing_vars$start_x
  x_max              <- postprocessing_vars$x_max
  end_x              <- postprocessing_vars$end_x
  ylimhigh           <- postprocessing_vars$ylimhigh
  ylimlow            <- postprocessing_vars$ylimlow
  
  csp_args <- names(formals(autospc:::create_spc_plot))
  c_args <- dots_exprs[which(names(dots_exprs) %in% csp_args)]
  
  # Create SPC plot
  sp <- eval(rlang::call2("create_spc_plot",
                          df = results_data,
                          split_rows = split_rows,
                          ylimlow = ylimlow,
                          ylimhigh = ylimhigh,
                          xType = xType,
                          x_max = x_max,
                          start_x = start_x,
                          end_x = end_x,
                          num_non_missing_y = num_non_missing_y,
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
                            df[1:x,]
                          })
  }
  
  return(data_splits)
  
}

