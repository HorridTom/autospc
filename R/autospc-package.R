#' @section Options:
#' \describe{
#'   \item{`autospc.warn_missing_x`}{Whether to warn when rows are excluded
#'   because `x` is `NA`. `TRUE` unless set otherwise. Set 
#'   `options(autospc.warn_missing_x = FALSE)` to omit this warning, for
#'   instance when drawing many charts to avoid many warnings. The warning
#'   carries the class `"autospc_missing_x_warning"`, so it can also be handled
#'   on its own with `withCallingHandlers()`.}
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom fpCompare %<<%
#' @importFrom fpCompare %<=%
#' @importFrom fpCompare %==%
#' @importFrom fpCompare %>=%
#' @importFrom fpCompare %>>%
## usethis namespace: end
NULL


# Column names used inside dplyr and ggplot2 calls, which R CMD check reads as
# undefined variables.
utils::globalVariables(c(
  ".",
  "above_cl",
  "above_or_below_cl",
  "amr",
  "annotation_curvature",
  "annotation_level",
  "break_point",
  "cl",
  "cl.x",
  "cl.y",
  "cl_label",
  "counter",
  "cumulative_num_non_missing",
  "excluded",
  "excluded.x",
  "excluded.y",
  "highlight",
  "interpretation",
  "lagged_above_or_below_cl",
  "lagged_period_type",
  "lcl",
  "lcl.x",
  "lcl.y",
  "lcl_display",
  "limit_change",
  "log_entry",
  "lower_annotation_level",
  "lower_level",
  "lrl",
  "median",
  "mr",
  "multiple_rows",
  "n",
  "new_period",
  "new_run",
  "non_missing_y",
  "num_rows",
  "period_count",
  "period_start",
  "period_type",
  "period_type.x",
  "period_type.y",
  "plot_period",
  "prev_value",
  "prev_x",
  "row_index",
  "rule1",
  "rule1_distance",
  "rule2",
  "run_count",
  "run_start",
  "series",
  "stage",
  "ucl",
  "ucl.x",
  "ucl.y",
  "ucl_display",
  "upper_annotation_level",
  "value",
  "x",
  "y",
  "y.x",
  "y.y"
))
