#' @section Options:
#' \describe{
#'   \item{`autospc.warn_missing_x`}{Whether to warn when rows are excluded
#'   because `x` is `NA`. `TRUE` unless set otherwise. Set
#'   `options(autospc.warn_missing_x = FALSE)` to omit this warning, for
#'   instance when drawing many charts to avoid many warnings. The warning
#'   carries the class `"autospc_missing_x_warning"`, so it can also be handled
#'   on its own with `withCallingHandlers()`.}
#'   \item{`autospc.rounded_constants`}{Whether to use the published rounded
#'   values of the antibiasing constants, rather than their exact values.
#'   `FALSE` unless set to `TRUE`, so the exact values are used by default. Set
#'   `options(autospc.rounded_constants = TRUE)` for limits that agree with a
#'   hand calculation from a published table of constants.}
#'   \item{`autospc.constrain_limits`}{Whether to constrain control limits to
#'   the range the plotted statistic can take: a count and a moving range at or
#'   above zero, a percentage between 0 and 100. `TRUE` unless set to `FALSE`.
#'   Set `options(autospc.constrain_limits = FALSE)` to draw the limits where
#'   the arithmetic puts them, which shows how wide they are but potentially
#'   puts them at values the statistic could not take. The vertical axis
#'   follows the limits either way. An MR chart has a lower limit of zero
#'   either way, as is standard.}
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
  "limit_change",
  "limit_extension",
  "log_entry",
  "lower_annotation_level",
  "lower_level",
  "median",
  "multiple_rows",
  "n",
  "new_period",
  "new_run",
  "non_missing",
  "num_rows",
  "period_count",
  "period_start",
  "period_type",
  "period_type.x",
  "period_type.y",
  "plot_period",
  "plotted_line",
  "prev_value",
  "prev_x",
  "row_index",
  "rule1",
  "rule1_distance",
  "rule2",
  "run_count",
  "run_start",
  "s",
  "series",
  "series.x",
  "series.y",
  "spread",
  "stage",
  "subgroup_mean",
  "subgroup_n",
  "ucl",
  "ucl.x",
  "ucl.y",
  "upper_annotation_level",
  "value",
  "x",
  "y"
))
