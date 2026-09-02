# The values autospc()'s arguments take when the caller does not give them.
#
# autospc() is where every default is declared, and it is the documented
# signature. Everything else in the package that needs a default reads it from
# there through autospc_default(), so that no default is declared twice and two
# declarations cannot disagree.


#' The default value of one autospc() argument
#'
#' `formals()` returns each default as the expression it was written as rather
#' than as a value, so it has to be evaluated. Evaluating it in the package
#' namespace is what R does when `autospc()` is called, and is what makes a
#' default such as `deprecated()`, imported from lifecycle, resolvable.
#'
#' @param name Character scalar naming an argument of `autospc()`.
#'
#' @return The default value of that argument.
#' @noRd
autospc_default <- function(name) {
  default <- formals(autospc)[[name]]

  return(eval(default,
    envir = asNamespace("autospc")
  ))
}


#' The arguments of autospc() that must be TRUE or FALSE
#'
#' The arguments whose default is a single TRUE or FALSE. They are read from the
#' signature so any arguments added are covered without a second list to keep in
#' step.
#'
#' @return A character vector of argument names.
#' @noRd
autospc_flag_arguments <- function() {
  names_wanted <- setdiff(
    names(formals(autospc)),
    c("data", "x", "y", "n", autospc_deprecated_arguments())
  )

  is_flag <- vapply(
    names_wanted,
    function(name) {
      default <- autospc_default(name)

      return(is.logical(default) && length(default) == 1L)
    },
    logical(1L)
  )

  return(names_wanted[is_flag])
}


#' The numeric arguments of autospc(), and what each accepts
#'
#' The kinds are:
#' \itemize{
#'   \item `count` — a whole number of one or more
#'   \item `count_from_zero` — a whole number of zero or more
#'   \item `loops` — a whole number of zero or more, or Inf
#'   \item `non_negative` — a number of zero or more
#'   \item `positive` — a number above zero
#'   \item `number` — any single finite number
#'   \item `axis_value` — a single value of whatever type the horizontal axis
#'     holds, which is not restricted to numbers
#' }
#'
#' Unlike the Boolean arguments these cannot be read off the signature, because
#' the default does not say what the argument accepts. This is the one place the
#' constraint is written.
#'
#' @return A named character vector, argument name to kind.
#' @noRd
autospc_numeric_arguments <- function() {
  return(c(
    period_min = "count",
    baseline_length = "count",
    shift_rule_threshold = "count",
    floating_median_n = "count",
    max_exclusions = "count_from_zero",
    mr_screen_max_loops = "loops",
    centre_line_tolerance = "non_negative",
    point_size = "positive",
    line_width_sf = "positive",
    annotation_size = "positive",
    annotation_arrow_curve = "number",
    upper_annotation_sf = "number",
    lower_annotation_sf = "number",
    override_y_lim = "number",
    x_break = "axis_value",
    x_pad_end = "axis_value",
    extend_limits_to = "axis_value"
  ))
}


#' The deprecated arguments of autospc()
#'
#' The arguments whose default is `deprecated()`.
#'
#' @return A character vector of argument names.
#' @noRd
autospc_deprecated_arguments <- function() {
  defaults <- formals(autospc)

  is_deprecated <- vapply(
    defaults,
    function(default) {
      identical(default, quote(deprecated()))
    },
    logical(1L)
  )

  return(names(defaults)[is_deprecated])
}
