# Getting from the column names the user supplied to the names x, y and n that
# the rest of the package expects.
#
# Two functions, in the order they run. resolve_column_name() turns the x, y and
# n arguments of autospc() into column names. normalise_columns() then renames
# those columns in the data, inside each construction helper.

#' Resolve a column argument to a column name
#'
#' Mirrors what `rename_columns()` accepts: a bare symbol or a string, with a
#' missing argument meaning the column is already named `fallback`.
#'
#' @param column A quosure, as produced by `rlang::enquo()`.
#' @param fallback Character scalar used when `column` is missing.
#'
#' @return A character scalar.
#' @noRd
resolve_column_name <- function(column,
                                fallback) {

  if(rlang::quo_is_missing(column)) {
    return(fallback)
  }

  return(rlang::as_name(column))

}


#' Normalise the named columns of a chart's data
#'
#' Renames the columns named by the given fields to the field names themselves,
#' so that `data` carries `x`, `y` and, where the class has one, `n`. Called by
#' each construction helper once all its fields are in place.
#'
#'
#' @return the chart list, with `data` renamed
#' @noRd
normalise_columns <- function(chart_list,
                              fields) {

  for(field in fields) {

    chart_list$data <- rename_if_different(df = chart_list$data,
                                           target = field,
                                           source = chart_list[[field]])

  }

  return(chart_list)

}


#' Rename one column, if it is not already named as required
#'
#' Doing nothing when source and target match matters for P and P' charts given
#' individual binary observations: no denominator column is supplied, `n` falls
#' back to `"n"`, and there is no column to rename. A blind rename would error.
#'
#' Renaming is silent. `rename_columns()` warns when the data already holds a
#' column of the target name *and* the argument was supplied; that warning stays
#' there until it retires, to avoid users seeing it twice.
#'
#' @return `df`, with the source column renamed to target
#' @noRd
rename_if_different <- function(df,
                                target,
                                source) {

  if(identical(target, source)) {
    return(df)
  }

  df <- df %>%
    dplyr::rename(!!target := !!rlang::sym(source))

  return(df)

}
