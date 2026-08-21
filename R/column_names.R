# Getting from the column names the user supplied to the names x, y and n that
# the rest of the package expects.
#
# Two functions, in the order they run. resolve_column_name() turns the x, y and
# n arguments of autospc() into column names. normalise_columns() then takes
# those columns out of the data under the names x, y and n, inside each
# construction helper.

#' Resolve a column argument to a column name
#'
#' A bare symbol or a string, with a missing argument meaning the column is
#' already named `fallback`.
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


#' The column name given by one of `facet_stages()`'s x, y and n arguments
#'
#' `facet_stages()` holds its arguments as expressions rather than as quosures,
#' because it forwards them to `autospc()`. An argument the caller left out
#' means the column is already named for the field.
#'
#' @param exprs The x, y and n expressions the caller gave, named.
#'
#' @return A character scalar.
#' @noRd
column_name_of <- function(exprs,
                           field) {

  if(!field %in% names(exprs)) {
    return(field)
  }

  return(rlang::as_name(exprs[[field]]))

}


#' Take the named columns out of a chart's data, under the names x, y and n
#'
#' `data` comes out carrying the columns the analysis uses and nothing else -
#' `x`, `y` and, where the class has one, `n` - each named for the field it
#' fills rather than for the column it came from. `data_original` still holds
#' everything the caller passed, under the names they passed it with.
#'
#' Selecting rather than renaming is what settles a column already named `x`
#' where the caller named a different column as x: the one they named is the one
#' kept, which is what they asked for, and the other goes with the rest of the
#' columns the analysis does not use.
#'
#' Called by each construction helper once all its fields are in place.
#'
#' @return the chart list, with `data` cut down to the named columns
#' @noRd
normalise_columns <- function(chart_list,
                              fields) {

  sources <- vapply(fields,
                    function(field) chart_list[[field]],
                    character(1))

  chart_list$data <- select_named_columns(df = chart_list$data,
                                          sources = sources)

  return(chart_list)

}


#' Select the columns named by a field-to-column mapping
#'
#' A column the caller named must be there, and its absence is their mistake to
#' hear about. A column named only by the fallback may be absent: that is P and
#' P' charts given individual binary observations, where no denominator column
#' is supplied and `n` falls back to `"n"`.
#'
#' @param sources A character vector of column names, named for the fields they
#'   fill.
#'
#' @return `df`, holding only the named columns, named for their fields
#' @noRd
select_named_columns <- function(df,
                                 sources) {

  wanted <- vapply(seq_along(sources),
                   function(i) {
                     sources[[i]] != names(sources)[[i]] ||
                       sources[[i]] %in% colnames(df)
                   },
                   logical(1))

  df <- df %>%
    dplyr::select(dplyr::all_of(sources[wanted]))

  return(df)

}
