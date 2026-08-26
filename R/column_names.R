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


#' The column name one of `facet_stages()`'s x, y and n arguments gives
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


#' Select the columns the analysis uses, and name them x, y and n
#'
#' `chart_list$data` is returned holding only `x`, `y` and, where the class has
#' one, `n`, each renamed from the column the caller named for that argument.
#' `chart_list$data_original` still holds every column the caller passed, under
#' its original name.
#'
#' `dplyr::select()` is used rather than `dplyr::rename()` so that data holding
#' a column called `x`, where the caller passed a different column as `x`, is
#' handled without error: the column the caller named is selected, and the one
#' called `x` is dropped along with the other unused columns. `dplyr::rename()`
#' would error, because the result would hold two columns called `x`.
#'
#' Called by each construction helper once all its fields are in place.
#'
#' @return the chart list, with `data` reduced to the selected columns
#' @noRd
normalise_columns <- function(chart_list,
                              fields) {

  sources <- vapply(fields,
                    function(field) chart_list[[field]],
                    character(1))

  chart_list$data <- select_named_columns(data = chart_list$data,
                                          sources = sources)

  return(chart_list)

}


#' Select and rename columns, given the column name for each field
#'
#' A column whose name differs from the field name was named by the caller, so
#' it must be present and `dplyr::all_of()` errors if it is not. A column whose
#' name equals the field name may be absent, and is skipped: this is the case
#' for P and P' charts given individual binary observations, where no
#' denominator column is supplied and `n` takes its default value of `"n"`.
#'
#' @param sources A character vector of column names, named for the fields they
#'   fill.
#'
#' @return `data`, holding only the named columns, named for their fields
#' @noRd
select_named_columns <- function(data,
                                 sources) {

  wanted <- vapply(seq_along(sources),
                   function(i) {
                     sources[[i]] != names(sources)[[i]] ||
                       sources[[i]] %in% colnames(data)
                   },
                   logical(1))

  data <- data %>%
    dplyr::select(dplyr::all_of(sources[wanted]))

  return(data)

}
