# The ranges of x in which the algorithm may not establish new limits


#' The row after the prevented range a candidate would start in
#'
#' A candidate starts in a range given by `prevent_establishment_in` where its
#' `x` is between the range's two ends, both included, an end given as NA
#' being open. Where it lies in more than one range, the row is the one after
#' the range that ends last.
#'
#' @param chart The chart being analysed.
#' @param table The limits table, one row per observation in the segment.
#' @param counter The row the candidate would start at.
#'
#' @return The first row of `table` after the range, or one more than the
#'   number of rows where the range runs to the end of the table; NULL where
#'   the candidate is in no range.
#' @noRd
row_after_prevented_range <- function(chart,
                                      table,
                                      counter) {
  ranges <- chart$prevent_establishment_in

  if (is.null(ranges)) {
    return(NULL)
  }

  x <- table$x[[counter]]

  contains <- vapply(
    ranges,
    function(bounds) {
      return((is.na(bounds[[1L]]) || x >= bounds[[1L]]) &&
        (is.na(bounds[[2L]]) || x <= bounds[[2L]]))
    },
    logical(1L)
  )

  if (!any(contains)) {
    return(NULL)
  }

  past_end <- nrow(table) + 1L

  rows_after <- vapply(
    ranges[contains],
    function(bounds) {
      if (is.na(bounds[[2L]])) {
        return(past_end)
      }

      row <- match(TRUE, table$x > bounds[[2L]])

      return(if (is.na(row)) past_end else row)
    },
    integer(1L)
  )

  return(max(rows_after))
}
