# Everything the package decides about missing values.


#' Whether to warn about rows the package had to exclude due to missing x
#'
#' Read from the option rather than from an argument of `autospc()` so that a
#' caller drawing many charts can set it once for the session. Any value other
#' than FALSE warns, so that a mis-typed option does not silence the warning.
#'
#' @return TRUE or FALSE
#' @noRd
warn_missing_x_enabled <- function() {
  return(!isFALSE(getOption("autospc.warn_missing_x")))
}


#' Drop the rows whose x is missing
#'
#' A row with no x cannot be placed on the horizontal axis, so it cannot be
#' plotted and cannot be put in order for the algorithm to walk. There is
#' nothing the package can do with it but leave it out.
#'
#' This runs once per call to `autospc()` or `facet_stages()`, before any chart
#' is built, so that everything downstream is working with a series that has an
#' x for every row.
#'
#' The column may be absent, where the caller named one the data does not hold.
#' The class validator reports that; nothing is dropped here.
#'
#' @param data The data frame the caller supplied.
#' @param x_column Character scalar naming the column used as x.
#'
#' @return `data`, without the rows whose x is missing.
#' @noRd
drop_missing_x <- function(data,
                           x_column) {
  if (!x_column %in% colnames(data)) {
    return(data)
  }

  missing_x <- is.na(data[[x_column]])
  dropped <- sum(missing_x)

  if (dropped == 0L) {
    return(data)
  }

  if (warn_missing_x_enabled()) {
    rlang::warn(
      sprintf(
        paste(
          "%d %s excluded because %s no %s value.",
          "Set options(autospc.warn_missing_x = FALSE) to stop this warning."
        ),
        dropped,
        if (dropped == 1L) "row was" else "rows were",
        if (dropped == 1L) "it has" else "they have",
        x_column
      ),
      class = "autospc_missing_x_warning"
    )
  }

  return(data[!missing_x, , drop = FALSE])
}
