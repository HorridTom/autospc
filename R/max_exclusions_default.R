# The warning that announces the change of the max_exclusions default from 3
# to 0 in autospc 0.3.0. Removed in 0.3.0, when the default changes.


#' Warn that the max_exclusions default will change, where it affects a call
#'
#' Called by `autospc()` and `facet_stages()` when the caller did not set
#' `max_exclusions`. Warns once for the call if any of its charts excluded a
#' point from its limit calculations, because those are the analyses whose
#' results the new default will change.
#'
#' @param charts The analysed charts of the call.
#'
#' @return NULL, invisibly.
#' @noRd
warn_max_exclusions_default <- function(charts) {
  excluded_any <- vapply(
    charts,
    function(chart) any(chart$result$table$excluded %in% TRUE),
    logical(1L)
  )

  if (any(excluded_any)) {
    rlang::warn(
      paste(
        "This analysis excluded points from its limit calculations using the",
        "default `max_exclusions = 3`. The default will change to 0 in",
        "autospc 0.3.0, which will change these results. Set `max_exclusions`",
        "explicitly to keep the current behaviour (3) or adopt the new one",
        "(0); either silences this warning."
      ),
      class = "autospc_max_exclusions_default_warning"
    )
  }

  return(invisible(NULL))
}
