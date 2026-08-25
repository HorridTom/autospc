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
              envir = asNamespace("autospc")))

}


#' The deprecated arguments of autospc()
#'
#' The arguments whose default is `deprecated()`.
#'
#' @return A character vector of argument names.
#' @noRd
autospc_deprecated_arguments <- function() {

  defaults <- formals(autospc)

  is_deprecated <- vapply(defaults,
                          function(default) {
                            identical(default, quote(deprecated()))
                          },
                          logical(1L))

  return(names(defaults)[is_deprecated])

}
