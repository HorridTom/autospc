#' Rule sets for highlighting rule breaks
#'
#' A rule set says which patterns of points are highlighted on a chart, in
#' addition to two rules every chart has: rule 1, a point outside the control
#' limits, and the shift rule, a run of points on one side of the centre line
#' whose length is `shift_rule_threshold`. The other rules are built with
#' `zone_rule()`, `trend_rule()` and `alternating_rule()`.
#'
#' `rule_set()` takes either the name of one of the sets below, or the rules of
#' a set of your own. Printing a named set shows its rules, so a set of your own
#' can start from one of them.
#'
#' @section Named sets:
#' \describe{
#'   \item{`"provost_murray"`}{Provost and Murray (2011), the default. Rule 1,
#'   a single point outside the limits; rule 2, the shift rule, a run of eight
#'   or more points on one side of the centre line; rule 3, six consecutive
#'   points increasing or decreasing; rule 4, two out of three consecutive
#'   points beyond two standard deviations, on either side of the centre line;
#'   rule 5, fifteen consecutive points within one standard deviation of the
#'   centre line.}
#'   \item{`"western_electric"`}{The Western Electric Company (1956) handbook's
#'   tests 1 to 4. Test 1, a single point outside the limits; test 2, two out of
#'   three successive points beyond two standard deviations, on one side of the
#'   centre line; test 3, four out of five successive points beyond one
#'   standard deviation, on one side; test 4, the shift rule, a run of eight
#'   successive points on one side.}
#'   \item{`"nelson"`}{Nelson (1984), tests 1 to 8. Test 1, a single point
#'   outside the limits; test 2, the shift rule, nine points in a row on one
#'   side of the centre line; test 3, six points in a row steadily increasing
#'   or decreasing; test 4, fourteen points in a row alternating up and down;
#'   test 5, two out of three points in a row beyond two standard deviations,
#'   on one side; test 6, four out of five points in a row beyond one standard
#'   deviation, on one side; test 7, fifteen points in a row within one
#'   standard deviation, on either side; test 8, eight points in a row beyond
#'   one standard deviation, on either side.}
#' }
#'
#' Each named set numbers its rules as its source does, and sets the length of
#' the shift rule its source gives.
#'
#' @param ... Either the name of a set, as a single string, or the rules of the
#'   set, each made by `zone_rule()`, `trend_rule()` or `alternating_rule()`.
#'   The rules are listed in increasing order of id.
#' @param shift_id The id of the shift rule in this set, a whole number of two
#'   or more. Not given with the name of a set.
#' @param shift_length The length of the shift rule in the set's source, a
#'   whole number of one or more. It is the default for `shift_rule_threshold`.
#'   Not given with the name of a set.
#' @param label The name of the set, used in the chart's caption, or `NULL`.
#'   Not given with the name of a set.
#'
#' @return An `autospc_rule_set`.
#'
#' @references
#' Provost, L. P. and Murray, S. K. (2011). *The Health Care Data Guide:
#' Learning from Data for Improvement*. Jossey-Bass.
#'
#' Western Electric Company (1956). *Statistical Quality Control Handbook*.
#' Western Electric Company.
#'
#' Nelson, L. S. (1984). The Shewhart control chart - tests for special causes.
#' *Journal of Quality Technology*, 16(4), 237-239.
#' \doi{10.1080/00224065.1984.11978921}
#'
#' @seealso [zone_rule()], [trend_rule()], [alternating_rule()]
#'
#' @examples
#' rule_set("nelson")
#'
#' # Provost and Murray's rules without the trend rule
#' rule_set(
#'   zone_rule(k = 2, m = 3, beyond = 2, side = "either", id = 4),
#'   zone_rule(k = 15, m = 15, within = 1, side = "either", id = 5),
#'   shift_id = 2,
#'   shift_length = 8,
#'   label = "Provost and Murray without the trend rule"
#' )
#' @export
rule_set <- function(...,
                     shift_id,
                     shift_length,
                     label = NULL) {
  rules <- list(...)

  if (length(rules) == 1L && is.character(rules[[1L]])) {
    if (!missing(shift_id) || !missing(shift_length) || !missing(label)) {
      rlang::abort(
        paste(
          "`shift_id`, `shift_length` and `label` are not given with the",
          "name of a set."
        )
      )
    }

    return(named_rule_set(rules[[1L]]))
  }

  if (missing(shift_id)) {
    rlang::abort("`shift_id` must be given: the id of the shift rule.")
  }
  if (missing(shift_length)) {
    rlang::abort(
      "`shift_length` must be given: the length of the shift rule."
    )
  }

  is_rule <- vapply(
    rules, function(rule) inherits(rule, "autospc_rule"),
    logical(1L)
  )
  if (!all(is_rule)) {
    rlang::abort(paste(
      "Each rule of a set must be made by `zone_rule()`, `trend_rule()` or",
      "`alternating_rule()`."
    ))
  }

  shift_id <- match_number(shift_id, "shift_id", kind = "count")
  if (shift_id == 1) {
    rlang::abort(paste(
      "`shift_id` must not be 1, the id of the rule for a point outside the",
      "limits."
    ))
  }
  shift_length <- match_number(shift_length, "shift_length", kind = "count")
  if (!is.null(label)) {
    label <- match_string(label, "label")
  }

  rules <- with_rule_ids(unname(rules), shift_id = as.integer(shift_id))

  return(validate_autospc_rule_set(new_autospc_rule_set(
    rules = rules,
    shift_id = as.integer(shift_id),
    shift_length = as.integer(shift_length),
    label = label
  )))
}


#' A zone rule
#'
#' A zone rule is broken where `k` out of `m` consecutive points lie in a zone,
#' measured from the centre line in standard deviations: three standard
#' deviations is the distance from the centre line to a control limit. Every
#' point of the pattern in the zone is highlighted.
#'
#' @param k The number of points that must lie in the zone, a whole number of
#'   one or more.
#' @param m The number of consecutive points those `k` are taken from, a whole
#'   number of at least `k`.
#' @param beyond The zone starts this many standard deviations from the centre
#'   line, and a point exactly this far from it is not in the zone. `NULL` for
#'   a zone that starts at the centre line.
#' @param within The zone ends this many standard deviations from the centre
#'   line, and a point exactly this far from it is in the zone. `NULL` for a
#'   zone with no outer end, so that points outside the limits are in it.
#' @param side `"either"` where the points may be on either side of the centre
#'   line, or `"same"` where they must all be on the same side.
#' @param id The rule's number, used in the name of its column `rule<id>`, and
#'   to order the rules. A whole number other than 1 and the set's `shift_id`.
#'   `NULL` to number the rules of a set in order.
#' @param label The text for the rule in the chart's legend. `NULL` for a
#'   description written from the rule's arguments.
#'
#' @return An `autospc_rule`.
#'
#' @seealso [rule_set()]
#'
#' @examples
#' # two out of three consecutive points more than two standard deviations from
#' # the centre line, on the same side of it
#' zone_rule(k = 2, m = 3, beyond = 2, side = "same")
#' @export
zone_rule <- function(k,
                      m,
                      beyond = NULL,
                      within = NULL,
                      side = "either",
                      id = NULL,
                      label = NULL) {
  k <- match_number(k, "k", kind = "count")
  m <- match_number(m, "m", kind = "count")
  if (k > m) {
    rlang::abort(sprintf(
      "`k` must be at most `m`: %s points cannot be taken from %s.", k, m
    ))
  }

  if (is.null(beyond) && is.null(within)) {
    rlang::abort("At least one of `beyond` and `within` must be given.")
  }
  if (!is.null(beyond)) {
    beyond <- match_number(beyond, "beyond", kind = "non_negative")
  }
  if (!is.null(within)) {
    within <- match_number(within, "within", kind = "positive")
  }
  if (!is.null(beyond) && !is.null(within) && beyond >= within) {
    rlang::abort("`beyond` must be less than `within`.")
  }

  side <- rlang::arg_match0(side, values = c("either", "same"))

  return(new_autospc_rule(
    list(
      k = as.integer(k),
      m = as.integer(m),
      beyond = beyond,
      within = within,
      side = side
    ),
    kind = "zone",
    id = match_rule_id(id),
    label = match_rule_label(label)
  ))
}


#' A trend rule
#'
#' A trend rule is broken where `k` consecutive points are each higher than the
#' one before, or each lower. A point equal to the one before it neither
#' continues nor ends a trend. Every point of the trend is highlighted.
#'
#' @param k The number of points in the trend, a whole number of two or more.
#' @inheritParams zone_rule
#'
#' @return An `autospc_rule`.
#'
#' @seealso [rule_set()]
#'
#' @examples
#' trend_rule(k = 6)
#' @export
trend_rule <- function(k,
                       id = NULL,
                       label = NULL) {
  k <- match_number(k, "k", kind = "count")
  if (k < 2) {
    rlang::abort("`k` must be at least 2: a trend needs two points.")
  }

  return(new_autospc_rule(
    list(k = as.integer(k)),
    kind = "trend",
    id = match_rule_id(id),
    label = match_rule_label(label)
  ))
}


#' An alternating rule
#'
#' An alternating rule is broken where `k` consecutive points go up and down in
#' turn: each step from one point to the next is in the opposite direction to
#' the step before it. A point equal to the one before it ends the pattern.
#' Every point of the pattern is highlighted.
#'
#' @param k The number of points in the pattern, a whole number of three or
#'   more.
#' @inheritParams zone_rule
#'
#' @return An `autospc_rule`.
#'
#' @seealso [rule_set()]
#'
#' @examples
#' alternating_rule(k = 14)
#' @export
alternating_rule <- function(k,
                             id = NULL,
                             label = NULL) {
  k <- match_number(k, "k", kind = "count")
  if (k < 3) {
    rlang::abort(
      "`k` must be at least 3: going up and down in turn needs three points."
    )
  }

  return(new_autospc_rule(
    list(k = as.integer(k)),
    kind = "alternating",
    id = match_rule_id(id),
    label = match_rule_label(label)
  ))
}


#' Print a rule set
#'
#' One line for each rule, in order of id: its column name, its description and
#' its label. The descriptions of rule 1 and the shift rule are included, with
#' the shift rule's length as the set gives it.
#'
#' @param x An `autospc_rule_set`.
#' @param ... Ignored, for consistency with the generic.
#'
#' @return `x`, invisibly.
#' @export
print.autospc_rule_set <- function(x, ...) {
  title <- if (is.null(x$label)) "" else paste0(" ", x$label)
  cat(sprintf("<autospc_rule_set>%s\n", title))

  rules <- rule_set_table(x)
  descriptions <- ifelse(
    rules$id == x$shift_id,
    paste(rules$description, "(shift)"),
    rules$description
  )

  cat(paste0(
    "  ",
    format(paste0("rule", rules$id)),
    "  ",
    format(descriptions),
    "  ",
    rules$label,
    "\n"
  ), sep = "")

  return(invisible(x))
}


#' Print a rule
#'
#' @param x An `autospc_rule`.
#' @param ... Ignored, for consistency with the generic.
#'
#' @return `x`, invisibly.
#' @export
print.autospc_rule <- function(x, ...) {
  cat(sprintf("<autospc_rule> %s\n", rule_description(x)))

  return(invisible(x))
}


# Descriptions


#' Describe a zone rule
#'
#' The number of points ("8 in a row" where `k` equals `m`, "2 of 3" otherwise,
#' "1 point" for a single point), the zone ("beyond 2 SD", "within 1 SD",
#' "between 1 and 2 SD", "outside the limits" for beyond 3 SD), and the side
#' ("on one side" or "either side", left out for a single point). A zone that
#' starts at the centre line and has no outer end reads "on one side of the
#' centre line" or "either side of the centre line".
#'
#' @return string
#' @noRd
rule_description.autospc_zone_rule <- function(rule) {
  count <- if (rule$k == 1L && rule$m == 1L) {
    "1 point"
  } else if (rule$k == rule$m) {
    sprintf("%d in a row", rule$k)
  } else {
    sprintf("%d of %d", rule$k, rule$m)
  }

  side <- if (identical(rule$side, "same")) "on one side" else "either side"

  starts_at_centre <- is.null(rule$beyond) || rule$beyond == 0

  if (starts_at_centre && is.null(rule$within)) {
    return(sprintf("%s %s of the centre line", count, side))
  }

  zone <- if (is.null(rule$within) && rule$beyond == 3) {
    "outside the limits"
  } else if (is.null(rule$within)) {
    sprintf("beyond %s SD", format(rule$beyond))
  } else if (starts_at_centre) {
    sprintf("within %s SD", format(rule$within))
  } else {
    sprintf("between %s and %s SD", format(rule$beyond), format(rule$within))
  }

  if (rule$k == 1L && rule$m == 1L) {
    return(sprintf("%s %s", count, zone))
  }

  return(sprintf("%s %s, %s", count, zone, side))
}


#' Describe a trend rule
#'
#' @return string
#' @noRd
rule_description.autospc_trend_rule <- function(rule) {
  return(sprintf("%d increasing or decreasing", rule$k))
}


#' Describe an alternating rule
#'
#' @return string
#' @noRd
rule_description.autospc_alternating_rule <- function(rule) {
  return(sprintf("%d alternating up and down", rule$k))
}


# Constructors and validator


#' Make a rule
#'
#' @param fields A named list of the rule's own arguments.
#' @param kind `"zone"`, `"trend"` or `"alternating"`.
#' @param id The rule's id, or `NULL`.
#' @param label The rule's label, or `NULL`.
#'
#' @return An `autospc_rule`, also of class `autospc_<kind>_rule`.
#' @noRd
new_autospc_rule <- function(fields,
                             kind,
                             id,
                             label) {
  rule <- c(fields, list(kind = kind, id = id, label = label))

  return(structure(
    rule,
    class = c(paste0("autospc_", kind, "_rule"), "autospc_rule")
  ))
}


#' Make a rule set
#'
#' @param rules A list of `autospc_rule`s, each with its id given.
#' @param shift_id The id of the shift rule.
#' @param shift_length The length of the shift rule in the set's source.
#' @param label The name of the set, or `NULL`.
#'
#' @return An `autospc_rule_set`.
#' @noRd
new_autospc_rule_set <- function(rules,
                                 shift_id,
                                 shift_length,
                                 label) {
  return(structure(
    list(
      rules = rules,
      shift_id = shift_id,
      shift_length = shift_length,
      label = label
    ),
    class = "autospc_rule_set"
  ))
}


#' Check that a rule set is valid
#'
#' A valid set's rules each have an id and a label; the ids are whole numbers
#' other than 1 and `shift_id`, with no repeats, in increasing order.
#'
#' @param set An `autospc_rule_set`.
#' @param call The call the error names.
#'
#' @return `set`, unchanged, or an error.
#' @noRd
validate_autospc_rule_set <- function(set,
                                      call = rlang::caller_env()) {
  ids <- vapply(set$rules, function(rule) rule$id, integer(1L))

  if (any(ids %in% c(1L, set$shift_id))) {
    rlang::abort(
      sprintf(
        paste(
          "No rule of a set may have id 1, the rule for a point outside the",
          "limits, or the set's `shift_id`, %d."
        ),
        set$shift_id
      ),
      call = call
    )
  }
  if (anyDuplicated(ids) > 0L) {
    rlang::abort("No two rules of a set may have the same id.", call = call)
  }
  if (is.unsorted(ids)) {
    rlang::abort(
      "The rules of a set must be listed in increasing order of id.",
      call = call
    )
  }

  return(set)
}


# Helpers


#' Check a rule's id
#'
#' @return The id as an integer, or `NULL`.
#' @noRd
match_rule_id <- function(id,
                          call = rlang::caller_env()) {
  if (is.null(id)) {
    return(NULL)
  }

  return(as.integer(match_number(id, "id", kind = "count", call = call)))
}


#' Check a rule's label
#'
#' @return The label, or `NULL`.
#' @noRd
match_rule_label <- function(label,
                             call = rlang::caller_env()) {
  if (is.null(label)) {
    return(NULL)
  }

  return(match_string(label, "label", call = call))
}


#' Give each rule of a set its id and its label
#'
#' Where no rule was given an id, the rules take in order the whole numbers
#' from 2 upwards that are not `shift_id`. A rule with no label takes its
#' description.
#'
#' @param rules A list of `autospc_rule`s.
#' @param shift_id The id of the shift rule.
#' @param call The call the error names.
#'
#' @return `rules`, each with `id` and `label` set.
#' @noRd
with_rule_ids <- function(rules,
                          shift_id,
                          call = rlang::caller_env()) {
  has_id <- vapply(rules, function(rule) !is.null(rule$id), logical(1L))

  if (any(has_id) && !all(has_id)) {
    rlang::abort(
      "Either every rule of a set is given an `id`, or none is.",
      call = call
    )
  }

  if (!any(has_id)) {
    free <- setdiff(seq(2L, length(rules) + 2L), shift_id)
    for (position in seq_along(rules)) {
      rules[[position]]$id <- free[[position]]
    }
  }

  for (position in seq_along(rules)) {
    if (is.null(rules[[position]]$label)) {
      rules[[position]]$label <- rule_description(rules[[position]])
    }
  }

  return(rules)
}


#' Every rule of a set, rule 1 and the shift rule included
#'
#' @param set An `autospc_rule_set`.
#'
#' @return A data frame with one row per rule, in order of id, and the columns
#'   `id`, `kind`, `description` and `label`.
#' @noRd
rule_set_table <- function(set) {
  others <- data.frame(
    id = vapply(set$rules, function(rule) rule$id, integer(1L)),
    kind = vapply(set$rules, function(rule) rule$kind, character(1L)),
    description = vapply(
      set$rules, function(rule) rule_description(rule),
      character(1L)
    ),
    label = vapply(set$rules, function(rule) rule$label, character(1L))
  )

  fixed <- data.frame(
    id = c(1L, set$shift_id),
    kind = c("outside_limits", "shift"),
    description = c(
      "1 point outside the limits",
      sprintf("%d in a row on one side of the centre line", set$shift_length)
    ),
    label = c("Outside limits", "Shift")
  )

  return(
    dplyr::bind_rows(fixed, others) %>%
      dplyr::arrange(id)
  )
}


#' A named rule set
#'
#' @param name One of `autospc_rule_set_names()`.
#' @param call The call the error names.
#'
#' @return An `autospc_rule_set`.
#' @noRd
named_rule_set <- function(name,
                           call = rlang::caller_env()) {
  if (length(name) != 1L || !name %in% autospc_rule_set_names()) {
    rlang::abort(
      sprintf(
        "%s is not the name of a rule set. The named sets are %s.",
        describe_value(name),
        paste0("\"", autospc_rule_set_names(), "\"", collapse = ", ")
      ),
      call = call
    )
  }

  set <- switch(name,
    provost_murray = rule_set(
      trend_rule(k = 6, id = 3, label = "Trend"),
      zone_rule(
        k = 2, m = 3, beyond = 2, side = "either", id = 4,
        label = "Two of three near a limit"
      ),
      zone_rule(
        k = 15, m = 15, within = 1, side = "either", id = 5,
        label = "Fifteen near the centre line"
      ),
      shift_id = 2,
      shift_length = 8,
      label = "Provost and Murray"
    ),
    western_electric = rule_set(
      zone_rule(
        k = 2, m = 3, beyond = 2, side = "same", id = 2,
        label = "Two of three near a limit"
      ),
      zone_rule(
        k = 4, m = 5, beyond = 1, side = "same", id = 3,
        label = "Four of five beyond 1 SD"
      ),
      shift_id = 4,
      shift_length = 8,
      label = "Western Electric"
    ),
    nelson = rule_set(
      trend_rule(k = 6, id = 3, label = "Trend"),
      alternating_rule(k = 14, id = 4, label = "Alternating"),
      zone_rule(
        k = 2, m = 3, beyond = 2, side = "same", id = 5,
        label = "Two of three near a limit"
      ),
      zone_rule(
        k = 4, m = 5, beyond = 1, side = "same", id = 6,
        label = "Four of five beyond 1 SD"
      ),
      zone_rule(
        k = 15, m = 15, within = 1, side = "either", id = 7,
        label = "Fifteen near the centre line"
      ),
      zone_rule(
        k = 8, m = 8, beyond = 1, side = "either", id = 8,
        label = "Eight beyond 1 SD"
      ),
      shift_id = 2,
      shift_length = 9,
      label = "Nelson"
    )
  )

  return(set)
}


#' The names of the named rule sets
#'
#' @return A character vector.
#' @noRd
autospc_rule_set_names <- function() {
  return(c("provost_murray", "western_electric", "nelson"))
}
