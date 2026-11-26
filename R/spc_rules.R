#' Add the rule break columns
#'
#' `rule1` (the point is outside the limits), `rule2` (the point is in a run of
#' at least `shift_rule_threshold` points on one side of the centre line),
#' `highlight` (the rule whose colour the point is drawn in, with rule 2 taking
#' precedence over rule 1), and the columns `add_shift_columns()` adds, apart
#' from `shift`, which `rule2` replaces.
#'
#' @param x A table with the columns `series`, `cl`, `lcl` and `ucl`.
#' @param centre_line_tolerance As for `add_shift_columns()`.
#' @param shift_rule_threshold As for `add_shift_columns()`.
#'
#' @return `x`, with the columns added.
#' @noRd
add_rule_breaks <- function(x,
                            centre_line_tolerance,
                            shift_rule_threshold) {
  x <- add_shift_columns(x,
    centre_line_tolerance = centre_line_tolerance,
    shift_rule_threshold = shift_rule_threshold
  )

  # rule 1: the point is outside the limits
  rule1 <- (x$series > x$ucl) | (x$series < x$lcl)
  rule2 <- x$shift

  x[["rule1"]] <- rule1
  x[["rule2"]] <- rule2
  x[["shift"]] <- NULL

  # rule 2 takes precedence over rule 1 in the highlight
  highlight <- rep("None", length(rule1))
  highlight[which(rule1)] <- "Rule 1"
  highlight[which(rule2)] <- "Rule 2"
  x[["highlight"]] <- highlight

  return(x)
}


#' Add the rule break columns to each period of a table separately
#'
#' A run does not continue from one period into the next. The periods are
#' separated at the rows where `break_point` is TRUE.
#'
#' @param limits_table The table the algorithm formed, with a `break_point`
#'   column.
#' @param chart The chart being analysed.
#'
#' @return `limits_table`, with the columns `add_rule_breaks()` adds.
#' @noRd
add_rule_breaks_by_period <- function(limits_table,
                                      chart) {
  if (nrow(limits_table) == 0L) {
    return(add_rule_breaks(limits_table,
      centre_line_tolerance = chart$centre_line_tolerance,
      shift_rule_threshold = chart$shift_rule_threshold
    ))
  }

  period <- cumsum(dplyr::coalesce(limits_table$break_point, FALSE))

  tables <- lapply(
    split(seq_len(nrow(limits_table)), period),
    function(rows) {
      add_rule_breaks(dplyr::slice(limits_table, rows),
        centre_line_tolerance = chart$centre_line_tolerance,
        shift_rule_threshold = chart$shift_rule_threshold
      )
    }
  )

  return(dplyr::bind_rows(tables))
}


#' Add the columns the algorithm uses to find shifts
#'
#' `above_or_below_cl` (which side of the centre line each point is),
#' `run_start` (whether the point is the first of a run on one side of the
#' centre line) and `shift` (whether the point is in a run of at least
#' `shift_rule_threshold` points on one side of the centre line).
#'
#' @param x A table with the columns `series` and `cl`, and optionally
#'   `run_break`.
#' @param centre_line_tolerance The distance from the centre line within which
#'   a point is taken to be on it.
#' @param shift_rule_threshold The number of points a run needs to break the
#'   shift rule.
#'
#' @return `x`, with the columns added.
#' @noRd
add_shift_columns <- function(x,
                              centre_line_tolerance,
                              shift_rule_threshold) {
  # which side of the centre line each point is: 1 above, -1 below, 0 on it
  # (within centre_line_tolerance), NA where it cannot be compared
  difference <- x$series - x$cl
  side <- rep(NA_integer_, length(difference))
  side[which(difference %<<% -centre_line_tolerance)] <- -1L
  side[which(difference %>>% centre_line_tolerance)] <- 1L
  side[which(abs(difference) %<=% centre_line_tolerance)] <- 0L
  x[["above_or_below_cl"]] <- side

  x <- add_run_columns(x, shift_rule_threshold = shift_rule_threshold)

  # a point on the centre line is not part of a shift
  shift <- x$shift
  on_cl <- shift & side == 0L
  shift[which(on_cl)] <- FALSE
  shift[is.na(on_cl)] <- NA
  x[["shift"]] <- shift

  return(x)
}


#' Add `run_start` and the run length test
#'
#' @param table A table with the column `above_or_below_cl`, and optionally
#'   `run_break`.
#' @param shift_rule_threshold The number of points a run needs to break the
#'   shift rule.
#'
#' @return `table`, with `run_start` and `shift` added. `shift` is TRUE for
#'   every point of a run of at least `shift_rule_threshold` points.
#' @noRd
add_run_columns <- function(table, shift_rule_threshold) {
  # which side of the centre line each point is: 1 above, -1 below, 0 on it
  # (within centre_line_tolerance), NA where there is no point or no centre
  # line to compare it with
  side <- unlist(table$above_or_below_cl)

  if (length(side) == 0L) {
    table$shift <- logical(0)
    table$run_start <- logical(0)

    return(table)
  }

  # Whether each point is above or below the centre line. A point on the centre
  # line neither commences a run nor counts towards the length of one, and a
  # point with no side is not part of one either
  counts <- !is.na(side) & side != 0L

  # a run does not continue into a point with no side, nor into the point after
  # a gap where na_ends_pattern asked for that
  barrier <- is.na(side)

  if ("run_break" %in% names(table)) {
    barrier <- barrier | table$run_break
  }

  # number every run, so that every point carries the number of the run it
  # belongs to, and the points before the first run of the table carry 0
  table$run_start <- commences_a_run(
    side = side,
    counts = counts,
    barrier = barrier
  )

  run <- cumsum(table$run_start)

  run_lengths <- tabulate(run[counts], nbins = max(run))

  length_of_run <- rep(0L, length(side))
  in_a_run <- run > 0L
  length_of_run[in_a_run] <- run_lengths[run[in_a_run]]

  table$shift <- length_of_run >= shift_rule_threshold

  table
}


#' Which points commence a run
#'
#' A run commences at a point above or below the centre line that does not
#' continue the run before it, either because the last such point was on the
#' other side of the centre line or because a barrier lies between the two.
#' Points on the centre line, and points with no side, never commence one.
#'
#' @param side 1 above the centre line, -1 below it, 0 on it, NA for a point
#'   with no side.
#' @param counts Whether each point counts towards the length of a run.
#' @param barrier Whether a run cannot continue into each point.
#'
#' @return logical vector, one value per point
#' @noRd
commences_a_run <- function(side,
                            counts,
                            barrier) {
  commences <- rep(FALSE, length(side))
  counting <- which(counts)

  if (length(counting) == 0L) {
    return(commences)
  }

  # the counting point before each counting point, and how many barriers lie
  # between the two
  previous <- c(NA_integer_, counting[-length(counting)])
  barriers_so_far <- cumsum(barrier)

  blocked <- is.na(previous) |
    barriers_so_far[counting] > barriers_so_far[previous]

  commences[counting] <- !(!blocked & side[counting] == side[previous])

  return(commences)
}
