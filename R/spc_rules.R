# Code to add rule break columns to a dataframe x
# Assumes the following columns in x:
# series : the values under analysis
# cl     : the centre line
# lcl    : the lower control limit
# ucl    : the upper control limit

add_rule_breaks <- function(x,
                            centre_line_tolerance,
                            shift_rule_threshold) {
  x <- x %>%
    dplyr::mutate(rule1 = (series > ucl) | (series < lcl)) %>%
    dplyr::mutate(
      above_or_below_cl = dplyr::case_when(
        abs(series - cl) %<=%
          centre_line_tolerance ~ 0L,
        (series - cl) %>>%
          centre_line_tolerance ~ 1L,
        (series - cl) %<<%
          -centre_line_tolerance ~ -1L
      )
    ) %>%
    add_rule_two(shift_rule_threshold = shift_rule_threshold) %>%
    dplyr::mutate(rule2 = dplyr::if_else(rule2 & above_or_below_cl == 0L,
      FALSE,
      rule2
    )) %>%
    add_highlight() %>%
    dplyr::relocate(above_or_below_cl, .after = rule2)
}

add_rule_two <- function(table, shift_rule_threshold) {
  # which side of the centre line each point is: 1 above, -1 below, 0 on it
  # (within centre_line_tolerance), NA where there is no point or no centre
  # line to compare it with
  side <- unlist(table$above_or_below_cl)

  if (length(side) == 0L) {
    table$rule2 <- logical(0)
    table$run_start <- logical(0)

    return(table)
  }

  # Whether each point is above or below the centre line. A point on the centre
  # line neither commences a run nor counts towards the length of one, and a
  # point with no side is not part of one either
  counts <- !is.na(side) & side != 0L

  # a run does not continue into a point with no side, nor into the point after
  # a gap where na_ends_run asked for that
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

  table$rule2 <- length_of_run >= shift_rule_threshold

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

add_highlight <- function(table) {
  table <- table %>%
    dplyr::mutate(highlight = dplyr::case_when(
      rule2 ~ "Rule 2",
      rule1 ~ "Rule 1",
      TRUE ~ "None"
    ))
}
