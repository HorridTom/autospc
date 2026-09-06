# Code to add rule break columns to a dataframe x
# Assumes the following columns in x:
# y   : the measure
# cl  : the centre line
# lcl : the lower control limit
# ucl : the upper control limit

add_rule_breaks <- function(x,
                            centre_line_tolerance,
                            shift_rule_threshold) {
  x <- x %>%
    dplyr::mutate(rule1 = (y > ucl) | (y < lcl)) %>%
    dplyr::mutate(
      above_or_below_cl = dplyr::case_when(
        abs(y - cl) %<=%
          centre_line_tolerance ~ 0L,
        (y - cl) %>>%
          centre_line_tolerance ~ 1L,
        (y - cl) %<<%
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

  # the side of the point before each one, NA for the first
  previous_side <- dplyr::lag(side)

  # a point continues the run before it when it is on the same side.
  # FOR NOW: A point on the centre line is a side of its own, so it ends the run
  # it interrupts and starts one of its own.
  # TO DO: Fix this so that points on the centre line do not end a run, and do
  # not contribute to run length.
  # A missing side continues nothing. Note that missing values in the analysed
  # series have been removed by this point, and how their presence impacts run
  # continuation is dictated by na_ends_run, through table$run_break.
  continues <- !is.na(side) &
    !is.na(previous_side) &
    side == previous_side

  # a gap ends the run before it, where na_ends_run asked for that.
  if ("run_break" %in% names(table)) {
    continues <- continues & !table$run_break
  }

  # number every run, so that the first point of each is where a run does not
  # continue, and every point of a run carries that run's number
  run_start <- !continues
  run <- cumsum(run_start)

  # how many points each run holds, indexed by run number
  run_lengths <- tabulate(run)

  table$rule2 <- run_lengths[run] >= shift_rule_threshold
  table$run_start <- run_start

  table
}

add_highlight <- function(table) {
  table <- table %>%
    dplyr::mutate(highlight = dplyr::case_when(
      rule2 ~ "Rule 2",
      rule1 ~ "Rule 1",
      TRUE ~ "None"
    ))
}
