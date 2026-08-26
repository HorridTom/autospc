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
      above_or_below_cl = dplyr::case_when(abs(y - cl) %<=%
                                             centre_line_tolerance ~ 0L,
                                           (y - cl) %>>%
                                             centre_line_tolerance ~ 1L,
                                           (y - cl) %<<%
                                             -centre_line_tolerance ~ -1L)) %>%
    add_rule_two(shift_rule_threshold = shift_rule_threshold) %>%
    dplyr::mutate(rule2 = dplyr::if_else(rule2 & above_or_below_cl == 0L,
                                         FALSE,
                                         rule2)) %>%
    add_highlight() %>%
    dplyr::relocate(above_or_below_cl, .after = rule2)
  
}

add_rule_two <- function(table, shift_rule_threshold) {
  
  runs <- rle(unlist(table$above_or_below_cl))
  rulebreakingruns <- runs$lengths >= shift_rule_threshold
  runs$values <- rulebreakingruns
  partofrun <- inverse.rle(runs)
  table$rule2 <- partofrun
  table <- table %>% dplyr::mutate(run_start =
                               (dplyr::row_number() %in% cumsum(c(1,
                                                                  runs$lengths))
                               )
  )
  table
  
}

add_highlight <- function(table) {
  
  table <- table %>%
    dplyr::mutate(highlight = dplyr::case_when(rule2 ~ "Rule 2",
                                               rule1 ~ "Rule 1",
                                               TRUE ~ "None"))
  
}

