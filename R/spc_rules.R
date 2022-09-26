# Code to add rule break columns to a dataframe x
# Assumes the following columns in x:
# y   : the measure
# cl  : the centre line
# lcl : the lower control limit
# ucl : the upper control limit

add_rule_breaks <- function(x) {

    x <- x %>%
      dplyr::mutate(rule1 = (y > ucl) | (y < lcl)) %>%
      rule_two() %>%
      dplyr::mutate(aboveOrBelowCl = ifelse(y > cl, 1, ifelse(y < cl, -1, 0))) %>%
      dplyr::mutate(rule2 = dplyr::if_else(rule2 & aboveOrBelowCl == 0, FALSE, rule2)) %>%
      add_highlight()

}

rule_two <- function(df) {

  runs <- rle(ifelse(df$y == df$cl,0,
                     ifelse(df$y > df$cl, 1,-1)))
  rulebreakingruns <- runs$lengths >= 8
  runs$values <- rulebreakingruns
  partofrun <- inverse.rle(runs)
  df$rule2 <- partofrun
  df
  
}

add_highlight <- function(df) {

  df <- df %>%
    dplyr::mutate(highlight = dplyr::case_when(rule2 ~ "Rule 2",
                                                rule1 ~ "Rule 1",
                                                TRUE ~ "None"))
  
}

