# Code to add rule break columns to a dataframe x
# Assumes the following columns in x:
# y   : the measure
# cl  : the centre line
# lcl : the lower control limit
# ucl : the upper control limit

add_rule_breaks <- function(x, rule2Tolerance) {

    x <- x %>%
      dplyr::mutate(rule1 = (y > ucl) | (y < lcl)) %>%
      dplyr::mutate(aboveOrBelowCl = dplyr::case_when(isTRUE(all.equal(y, cl, tolerance = rule2Tolerance)) ~ 0,
                                                      y > cl ~ 1,
                                                      y < cl ~ -1)) %>%
      rule_two() %>%
      dplyr::mutate(rule2 = dplyr::if_else(rule2 & aboveOrBelowCl == 0, FALSE, rule2)) %>%
      add_highlight()

}

rule_two <- function(df) {

  runs <- rle(unlist(df$aboveOrBelowCl))
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

