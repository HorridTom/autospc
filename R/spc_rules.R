# Code to add rule break columns to a dataframe x
# Assumes the following columns in x:
# y   : the measure
# cl  : the centre line
# lcl : the lower control limit
# ucl : the upper control limit

add_rule_breaks <- function(x,
                            rule2Tolerance,
                            runRuleLength) {

    x <- x %>%
      dplyr::mutate(rule1 = (y > ucl) | (y < lcl)) %>%
      dplyr::mutate(aboveOrBelowCl = dplyr::case_when(abs(y - cl) %<=% rule2Tolerance ~ 0L,
                                                      (y - cl) %>>% rule2Tolerance ~ 1L,
                                                      (y - cl) %<<% -rule2Tolerance ~ -1L)) %>%
      rule_two(runRuleLength = runRuleLength) %>%
      dplyr::mutate(rule2 = dplyr::if_else(rule2 & aboveOrBelowCl == 0L, FALSE, rule2)) %>%
      add_highlight() %>%
      dplyr::relocate(aboveOrBelowCl, .after = rule2)

}

rule_two <- function(df, runRuleLength) {

  runs <- rle(unlist(df$aboveOrBelowCl))
  rulebreakingruns <- runs$lengths >= runRuleLength
  runs$values <- rulebreakingruns
  partofrun <- inverse.rle(runs)
  df$rule2 <- partofrun
  df <- df %>% dplyr::mutate(runStart =
                               (dplyr::row_number() %in% cumsum(c(1,
                                                                  runs$lengths))
                                )
                             )
  df
  
}

add_highlight <- function(df) {

  df <- df %>%
    dplyr::mutate(highlight = dplyr::case_when(rule2 ~ "Rule 2",
                                                rule1 ~ "Rule 1",
                                                TRUE ~ "None"))
  
}

