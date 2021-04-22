# Code to add rule break columns to a dataframe x
# Assumes the following columns in x:
# y   : the measure
# cl  : the centre line
# lcl : the lower control limit
# ucl : the upper control limit

add_rule_breaks <- function(x) {
    x$rule1 <- (x$y > x$ucl) | (x$y < x$lcl)
    x <- rule_two(x)
    x <- add_highlight(x)
    x
}

rule_two <- function(df) {
  
  runs <- rle(ifelse(df$y > df$cl,1,-1))
  rulebreakingruns <- runs$lengths >= 8
  runs$values <- rulebreakingruns
  partofrun <- inverse.rle(runs)
  df$rule2 <- partofrun
  df
  
}

add_highlight <- function(df) {
  df$highlight <- ifelse(df$rule2, "Rule 2", "None")
  df$highlight <- ifelse(df$rule1, "Rule 1", df$highlight)
  df[is.na(df$highlight),'highlight'] <- "none"
  df
}