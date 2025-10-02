# Construct a time series with the following features:
# 1. A stable baseline + some stable extension
# 2. A triggering rule break, candidate limits rejected due to opposing run
#    entirely contained within the candidate period.
# 3. A triggering rule break, with no opposing run or possibility of one through
#    continuation of the final run in the candidate period, hence candidate
#    limits accepted regardless of noRegrets and overhangingReversions
# 4. A triggering rule break against the new limits, no opposing rule break but
#    with the final run (of length e.g. 2) of the candidate period in the
#    opposite direction to the triggering rule break (run becoming of length
#    e.g. 3)
# 5. A triggering rule break against the prevailing limits
#    with the final run of the candidate period rule breaking in the
#    opposite direction to the triggering rule break and overhanging the end of
#    the candidate period

library(autospc)
library(dplyr)
library(tibble)


f_to_df <- function(f,
                    ...,
                    n_start = 1) {
  
  dots_exprs <- rlang::exprs(...)
  y <- do.call(f,
               dots_exprs)
  
  x <- seq_along(y) + n_start - 1
  
  df <- tibble::tibble(x = x,
                       y = y)
  
  return(df)
  
}


f_extend_df <- function(df,
                        f,
                        ...) {
  
  df_x_max <- df %>%
    dplyr::summarise(xmax = max(x,
                                na.rm = TRUE)) %>%
    dplyr::pull(xmax)
  
  df_ext <- f_to_df(f,
                    ...,
                    n_start = df_x_max + 1L)
  
  df_result <- dplyr::bind_rows(df,
                                df_ext)
  
}


generate_example_series_1 <- function() {
  # [1]
  
  set.seed(20251002)
  
  df1 <- f_to_df(rnorm,
                 n = 26L,
                 mean = 10,
                 sd = 1)
  
  # [2]
  
  df2 <- f_extend_df(df1,
                     rnorm,
                     n = 21,
                     mean = 12,
                     sd = 1)
  
  df2 <- df2 %>%
    dplyr::mutate(y = replace(y,
                              c(36:38),
                              y[36:38] - 1))
  
  df2b <- f_extend_df(df2,
                     rnorm,
                     n = 5,
                     mean = 10,
                     sd = 1)
  
  # [3]
  df3 <- f_extend_df(df2b,
                     rnorm,
                     n = 21,
                     mean = 9.25,
                     sd = 1)
  
  # [4]
  
  df4 <- f_extend_df(df3,
                     rnorm,
                     n = 24,
                     mean = 11,
                     sd = 1)
  
  df4 <- df4 %>%
    dplyr::mutate(y = replace(y,
                              c(94:96),
                              y[94:96] - 1))
  
  # [5]
  
  df5 <- f_extend_df(df4,
                     rnorm,
                     n = 28,
                     mean = 9,
                     sd = 1)
  df5 <- df5 %>%
    dplyr::mutate(y = replace(y,
                              c(117:125),
                              y[117:125] +
                                c(-3,1,1,2.5,4,1,1,1,1)),
                  y = replace(y,
                              c(106:110),
                              y[106:110] - 2.5))
  
  df_lengths <- sapply(list(df1,
                            df2,
                            df2b,
                            df3,
                            df4,
                            df5),
                       nrow)
  
  return(list(df = df5,
              timepoints = df_lengths))
  
}

