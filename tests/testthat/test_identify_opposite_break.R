#dataset with second rule break pulling the mean down from triggering rule break
test_op_break1_data <- structure(list(x = 1:46, y = c(11L, 13L, 
                                                      13L, 13L, 12L, 6L, 8L, 10L, 11L, 15L, 14L, 10L, 9L, 8L, 
                                                      8L, 12L, 6L, 9L, 14L, 9L, 14L, 15L, 15L, 14L, 14L, 14L, 
                                                      16L, 15L, 15L, 14L,  14L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 
                                                      2L, 1L, 1L, 1L, 1L, 1L, 0L, 0L
                                                      )), 
                                 class = "data.frame", row.names = c(NA, -46L))

#dataset with second rule break pulling the mean up from triggering rule break
test_op_break2_data <- structure(list(x = 1:46, y = c(9L, 8L, 14L, 13L, 11L, 13L, 7L, 
                                                      12L, 3L, 14L, 11L, 6L, 11L, 5L, 14L, 7L, 12L, 8L, 11L, 9L, 8L, 
                                                      7L, 7L, 7L, 7L, 6L, 6L, 6L, 6L, 8L, 7L, 15L, 15L, 14L, 15L, 16L, 
                                                      13L, 14L, 16L, 15L, 15L, 15L, 14L, 14L, 15L, 14L)), 
                                 row.names = c(NA, -46L), class = "data.frame")

#dataset with triggering and second rule break equidistant from mean
test_op_break3_data <- structure(list(x = 1:46, y = c(10L, 14L, 5L, 13L, 11L, 6L, 9L, 
                                                      11L, 10L, 12L, 7L, 14L, 12L, 14L, 7L, 11L, 5L, 9L, 8L, 10L, 11L, 
                                                      4L, 5L, 6L, 4L, 5L, 3L, 4L, 4L, 4L, 5L, 15L, 15L, 16L, 14L, 15L, 
                                                      15L, 14L, 14L, 14L, 16L, 12L, 16L, 15L, 13L, 15L)), 
                                 row.names = c(NA, -46L), class = "data.frame")

#normal rule break with expected recalculation
test_op_break4_data <- structure(list(x = 1:46, y = c(8L, 8L, 13L, 5L, 12L, 13L, 10L, 
                                                      8L, 6L, 12L, 11L, 4L, 8L, 9L, 7L, 10L, 12L, 5L, 11L, 9L, 11L, 
                                                      16L, 14L, 14L, 15L, 14L, 15L, 14L, 15L, 15L, 13L, 13L, 16L, 15L, 
                                                      15L, 12L, 15L, 14L, 15L, 14L, 16L, 17L, 13L, 15L, 14L, 14L)), 
                                 row.names = c(NA, -46L), class = "data.frame")

#rule break with further rule break in direction of triggering rule break - expected recalc
test_op_break5_data <- structure(list(x = 1:46, y = c(8L, 8L, 13L, 5L, 12L, 13L, 10L, 
                                                      8L, 6L, 12L, 11L, 4L, 8L, 9L, 7L, 10L, 12L, 5L, 11L, 9L, 11L, 
                                                      16L, 14L, 14L, 15L, 14L, 15L, 14L, 15L, 15L, 13L, 13L, 16L, 15L, 
                                                      15L, 12L, 15L, 14L, 15L, 14L, 16L, 17L, 13L, 15L, 14L, 14L)), 
                                 row.names = c(NA, -46L), class = "data.frame")

#opposite rule break commencing after the end of the candidate calculation period
test_op_break6_data <- readRDS("testdata/test_oppositeRuleBreak_later.rds")

test_that("Rule 2 break within candidate period in opposite direction identified correctly",{
  
  #should not recalc due to break in op direction
  test_op_break1 <- autospc(test_op_break1_data,
                                  chart_type = "C'",
                                  plotChart = F)
  test_op_break1_break_pos <- which(test_op_break1$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break1_break_pos, integer(0))
  
  #should not recalc due to break in op direction
  test_op_break2 <- autospc(test_op_break2_data, chart_type = "C'", plotChart = F)
  test_op_break2_break_pos <- which(test_op_break2$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break2_break_pos, integer(0))
  
  #should not recalc due to break in op direction
  test_op_break3 <- autospc(test_op_break3_data, chart_type = "C'", plotChart = F)
  test_op_break3_break_pos <- which(test_op_break3$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break3_break_pos, integer(0))
  
  #should recalc
  test_op_break4 <- autospc(test_op_break4_data, chart_type = "C'", plotChart = F)
  test_op_break4_break_pos <- which(test_op_break4$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break4_break_pos, 22)
  
  #should recalc
  test_op_break5 <- autospc(test_op_break5_data, chart_type = "C'", plotChart = F)
  test_op_break5_break_pos <- which(test_op_break5$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break5_break_pos, 22)
  
})


test_that("Opposite rule break after candidate calc period doesn't prevent recalculation",{
  #should recalc
  test_op_break6 <- autospc(test_op_break6_data,
                                  no_regrets = TRUE,
                                  chart_type = "C'",
                                  plotChart = FALSE)
  
  test_op_break6_break_pos <- which(test_op_break6$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break6_break_pos, 31L)
})
