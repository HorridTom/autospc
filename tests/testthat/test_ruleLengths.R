test_data6 <- readRDS("testdata/test_rulelength_data/test_e2e_data_6.rds")
test_data7 <- readRDS("testdata/test_rulelength_data/test_e2e_data_7.rds")
test_data8 <- readRDS("testdata/test_rulelength_data/test_e2e_data_8.rds")
test_data9 <- readRDS("testdata/test_rulelength_data/test_e2e_data_9.rds")

test_that("check rule length for run of 6", {
  
  results6 <- plot_auto_SPC(test_data6, chartType = "XMR", plotChart = FALSE, runRuleLength = 6)
  results7 <- plot_auto_SPC(test_data6, chartType = "XMR", plotChart = FALSE, runRuleLength = 7)
  results8 <- plot_auto_SPC(test_data6, chartType = "XMR", plotChart = FALSE, runRuleLength = 8)
  results9 <- plot_auto_SPC(test_data6, chartType = "XMR", plotChart = FALSE, runRuleLength = 9)
  resultslist <- unlist(c(results6[22,17], results7[22,17], results8[22,17], results9[22,17]))
  names(resultslist) <- NULL
  
  expect_equal(resultslist, c(FALSE, TRUE, TRUE, TRUE))
})

test_that("check rule length for run of 7", {
  
  results6 <- plot_auto_SPC(test_data7, chartType = "XMR", plotChart = FALSE, runRuleLength = 6)
  results7 <- plot_auto_SPC(test_data7, chartType = "XMR", plotChart = FALSE, runRuleLength = 7)
  results8 <- plot_auto_SPC(test_data7, chartType = "XMR", plotChart = FALSE, runRuleLength = 8)
  results9 <- plot_auto_SPC(test_data7, chartType = "XMR", plotChart = FALSE, runRuleLength = 9)
  resultslist <- unlist(c(results6[22,17], results7[22,17], results8[22,17], results9[22,17]))
  names(resultslist) <- NULL
  
  expect_equal(resultslist, c(FALSE, FALSE, TRUE, TRUE))
})

test_that("check rule length for run of 8", {
  
  results6 <- plot_auto_SPC(test_data8, chartType = "XMR", plotChart = FALSE, runRuleLength = 6)
  results7 <- plot_auto_SPC(test_data8, chartType = "XMR", plotChart = FALSE, runRuleLength = 7)
  results8 <- plot_auto_SPC(test_data8, chartType = "XMR", plotChart = FALSE, runRuleLength = 8)
  results9 <- plot_auto_SPC(test_data8, chartType = "XMR", plotChart = FALSE, runRuleLength = 9)
  resultslist <- unlist(c(results6[22,17], results7[22,17], results8[22,17], results9[22,17]))
  names(resultslist) <- NULL
  
  expect_equal(resultslist, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("rule length change for run of 9", {
  
  results6 <- plot_auto_SPC(test_data9, chartType = "XMR", plotChart = FALSE, runRuleLength = 6)
  results7 <- plot_auto_SPC(test_data9, chartType = "XMR", plotChart = FALSE, runRuleLength = 7)
  results8 <- plot_auto_SPC(test_data9, chartType = "XMR", plotChart = FALSE, runRuleLength = 8)
  results9 <- plot_auto_SPC(test_data9, chartType = "XMR", plotChart = FALSE, runRuleLength = 9)
  resultslist <- unlist(c(results6[22,17], results7[22,17], results8[22,17], results9[22,17]))
  names(resultslist) <- NULL
  
  expect_equal(resultslist, c(FALSE, FALSE, FALSE, FALSE))
})






