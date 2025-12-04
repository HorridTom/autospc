test_data6 <- readRDS("testdata/test_rulelength_data/test_e2e_data_6.rds")
test_data7 <- readRDS("testdata/test_rulelength_data/test_e2e_data_7.rds")
test_data8 <- readRDS("testdata/test_rulelength_data/test_e2e_data_8.rds")
test_data9 <- readRDS("testdata/test_rulelength_data/test_e2e_data_9.rds")

# these tests use the "limitChange" column to check whether limits are 
# recalculated after 21 points, or if they become display limits, 
# based on different runRuleLengths
test_that("check rule length for run of 6", {
  
  results6 <- autospc(
    test_data6, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 6, 
    periodMin = 21
    )
  results7 <- autospc(
    test_data6, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 7, 
    periodMin = 21
    )
  results8 <- autospc(
    test_data6, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 8, 
    periodMin = 21
    )
  results9 <- autospc(
    test_data6, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 9, 
    periodMin = 21
    )
  resultslist <- unlist(c(results6[22,"limitChange"], 
                          results7[22,"limitChange"], 
                          results8[22,"limitChange"], 
                          results9[22,"limitChange"]
                          ))
  names(resultslist) <- NULL 
  #removes column names so the comparison can be carried out
  
  expect_equal(resultslist, !c(TRUE, FALSE, FALSE, FALSE))
})

#Tests are repeated for data sets that include increasing run lengths
test_that("check rule length for run of 7", {
  
  results6 <- autospc(
    test_data7, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 6, 
    periodMin = 21
    )
  results7 <- autospc(
    test_data7, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 7, 
    periodMin = 21
    )
  results8 <- autospc(
    test_data7, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 8, 
    periodMin = 21
    )
  results9 <- autospc(
    test_data7, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 9, 
    periodMin = 21
    )
  resultslist <- unlist(c(results6[22,"limitChange"], 
                          results7[22,"limitChange"], 
                          results8[22,"limitChange"], 
                          results9[22,"limitChange"]
                          ))
  names(resultslist) <- NULL
  expect_equal(resultslist, !c(TRUE, TRUE, FALSE, FALSE))
})

test_that("check rule length for run of 8", {
  
  results6 <- autospc(
    test_data8, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 6, 
    periodMin = 21
    )
  results7 <- autospc(
    test_data8, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 7, 
    periodMin = 21
    )
  results8 <- autospc(
    test_data8, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 8, 
    periodMin = 21
    )
  results9 <- autospc(
    test_data8, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 9, 
    periodMin = 21
    )
  resultslist <- unlist(c(results6[22,"limitChange"], 
                          results7[22,"limitChange"], 
                          results8[22,"limitChange"], 
                          results9[22,"limitChange"]
                          ))
  names(resultslist) <- NULL
  expect_equal(resultslist, !c(TRUE, TRUE, TRUE, FALSE))
})

test_that("rule length change for run of 9", {
  
  results6 <- autospc(
    test_data9, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 6, 
    periodMin = 21
    )
  results7 <- autospc(
    test_data9, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 7, 
    periodMin = 21
    )
  results8 <- autospc(
    test_data9, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 8, 
    periodMin = 21
    )
  results9 <- autospc(
    test_data9, 
    chart_type = "XMR", 
    plotChart = FALSE, 
    runRuleLength = 9, 
    periodMin = 21
    )
  resultslist <- unlist(c(results6[22,"limitChange"], 
                          results7[22,"limitChange"], 
                          results8[22,"limitChange"], 
                          results9[22,"limitChange"]
                          ))
  names(resultslist) <- NULL
  expect_equal(resultslist, !c(TRUE, TRUE, TRUE, TRUE))
})






