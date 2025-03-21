mnoe_bug_data <- readRDS("~/Rprojects/autospc/tests/testthat/testdata/mnoe_bug_data.rds")
plot_auto_SPC(df = mnoe_bug_data, chartType = "P'", periodMin = 24L, maxNoOfExclusions = 0L)
plot_auto_SPC(df = mnoe_bug_data, chartType = "P'", periodMin = 24L, maxNoOfExclusions = 3L)
plot_auto_SPC(df = mnoe_bug_data, chartType = "P'", periodMin = 24L, maxNoOfExclusions = 0L)

