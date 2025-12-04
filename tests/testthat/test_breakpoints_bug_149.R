bug_148_data <- readRDS(file.path("testdata", "bug_148_data.rds"))
# Bug 149 captured using same data as bug 148

test_that(paste0("break points are inserted even with no change in centre line",
                 "(bug 149 fixed)"), {
                   
                   result <- autospc(bug_148_data,
                                           chart_type = "C",
                                           plotChart = FALSE)
                   
                   num_breakpoints = sum(result %>%
                                           dplyr::filter(dplyr::row_number()
                                                         != 1L) %>%
                                           dplyr::pull(breakPoint))
                   
                   expect_equal(num_breakpoints,
                                1L)
                   
                   
                 })
