# log_file_path writes one file per call, holding every chart the call analysed.
# chart says which each entry came from: the chart type for the two halves of an
# XmR pair, and the stage for a faceted chart.

log_file_data <- data.frame(
  x = 1:60,
  y = c(
    rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L),
    rep(c(30L, 32L, 31L, 33L, 29L, 34L), 5L)
  )
)


written_log <- function(...) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  suppressWarnings(
    autospc(log_file_data,
      period_min = 21L, plot_chart = FALSE,
      log_file_path = path, ...
    )
  )

  readRDS(path)
}


test_that("a single chart writes its log to the file", {
  log_table <- written_log(chart_type = "C")

  expect_s3_class(log_table, "data.frame")

  expect_identical(unique(log_table$chart), "C")
})


test_that("an XmR pair writes both halves to one file", {
  log_table <- written_log(chart_type = "XMR")

  expect_identical(unique(log_table$chart), c("X", "MR"))
})


test_that("a pair with too few points for limits writes both halves too", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  suppressWarnings(
    autospc(log_file_data[1:10, ],
      period_min = 21L, chart_type = "XMR",
      plot_chart = FALSE, log_file_path = path
    )
  )

  expect_identical(unique(readRDS(path)$chart), c("X", "MR"))
})


test_that("the X chart's log is not written over by the moving range chart's", {
  pair <- written_log(chart_type = "XMR")

  alone <- written_log(chart_type = "X")

  expect_identical(
    pair[pair$chart == "X", setdiff(names(pair), "chart")],
    alone[, setdiff(names(alone), "chart")]
  )
})


test_that("a faceted chart writes one entry per stage", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  suppressWarnings(
    facet_stages(log_file_data,
      split_rows = c(20L, 40L, 60L),
      chart_type = "C", period_min = 21L, plot_chart = FALSE,
      log_file_path = path
    )
  )

  log_table <- readRDS(path)

  expect_identical(unique(log_table$chart), c("1", "2", "3"))
})


test_that("named split_rows name the stages in the log", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  suppressWarnings(
    facet_stages(log_file_data,
      split_rows = c(first = 20L, second = 40L, third = 60L),
      chart_type = "C", period_min = 21L, plot_chart = FALSE,
      log_file_path = path
    )
  )

  log_table <- readRDS(path)

  expect_identical(unique(log_table$chart), c("first", "second", "third"))
})


test_that("a csv log file is written too", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  suppressWarnings(
    autospc(log_file_data,
      chart_type = "XMR", period_min = 21L,
      plot_chart = FALSE, log_file_path = path
    )
  )

  log_table <- utils::read.csv(path)

  expect_setequal(unique(log_table$chart), c("X", "MR"))
})


test_that("an unusable extension warns and writes nothing", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)

  warnings_given <- character()

  withCallingHandlers(
    autospc(log_file_data,
      chart_type = "C", period_min = 21L,
      plot_chart = FALSE, log_file_path = path
    ),
    warning = function(w) {
      warnings_given <<- c(warnings_given, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Invalid extension", warnings_given, fixed = TRUE)))

  expect_false(file.exists(path))
})


test_that("no file is written when no path is given", {
  before <- list.files(recursive = TRUE)

  suppressWarnings(
    autospc(log_file_data,
      chart_type = "C", period_min = 21L,
      plot_chart = FALSE
    )
  )

  expect_identical(list.files(recursive = TRUE), before)
})
