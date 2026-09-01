# Run the test suite without its slowest files, for the edit and re-run loop.
# The full suite is what counts before a commit; this is for the minute to
# minute work.
#
#   source("dev/fast_tests.R")
#   fast_tests()
#
# The files named below took more than 1.5 seconds each when measured on
# 2026-09-01: 22 of the 66, and 84 per cent of the suite's 130 seconds. What is
# left runs in about 20. A test file added later is run unless it is named here,
# so a new file is never silently left out.
#
# Re-measure with measure_test_times().

slow_test_files <- c(
  "facet_stages",                    # 15.8s
  "ssa_variants",                    # 10.6s
  "autospc_returns_plot",            # 9.7s
  "xmr_pair",                        # 8.1s
  "median_plot",                     # 6.6s
  "x_break",                         # 6.2s
  "basic_annotations",               # 6.0s
  "chart_arguments",                 # 5.4s
  "ruleLengths",                     # 4.9s
  "establish_limits",                # 4.9s
  "show_mr_deprecated",              # 4.4s
  "no_regrets",                      # 4.3s
  "history",                         # 2.7s
  "log_file",                        # 2.6s
  "shift_separation",                # 2.5s
  "return_class",                    # 2.4s
  "print_autospc_chart",             # 2.3s
  "regressions",                     # 2.2s
  "pass_column_names",               # 2.1s
  "column_types",                    # 2.0s
  "identify_opposite_break",         # 1.8s
  "upper_annotation_sf_default"      # 1.7s
)


# Run every test file except the slow ones.
fast_tests <- function(path = ".", ...) {
  files <- list.files(
    file.path(path, "tests", "testthat"),
    pattern = "^test-.*\\.[rR]$"
  )

  subjects <- sub("\\.[rR]$", "", sub("^test-", "", files))
  wanted <- setdiff(subjects, slow_test_files)

  if (length(wanted) == 0L) {
    stop("Every test file is named in slow_test_files.", call. = FALSE)
  }

  return(devtools::test(
    path,
    filter = paste0("^(", paste(wanted, collapse = "|"), ")$"),
    ...
  ))
}


# How long each test file takes, slowest first.
measure_test_times <- function(path = ".") {
  results <- as.data.frame(testthat::test_local(path, reporter = "silent"))

  return(sort(tapply(results$real, results$file, sum), decreasing = TRUE))
}
