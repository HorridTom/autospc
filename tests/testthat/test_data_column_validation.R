data_column_validation_data_yn <- readRDS(
  file.path("testdata",
            "data_column_validation_data_yn.rds"))

data_column_validation_expected_conditions_yn <- readRDS(
  file.path("testdata",
            "data_column_validation_expected_conditions_yn.rds"))

data_column_validation_data_y <- readRDS(
  file.path("testdata",
            "data_column_validation_data_y.rds"))

data_column_validation_expected_conditions_y <- readRDS(
  file.path("testdata",
            "data_column_validation_expected_conditions_y.rds"))

chart_types_yn <- c("P", "P'")
chart_types_y <- c("XMR", "MR", "C", "C'")

for (chart_type in chart_types_yn) {
  for (i in seq_along(data_column_validation_data_yn)) {
    test_that(paste("validate_data_column_spec: chart_type =", chart_type, "| case i =", i), {
      df            <- data_column_validation_data_yn[[i]]
      expected_err  <- data_column_validation_expected_conditions_yn[[paste0(chart_type, "_err")]][i]
      expected_warn <- data_column_validation_expected_conditions_yn[[paste0(chart_type, "_warn")]][i]
      
      expect_error_val   <- !is.na(expected_err)
      expect_warning_val <- !is.na(expected_warn)
      
      if (expect_error_val && expect_warning_val) {
        expect_error(
          expect_warning(
            validate_data_column_spec(df, chart_type = chart_type),
            regexp = expected_warn,
            fixed  = TRUE
          ),
          regexp = expected_err,
          fixed  = TRUE
        )
        
      } else if (expect_error_val) {
        expect_error(
          validate_data_column_spec(df, chart_type = chart_type),
          regexp = expected_err,
          fixed  = TRUE
        )
        
      } else if (expect_warning_val) {
        warned <- FALSE
        result <- withCallingHandlers(
          validate_data_column_spec(df, chart_type = chart_type),
          warning = function(w) {
            if (grepl(expected_warn, conditionMessage(w), fixed = TRUE)) {
              warned <<- TRUE
            }
            invokeRestart("muffleWarning")
          }
        )
        expect_true(warned, label = paste("Expected warning not found:", expected_warn))
        expect_s3_class(result, "data.frame")
        
      } else {
        result <- expect_no_error(
          expect_no_warning(
            validate_data_column_spec(df, chart_type = chart_type)
          )
        )
        expect_s3_class(result, "data.frame")
      }
    })
  }
}


for (chart_type in chart_types_y) {
  for (i in seq_along(data_column_validation_data_y)) {
    test_that(paste("validate_data_column_spec: chart_type =", chart_type, "| case i =", i), {
      df            <- data_column_validation_data_y[[i]]
      expected_err  <- data_column_validation_expected_conditions_y[[paste0(chart_type, "_err")]][i]
      expected_warn <- data_column_validation_expected_conditions_y[[paste0(chart_type, "_warn")]][i]
      
      expect_error_val   <- !is.na(expected_err)
      expect_warning_val <- !is.na(expected_warn)
      
      if (expect_error_val && expect_warning_val) {
        expect_error(
          expect_warning(
            validate_data_column_spec(df, chart_type = chart_type),
            regexp = expected_warn,
            fixed  = TRUE
          ),
          regexp = expected_err,
          fixed  = TRUE
        )
        
      } else if (expect_error_val) {
        expect_error(
          validate_data_column_spec(df, chart_type = chart_type),
          regexp = expected_err,
          fixed  = TRUE
        )
        
      } else if (expect_warning_val) {
        warned <- FALSE
        result <- withCallingHandlers(
          validate_data_column_spec(df, chart_type = chart_type),
          warning = function(w) {
            if (grepl(expected_warn, conditionMessage(w), fixed = TRUE)) {
              warned <<- TRUE
            }
            invokeRestart("muffleWarning")
          }
        )
        expect_true(warned, label = paste("Expected warning not found:", expected_warn))
        expect_s3_class(result, "data.frame")
        
      } else {
        result <- expect_no_error(
          expect_no_warning(
            validate_data_column_spec(df, chart_type = chart_type)
          )
        )
        expect_s3_class(result, "data.frame")
      }
    })
  }
}
