# This function creates test dataframes for use in testing argument validation
# for data column arguments to autospc.
make_data_column_validation_data <- function() {
  
  # Data column "types" relevant for autospc. Type "double" is split into two,
  # "double - int" for doubles all of whose elements are whole numbers (up to 
  # machine precision at least), and "double - nonint" for other doubles (i.e.
  # those where at least one element has a non-zero fractional part)
  av_types <- c("null",
                "logical",
                "integer",
                "double - int",
                "double - nonint")
  
  # Variable names appended with `_y` are used in generating data for testing
  # related to chart types that only require the `y` argument, not `n`. 
  df_sigs <- tidyr::expand_grid(y = av_types, n = av_types)
  df_sigs_y <- data.frame(y = av_types)
  
  # Simulate one column of a specified type and length
  sim_av <- function(av_type,
                     num_rows) {
    
    av <- switch(av_type,
                 "null" = NULL,
                 "logical" = sample(x = c(TRUE,
                                          FALSE),
                                    size = num_rows,
                                    replace = TRUE),
                 "integer" = sample.int(100L,
                                        num_rows,
                                        replace = TRUE),
                 "double - int" = as.double(sample.int(100L,
                                                       num_rows,
                                                       replace = TRUE)),
                 "double - nonint" = runif(num_rows,
                                           0,
                                           100))
    
    return(av)
    
  }
  
  # Assemble columns of specified types into a tibble
  make_sig_test_df <- function(y_type,
                               n_type,
                               num_rows) {
    
    y_col <- sim_av(av_type = y_type,
                    num_rows = num_rows)
    
    n_col <- sim_av(av_type = n_type,
                    num_rows = num_rows)
    
    test_df <- tibble::tibble(x = 1L:num_rows,
                              y = y_col,
                              n = n_col)
    
    return(test_df)
    
  }
  
  # Generate test dataframes for all the required combinations of data column
  # types
  set.seed(1234L)
  
  sig_test_dfs <- lapply(c(1:nrow(df_sigs)),
                         function(x) {
                           df <- make_sig_test_df(y_type = df_sigs[[x, "y"]],
                                                  n_type = df_sigs[[x, "n"]],
                                                  num_rows = 30L)
                           
                           return(df)})
  
  sig_test_dfs_y <- lapply(c(1:nrow(df_sigs_y)),
                           function(x) {
                             df <- make_sig_test_df(y_type = df_sigs_y[[x, "y"]],
                                                    n_type = "null",
                                                    num_rows = 30L)
                             
                             return(df)})
  
  # As a starting point for expected results in testing, the remaining code in
  # this function captures the current results of running
  # validate_data_column_spec on the test dataframes. This makes the tests based
  # on this data regression tests, although I manually checked these expected
  # results before saving as fixtures (12/05/2026)
  
  # Factory to build functions that catch the return value and any conditions
  # generated on calling the specified function
  result_catcher_factory <- function(fun)
    function(...) {
      warn <- err <- NULL
      res <- withCallingHandlers(
        tryCatch(fun(...), error=function(e) {
          err <<- conditionMessage(e)
          NULL
        }), warning=function(w) {
          warn <<- append(warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      list(res, warn=warn, err=err)
    }
  
  # Run validate_data_column_spec on each dataframe in df_list, for the
  # specified chart_type, and column bind the results onto results_stub.
  # Returns either just the condition results (no_data = TRUE) or the returned
  # dataframe as well (no_data = FALSE)
  validate_data <- function(df_list,
                            chart_type,
                            results_stub = NULL,
                            no_data = TRUE) {
    
    validation_res <- lapply(df_list,
                             result_catcher_factory(autospc:::validate_data_column_spec),
                             chart_type = chart_type)
    
    validated_data <- lapply(validation_res,
                             function(x) {
                               x[[1]]
                             })
    errs <- sapply(validation_res,
                   function(x) {
                     res <- x[["err"]]
                     ifelse(is.null(res),
                            NA_character_,
                            res)
                   }) %>% unlist()
    warns <- sapply(validation_res,
                    function(x) {
                      res <- x[["warn"]]
                      ifelse(is.null(res),
                             NA_character_,
                             res)
                    }) %>% unlist()
    
    if(is.null(results_stub)) {
      results <- tibble::tibble(err = errs,
                                warn = warns)
    } else {
      results <- results_stub %>% 
        dplyr::bind_cols(err = errs,
                         warn = warns)
    }
    
    if(no_data) {
      return(results)
    } else {
      return(list(results = results,
                  validated_data = validated_data))
    }
  }
  
  # Run validate_data_column_spec on each dataframe in df_list, for all
  # specified chart_types, and column bind results to results_stub
  validate_data_all <- function(df_list,
                                chart_types,
                                results_stub) {
    
    names(chart_types) <- chart_types
    
    val_res <- lapply(chart_types,
                      validate_data,
                      df_list = df_list)
    
    res_df <- val_res %>%
      purrr::imap(\(df, name) dplyr::rename_with(df, \(col) paste(name, col, sep = "_"))) %>%
      dplyr::bind_cols()
    
    
    return(
      results_stub %>%
        dplyr::bind_cols(res_df)
    )
    
  }
  
  # Generate expected conditions for charts using y and n columns
  data_column_validation_expected_conditions_yn <- validate_data_all(
    sig_test_dfs,
    chart_types = c("P", "P'"),
    results_stub = df_sigs)
  
  # Generate expected conditions for charts using only y column
  data_column_validation_expected_conditions_y <- validate_data_all(
    sig_test_dfs_y,
    chart_types = c("XMR", "MR", "C", "C'"),
    results_stub = df_sigs_y)
  
  # Save results
  saveRDS(sig_test_dfs,
          file.path("tests",
                    "testthat",
                    "testdata",
                    "data_column_validation_data_yn.rds"))
  saveRDS(data_column_validation_expected_conditions_yn,
          file.path("tests",
                    "testthat",
                    "testdata",
                    "data_column_validation_expected_conditions_yn.rds"))
  saveRDS(sig_test_dfs_y,
          file.path("tests",
                    "testthat",
                    "testdata",
                    "data_column_validation_data_y.rds"))
  saveRDS(data_column_validation_expected_conditions_y,
          file.path("tests",
                    "testthat",
                    "testdata",
                    "data_column_validation_expected_conditions_y.rds"))
  
}
