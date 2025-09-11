# Functions to populate and interpret algorithm log

record_log_entry <- function(df,
                             counter,
                             entry){
  
  if(!("log" %in% colnames(df))){
    df <- df %>%
      dplyr::mutate(log = NA_character_)
  }
  
  counter_overflow <- FALSE
  if(counter > nrow(df)) {
    counter_arg <- counter
    counter <- nrow(df)
    counter_overflow <- TRUE
  }
  
  existing_log_entry <- df %>%
    dplyr::filter(dplyr::row_number() == counter) %>%
    dplyr::pull(log)
  
  if(is.na(existing_log_entry)) {
    if(counter_overflow) {
      entry <- paste0("co@",
                      counter_arg,
                      "|",
                      entry)
    }
    updated_log_entry <- as.character(entry)
  } else {
    if(counter_overflow) {
      existing_log_entry <- paste0(existing_log_entry,
                                   "co@",
                                   counter_arg,
                                   "|")
    }
    updated_log_entry <- paste(existing_log_entry,
                               as.character(entry),
                               sep = ";")
  }
  
  df <- df %>%
    #dplyr::rowwise() %>%
    dplyr::mutate(log = dplyr::if_else(dplyr::row_number() == counter,
                                       updated_log_entry,
                                       log)) #%>%
  #dplyr::ungroup()
  
  
  return(df)
  
}


interpret_log_entry <- function(entry,
                                verbosity) {
  
  # Deal with counter overflow marker
  if(stringr::str_detect(entry, "\\|")) {
    entry <- stringr::str_sub(
      stringr::str_extract(entry, "\\|.*$"),
      2L,
      -1L)
  }
  
  # Extract components from log entry
  step <- stringr::str_sub(entry,
                           1L,
                           2L)
  
  branch <- stringr::str_sub(entry,
                             3L,
                             4L)
  
  entry_data <- stringr::str_sub(entry,
                                 5L,
                                 -1L)
  if(stringr::str_length(entry_data) == 0L) {
    entry_data <- NA_character_
  }
  
  # Form log entry interpretation string based on log entry components
  switch (step,
          "01" = {
            eis <- "Counter initialised to 1."
          },
          "02" = {
            if(branch == "00") {
              eis <- "Sufficient data to form at least one period."
            } else if(branch == "10") {
              eis <- "Insufficient data to form control limits."
            } else {
              eis <- "Undefined branch at step 02."
            }
          },
          "03" = {
            eis <- "Main algorithm loop commenced."
          },
          "04" = {
            if(stringr::str_sub(branch,
                                1L,
                                1L) == "0") {
              eis <- "Sufficient data to proceed."
            } else if(stringr::str_sub(branch,
                                       1L,
                                       1L) == "1") {
              eis <- paste("Insufficient remaining data for further",
                            "re-establishment of limits.")
            } else {
              eis <- "Undefined branch at step 04."
            }
            
            if(!is.na(entry_data) & stringr::str_sub(branch,
                                                     2L,
                                                     2L) == "1"){
              eis <- paste0(eis,
                            " Moving counter to the next shift rule break,",
                            " commencing at point ",
                            entry_data,
                            ".")
            }
          },
          "05" = {
            if(branch == "00") {
              eis <- "There is a shift rule break commencing here,"
              
              switch(entry_data,
                     "01" = {
                       eis <- paste(eis,
                                    "downwards from the current centre line.")
                     },
                     "10" = {
                       eis <- paste(eis,
                                    "upwards from the current centre line.")
                     },
                     {
                       eis <- paste(eis,
                                    "information on its direction is missing.")
                     })
              
            } else if(branch == "10") {
              eis <- paste("There are no subsequent shift rule breaks.")
            } else {
              eis <- "Undefined branch at step 05."
            }
          },
          "06" = {
            if(branch == "00") {
              if(verbosity > 1) {
                eis <- "Sufficient data to proceed."
              } else {
                eis <- ""
              }
              eis <- paste(eis,
                            "Forming candidate limits.")
            } else if(branch == "10") {
              eis <- paste("Insufficient remaining data for further",
                            "re-establishment of limits.")
            } else {
              eis <- "Undefined branch at step 06."
            }
            
            if(!is.na(entry_data)){
              opp <- as.logical(as.integer(stringr::str_sub(entry_data,
                                                            1L,
                                                            1L)))
              frp <- as.logical(as.integer(stringr::str_sub(entry_data,
                                                            2L,
                                                            2L)))
              opp_str <- if(opp) {
                paste("There is a shift rule break back towards",
                      "the prevailing centre line.")
              } else if(verbosity > 1){
                paste("There is no shift rule break back towards the",
                      "prevailing centre line.")
              } else {""}
              
              frp_str <- if(frp) {
                paste("The final run in the candidate calculation period may",
                      "become a shift rule break back towards the prevailing",
                      "centre line.")
              } else if(verbosity > 1){
                paste("The final run in the candidate calculation period",
                      "cannot become a shift rule break back towards the",
                      "prevailing centre line.")
              } else {""}
              
              eis <- paste(eis,
                           opp_str,
                           frp_str)
              
            }
            
          },
          "07" = {
            if(branch == "00") {
              eis <- "Candidate limits accepted, limits re-established."
            } else if(branch == "10") {
              eis <- paste("Candidate limits rejected, prevailing limits",
                           "retained.")
            } else {
              eis <- "Undefined branch at step 07."
            }
          },
          {interpretation <- "Undefined log entry"}
  )
  
  return(eis)
  
}


create_log_dataframe <- function(df,
                                 verbosity) {
  
  df <- df %>% 
    dplyr::select(x,
                  log_entry = log) %>% 
    tibble::rowid_to_column("counter") %>%
    dplyr::filter(!is.na(log_entry)) %>%
    tidyr::separate_longer_delim(log_entry,
                                 delim = ";") %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(interpretation = interpret_log_entry(log_entry,
                                                       verbosity = verbosity))
  
  
  return(df)
  
}


interpret_log <- function(df,
                          verbosity) {
  
  log_df <- create_log_dataframe(df,
                                 verbosity = verbosity)
  
  log_df <- log_df %>%
    dplyr::filter(!(trimws(interpretation) == "")) %>%
    dplyr::group_by(counter) %>%
    dplyr::mutate(interpretation = stringr::str_wrap(interpretation,
                                                     width = 60L)) %>%
    dplyr::summarise(x = dplyr::first(x),
                     interpretation = paste(interpretation,
                                            collapse = "\n- "),
                     interpretation = paste0("- ", interpretation),
                     .groups = "drop")
  
  log_txt <- log_df %>%
    dplyr::mutate(log_txt = paste0("Counter at ",
                                   counter,
                                   ", ",
                                   x,
                                   ":\n",
                                   interpretation)) %>%
    dplyr::summarise(log_txt = paste0(log_txt,
                                      collapse = "\n\n")) %>%
    dplyr::pull(log_txt)
  
  return(log_txt)
  
}


log_output <- function(df,
                       verbosity,
                       chartType ,
                       log_file_path) {
  if(verbosity > 0){
    log_text <- interpret_log(df,
                              verbosity = verbosity)
    cat(paste0("\n",
               chartType,
               ":\n\n"))
    cat(log_text)
    cat("\n\n")
  }
  
  if(!is.null(log_file_path)) {
    log_df <- create_log_dataframe(df,
                                   verbosity = 2L)
    fext <- tools::file_ext(log_file_path)
    
    if(tolower(fext) == "rds") {
      
      tryCatch(
        expr = {
          saveRDS(log_df,
                  file = log_file_path)
        },
        error = function(cnd){
          message("Unable to save log file.")
          print(cnd)
        }
      )
    } else if(tolower(fext) == "csv") {
      tryCatch(
        expr = {
          write.csv(log_df,
                    file = log_file_path)
        },
        error = function(cnd){
          message("Unable to save log file.")
          print(cnd)
        }
      )
    } else {
      warning("Invalid extension in log_file_path. Log file not written.")
    }
  }
  
}

