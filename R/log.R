
record_log_entry <- function(df,
                             counter,
                             step){
  
  if(!("log" %in% colnames(df))){
    df <- df %>%
      dplyr::mutate(log = NA_character_)
  }
  
  existing_log_entry <- df %>%
    dplyr::filter(dplyr::row_number() == counter) %>%
    dplyr::pull(log)
  
  if(is.na(existing_log_entry)) {
    updated_log_entry <- as.character(step)
  } else {
    updated_log_entry <- paste(existing_log_entry,
                               as.character(step),
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

