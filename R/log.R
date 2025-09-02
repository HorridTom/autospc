
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
  
  updated_log_entry <- paste_narm(existing_log_entry,
                                  as.character(step),
                                  collapse = ";")
  
  df <- df %>%
    #dplyr::rowwise() %>%
    dplyr::mutate(log = dplyr::if_else(dplyr::row_number() == counter,
                                       updated_log_entry,
                                       log)) #%>%
    #dplyr::ungroup()
  
  
  return(df)
  
}


paste_narm <- function(v,
                       ...) {
  v <- v[!is.na(v)]
  return(paste(v,
               ...))
}
