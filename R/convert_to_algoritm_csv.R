
make_algoritm_csv <- function(){
  
  files <- list.files(path = "C:/Users/imooc/Documents/SPC-charts/tables", pattern = "*.csv")
  
  for (fname in files){
    data <- read.csv(paste0("tables/", fname))
    y <- dplyr::select(data, y)
    write.csv(y, paste0("C:/Users/imooc/Documents/SPC-charts/spc_csv/", fname), row.names = F)
  }

}