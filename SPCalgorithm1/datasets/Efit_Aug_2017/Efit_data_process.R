library(gdata)

efit_data <- read.xls("C:/Users/tw299/git/spc-algorithm/SPCalgorithm1/datasets/Efit_Aug_2017/Efit_SPCdata_Test.xlsx")
efit_data$month <- as.Date(efit_data$Period)

save_efit_data_for_algorithm <- function(efit_data, sv_dir =
                                           'C:/Users/tw299/git/spc-algorithm/SPCalgorithm1/datasets/Efit_Aug_2017/') {
  uom <- unique(efit_data %>% select(Provider, indicator.reference))
  apply(uom, 1, function(x) {
    org <- x['Provider']
    measure <- x['indicator.reference']
    save_efit_org_measure_vector(efit_data, org = org, measure = measure, sv_dir = sv_dir)
  })
  
}

save_efit_org_measure_vector <- function(efit_data, org, measure, sv_dir =
                                           'C:/Users/tw299/git/spc-algorithm/SPCalgorithm1/datasets/Efit_Aug_2017/') {
  v <- efit_data %>% filter(Provider == org, indicator.reference == measure) %>%
    arrange(month) %>%
    select(value)
  
  fn <- paste(sv_dir, org,'_',measure,'.csv',sep = '')
  
  write.table(v, file = fn, sep=',', row.names = FALSE, col.names = FALSE)
  
}