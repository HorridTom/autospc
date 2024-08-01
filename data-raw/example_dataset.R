## code to prepare `example_dataset` dataset goes here
# This function uses nhsAEscraper to find a dataset based on real NHS data
# devtools::install_github("HorridTom/nhsAEscraper")

usethis::use_data(example_dataset, overwrite = TRUE)

get_example_dataset <- function() {
  AE_Data_England <- getAE_data(country = "England")
  example_dataset <- AE_Data_England %>% filter(Prov_Code == 'RXC')
  #Removing unnecessary columns from dataset
  example_dataset <- example_dataset %>% select(-c(
    Region,
    Prov_Code, Prov_Name,
    Att_Typ1, Att_Typ2, Att_Typ3, 
    Att_Typ1_Br, Att_Typ2_Br, Att_Typ3_Br, 
    Perf_Typ1, 
    E_Adm_Typ1, E_Adm_Typ2, E_Adm_Typ34, 
    SourceFile, hashSourceFileContents))
  #Changing format of date to allow autoSPC to run
  example_dataset$Month_Start <- as.Date(example_dataset$Month_Start) 
  example_dataset <- example_dataset %>% arrange(Month_Start)
  example_dataset
}

usethis::use_data()
