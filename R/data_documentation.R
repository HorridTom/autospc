#' The Example Dataset
#' 
#' A set of attendance and admission data from one anonymised hospital between 2015-2024
#' 
#' @format 
#' A data frame with 109 rows and 9 columns
#' \describe{
#'  \item{Month_Start}{Start date for that month of data}
#'  \item{Att_All}{Total attendance to A&E}
#'  \item{Att_All_Br}{Total attendances to A&E that were < 4 hours}
#'  \item{Perf_All}{Percentage of attendances within 4 hours}
#'  \item{E_Adm_All_ED}{Total emergency admissions via A&E}
#'  \item{E_Adm_4hBr_D}{Total spending > 4 hours from decision to admit to admission}
#' }
#' 
#' @source <https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2023-24/>
#' 
"example_dataset"
