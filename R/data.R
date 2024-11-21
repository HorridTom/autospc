#' Emergency Department Attendances Monthly data
#' 
#' Volume, performance and admission data for emergency department attendances
#' at an English hospital between 2015-2024
#' 
#' @format 
#' A data frame with 109 rows and 9 columns
#' \describe{
#'  \item{Month_Start}{First date of the month in question}
#'  \item{Att_All}{Total emergency attendances in the month in question}
#'  \item{Within_4h}{Total attendances that were < 4 hours}
#'  \item{Over_4h}{Total attendances that were > 4 hours}
#'  \item{Percent_in_4h}{Percentage of attendances within 4 hours}
#'  \item{E_Adm_ViaAE}{Total emergency admissions via emergency department}
#'  \item{E_Adm_Over_4h}{Total > 4 hours from decision to admit to admission}
#' }
#' 
#' @source <https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2023-24/>
#' 
"ed_attendances_monthly"
