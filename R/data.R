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
#'  \item{E_Adm_Via_ED}{Total emergency admissions via emergency department}
#'  \item{E_Adm_Over_4h}{Total > 4 hours from decision to admit to admission}
#' }
#' 
#' @source <https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2023-24/>
#' 
"ed_attendances_monthly"

#' Example Time Series 1
#' 
#' A simulated example time series used in illustrating features of the Stable
#' Shift Algorithm.
#' 
#' @format
#' A data frame with 125 rows and 2 columns
#' \describe{
#'  \item{x}{Consecutive integer time}
#'  \item{y}{Numeric time series}
#' } 
#' 
#' @source Simulated data
#' 
"example_series_1"

#' Example Time Series 2a
#' 
#' A second simulated example time series used in illustrating features of the
#' Stable Shift Algorithm.
#' 
#' @format
#' A data frame with 43 rows and 2 columns
#' \describe{
#'  \item{x}{Consecutive integer time}
#'  \item{y}{Integer time series}
#' } 
#' 
#' @source Simulated data
#' 
"example_series_2a"

#' Example Time Series 2b
#' 
#' A third simulated example time series used in illustrating features of the
#' Stable Shift Algorithm. Identical to example_series_2a except at seven time
#' points, all occurring at or after x = 27.
#' 
#' @format
#' A data frame with 43 rows and 2 columns
#' \describe{
#'  \item{x}{Consecutive integer time}
#'  \item{y}{Integer time series}
#' } 
#' 
#' @source Simulated data
#' 
"example_series_2b"

#' Example Time Series 2c
#' 
#' A fourth simulated example time series used in illustrating features of the
#' Stable Shift Algorithm. The first 43 rows are identical to example_series_2a
#' except at six time points, all occurring at or after x = 38.
#' 
#' @format
#' A data frame with 47 rows and 2 columns
#' \describe{
#'  \item{x}{Consecutive integer time}
#'  \item{y}{Integer time series}
#' } 
#' 
#' @source Simulated data
#' 
"example_series_2c"
