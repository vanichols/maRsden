#' Maximum root depth across the season for 2018 and 2019
#'
#' A dataset containing data collected by Gina Nichols in the corn phase of the 2- and 4-year rotations.
#'
#' @format A data frame with 104 rows and 6 variables:
#' \describe{
#'   \item{year}{crop year of data collection}
#'   \item{date}{date of data collection in yyyy-mm-dd format}
#'   \item{doy}{day of the year of data collection}
#'   \item{plot_id}{unique plot id, see mrs_plotkey for associated info}
#'   \item{stage}{corn stage of measurement; planting, V1-V12, R1-R6}
#'   \item{rootdepth_cm}{average of 4 sub-rep measurements in that plot}
#'   ...
#' }
"mrs_rootdepth"
