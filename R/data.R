#' Regions from 20 parallel CUHRE iterations on the Genz Corner Peak function.
#'
#' A "raw integration dataset" containing the region data from 20 iterations
#' of the parallel CUHRE integration algorithm, on the Genz Corner Peak function
#' in 8 dimensions.
#'
#' @format A dataframe with 170803 rows and 24 variables:
#' \describe{
#'   \item{iteration}{the iteration number, starting at 0}
#'   \item{id}{the id of the region, starting at 0}
#'   \item{parentID}{the id of the region's parent; -1 means no parent}
#'   \item{estimate}{the estimate of the integral in this region}
#'   \item{errorest}{the estimate of the integral error in this region}
#'   \item{parEst}{the estimate of the integral the region's parent}
#'   \item{parErr}{the estimate of the integral error in the region's parent}
#'   \item{active}{0 if the region is finished in this iteration, 1 if it is still active (and will be split)}
#'   \item{dim[n]low}{the lower edge of the integration range for dimension $n$}
#'   \item{dim[n]high}{the upper edge of the integration range for dimension $n$}
#' }
#' @source Google Drive document https://drive.google.com/file/d/1zBMOSaab0ZNrS8vk6twCebNCfUoKj6-z/view?usp=sharing.
"rawcornerpeak"
