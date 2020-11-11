#' Read feather or RDS data.
#'
#' This function reads data from either a Feather or RDS file, preferring the
#' Feather file (if it exists).
#'
#' @param name The basename of the file (without the .feather or .rds extension)
#'
#' @return The object read from the file.
#' @export
#'
#' @examples
read_feather_or_rds <- function(name) {
  checkmate::assert_scalar(name)
  checkmate::assert_character(name)

  feather_name <- sprintf("%s.feather", name)
  if (file.exists(feather_name))
    arrow::read_feather(feather_name)
  else
    readRDS(sprintf("%s.rds", name))
}
