#' Negated value matching.
#'
#' %nin% is a binary operator which returns a logical vector indicating
#' if there is not a match for its left operand. It is the logical negation
#' of %in%.
#'
#' @param x vector, the values to be matched
#' @param y vector, the values to be matche against
#'
#' @return a vector of the same length as `x`
#' @examples
#' 1:10 %nin% 3:8
#' @export
#'
`%nin%` <- function(x, y) {
  !x %in% y
}


#' Convert RDS file to Feather file.
#'
#' @param infile character, name of RDS file to be read
#' @param outfile character, name of Feather file to be written
#' @param compression see arrow::write_feather
#' @param compression_level see arrow::write_feather
#' @param chunk_size see arrow::write_feather
#'
#' @return the data read, invisibly
#' @export
#' @importFrom checkmate assert_scalar assert_character assert_file_exists assert_path_for_output
#' @importFrom arrow write_feather
#'
rds_to_feather <- function(infile, outfile,
                           compression = "uncompressed",
                           compression_level = NULL,
                           chunk_size = 65536L
                           )
{
  assert_scalar(infile)
  assert_scalar(outfile)
  assert_character(infile)
  assert_character(outfile)
  assert_file_exists(infile)
  assert_path_for_output(outfile)
  x <- readRDS(infile)
  write_feather(x, outfile, compression = compression)
}
