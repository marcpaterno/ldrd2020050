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

#' Augment an integration analysis dataframe with region lengths.
#'
#' @param d an integration analysis tibble
#'
#' @return a tibble, augmented to carry a length for each dimension
#' @export
#' @importFrom dplyr select mutate inner_join
#' @importFrom tidyr pivot_longer separate pivot_wider
#' @importFrom checkmate assert_tibble
#' @importFrom stringr str_extract
#' @importFrom tidyselect starts_with
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
add_lengths <- function(d)
{
  assert_tibble(d)
  dims <- select(d, .data$iteration, .data$id, starts_with("dim")) %>%
    pivot_longer(cols = starts_with("dim")) %>%
    separate(.data$name, into=c("dim", "edge")) %>%
    mutate(dim = as.integer(str_extract(.data$dim, "[[:digit:]]+"))) %>%
    pivot_wider(names_from = .data$edge, values_from = .data$value)  %>%
    mutate(len = .data$hi - .data$lo) %>%
    pivot_wider(names_from = .data$dim, values_from = c(.data$lo, .data$hi, .data$len))

  inner_join(select(dims, .data$iteration, .data$id, starts_with("len")),
             d,
             by = c("iteration", "id"))
}

#' Augment an integration analysis dataframe with volumes
#'
#' @param d a tibble containing lengths for each dimension (see \code{add_lengths})
#'
#' @return a tibble, augmented to carry a total volume
#' @export
#' @importFrom checkmate assert_tibble
#' @importFrom dplyr c_across rowwise summarize
#' @importFrom tidyselect starts_with
#' @importFrom magrittr %>%
#' @importFrom rlang .data
add_volume <- function(d) {
  assert_tibble(d)
  tmp <-
    rowwise(d, .data$iteration, .data$id) %>%
    summarize(vol = prod(c_across(starts_with("len"))), .groups = "drop")
  inner_join(d, tmp, by = c("iteration", "id"))
}
