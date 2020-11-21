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

  #' Determine the number of dimensions in an integration analysis dataframe
#'
#' @param d an integration analysis dataframe
#'
#' @return the dimensionality of the integrand represented
#' @export
#'
n_dimensions <- function(d) {
  n_dim_cols <- length(grep("^dim", names(d)))
  # If we don't have an even number of columns with 'dimension' names, then we
  # have the wrong kind of dataframe
  stopifnot(n_dim_cols %% 2 == 0)
  n_dim_cols/2
}

#' Determine the names of the column dimensions
#'
#' @param d an integration analysis dataframe
#'
#' @return a character vector containing the names of all dimension columns
#' @export
#'
get_dim_names <- function(d) {
  all_names <- names(d)
  all_names[grep("^dim", all_names)]
}

#' Turn dimension names into canonical form
#'
#' @param d an integration analysis dataframe
#'
#' @return a copy of the input dataframe, with dimension names canonicalized
#' @export
#' @importFrom checkmate assert_count
#' @importFrom purrr map2_chr
#'
canonicalize_dim_names <- function(d) {
  dim_names <- get_dim_names(d)
  n_dims <- n_dimensions(d)
  assert_count(n_dims, positive = TRUE)
  dim_ids <- stringr::str_match(dim_names, "dim.*([[:digit:]]+)")[,2]
  templates <- rep(c("dim_%s_lo", "dim_%s_hi"), n_dims)
  new_names <- map2_chr(templates, dim_ids, sprintf)
  names(d)[c((length(d) + 1 - n_dims*2):length(d))] <- new_names
  d
}

#' Augment a raw integration analysis dataframe with lengths and volumes
#'
#' @param d : a raw integration dataframe
#'
#' @return a copy of the input dataframe, with dimension names canonicalized and
#'     dimension lengths and volume added
#' @export
#' @importFrom dplyr pull relocate
#' @importFrom tibble is_tibble as_tibble
#' @importFrom tidyselect last_col starts_with
#' @importFrom magrittr `%>%`
#'
augment_raw_dataframe <- function(d) {
  tmp <- canonicalize_dim_names(d)
  if (!is_tibble(tmp))
    tmp <- as_tibble(tmp)
  # Add vol = 1 (for all entries)
  tmp$vol <- 1
  # Iterate through dimensions numbers 'n'
  last_dim = n_dimensions(d) - 1
  for (i in 0:last_dim) {
    new_length <-
      pull(tmp, sprintf("dim_%d_hi", i)) -
      pull(tmp, sprintf("dim_%d_lo", i))
    tmp[sprintf("len_%d", i)] <- new_length
    tmp$vol = tmp$vol * new_length
  }
  tmp %>%
    relocate(starts_with("len"), .after = last_col()) %>%
    relocate(starts_with("dim"), .after = last_col())
}

