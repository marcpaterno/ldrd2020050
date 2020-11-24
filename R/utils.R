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
  names(d)[grep("dim.*([[:digit:]]+)", names(d))] <- new_names
  d
}

#' Augment a raw integration analysis dataframe with lengths and volumes
#'
#' @param d : a raw integration dataframe
#'
#' @return a copy of the input dataframe, with dimension names canonicalized and
#'     dimension lengths and volume added
#' @export
#' @importFrom dplyr pull relocate mutate
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
    mutate(active = as.logical(.data$active)) %>%
    relocate(starts_with("len"), .after = last_col()) %>%
    relocate(starts_with("dim"), .after = last_col())
}


#' Summarize an augmented raw integration dataframe by iteration
#'
#' @param d : an augmented raw integration dataframe
#'
#' @return a dataframe containing a per-iteration summary of the input dataframe
#' @export
#' @importFrom dplyr group_by n summarize
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#'
make_iteration_dataframe <- function(d)
{
  d %>%
    group_by(.data$iteration) %>%
    summarize(estimate = sum(.data$estimate),
              errorest = sum(.data$errorest),
              low = .data$estimate - .data$errorest,
              high = .data$estimate + .data$errorest,
              vol = sum(.data$vol),
              nregions = n(),
              .groups = "drop")
}

#' Selected finished regions from an augmented integration dataframe
#'
#' @param d : an aumented raw integration dataframe
#'
#' @return a dataframe contained only finished regions
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom tibble add_row
#'
select_finished <- function(d)
{
  parents <- unique(d$parentID)
  filter(d, .data$id %nin% parents) %>%
    filter(.data$iteration < max(.data$iteration))
}

#' Summarize finished regions by iteration
#'
#' @param d : an augmented raw integration dataframe
#'
#' @return a dataframe with one row per iteration, summarizing the input data
#' @export
#' @importFrom dplyr left_join mutate
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#'
summarize_finished_by_iteration <- function(d) {
  finished <- select_finished(d)
  tmp <-
    group_by(finished, .data$iteration) %>%
    summarize(
      n = n(),
      min = min(.data$estimate),
      max = max(.data$estimate),
      estimate = sum(.data$estimate),
      errorest = sum(.data$errorest),
      vol = sum(.data$vol),
      .groups = "drop",
    )
  # Add rows for iterations that had no finished regions.
  # They contain all zeros.
  tmp <-
    left_join(tibble(iteration = 0:max(d$iteration)),
              tmp,
              by = "iteration") %>%
    replace_na(replace = list(
      n = 0,
      min = 0,
      max = 0,
      estimate = 0,
      errorest = 0,
      vol = 0
    ))
  # Add cumulative sum results.
  tmp %>%
    mutate(
      tot.estimate = cumsum(.data$estimate),
      tot.errorest = cumsum(.data$errorest),
      tot.vol = cumsum(.data$vol)
    )
}
