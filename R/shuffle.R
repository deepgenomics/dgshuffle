#' Triple and shuffle columns in a dataframe with an even number of columns.
#'
#' @description Given a dataframe with columns ABCD, return a dataframe
#' with columns AAACCC if offset is 1 or columns BBBDDD if offset is 2.
#' The new columns are given names x1..xN.
#'
#' @param df (dataframe) The dataframe to shuffle.
#' @param offset (integer) Whether to grab odd (1) or even (2) columns.
#' @param each (integer) How many times to repeat each column (default 3).
#'
#' @return Selected and tripled columns.
#'
#' @export
select_repeat_cols <- function(df, offset, each=3) {
  # Require an even number of columns, 1 or 2 as offset, and positive repeat
  stopifnot((ncol(df) > 0) && (ncol(df) %% 2 == 0))
  stopifnot((offset == 1) || (offset == 2))
  stopifnot(each > 0)

  # Generate the column indices c(1, 3, 5, ...) or c(2, 4, 6, ...)
  column_indices <- seq(offset, ncol(df), 2)

  # Replicate each index three times, e.g., create c(1, 1, 1, 3, 3, 3, ...)
  tripled_column_indices <- rep(column_indices, each = each)

  # Select those columns (replicating as we select).
  new_df <- df[, tripled_column_indices]

  # Create new column names c("x1", "x2", "x3", ...)
  new_col_names <-
    stringr::str_c(rep("x", ncol(new_df)), seq(1, ncol(new_df)))

  # Rename columns.
  colnames(new_df) <- new_col_names

  # Return new dataframe.
  new_df
}

#' Shuffle indices so that c(1, 2, 3, 4) maps to c(1, 3, 2, 4).
#'
#' @description Given an even number n, produce the vector c(1, 3, 2, 4, ...)
#' up to n.
#'
#' @param n (integer) The (even) number to shuffle.
#'
#' @return Shuffled indices.
#'
#' @export
shuffle_indices <- function(n) {
  # Require an even number.
  stopifnot((n > 0) && (n %% 2 == 0))

  # Generate and shuffle.
  nums <- seq(n)
  ifelse(nums %% 2, (nums + 1) / 2, (nums / 2) + length(nums) / 2)
}

#' Shuffle a dataframe, repeating columns.
#'
#' @description Given a dataframe with columns ABCD and an even number of rows,
#' shuffle and duplicate the columns to produce (e.g.) AAACCCBBBDDD and shuffle
#' the rows from 1234 to 1324.
#'
#' @param df (dataframe) The dataframe to shuffle.
#' @param each (integer) How many times to repeat each column.
#'
#' @return The shuffled/duplicated dataframe.
#'
#' @export
shuffle_data <- function(df, each=3) {
  # Require an even number of rows and columns.
  stopifnot(nrow(df) %% 2 == 0)
  stopifnot(ncol(df) %% 2 == 0)

  # Require a positive number of repeats.
  stopifnot(each > 0)

  # Select and triple the odd columns.
  odd_frame <- select_repeat_cols(df, 1, each)

  # Select and triple the even columns.
  even_frame <- select_repeat_cols(df, 2, each)

  # Stack the two frames on top of each other.
  joined_frame <- dplyr::bind_rows(odd_frame, even_frame)

  # Shuffle the rows in the joined frame and return the result.
  new_row_indices <- shuffle_indices(nrow(joined_frame))
  joined_frame[new_row_indices,]
}
