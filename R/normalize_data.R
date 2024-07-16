#' Normalize Data
#'
#' This function normalizes numeric columns in a data frame using min-max scaling.
#'
#' @param df A data frame.
#' @return A data frame with normalized numeric columns.
#' @examples
#' df <- data.frame(a = c(1, 2, 3, 4), b = c(5, 6, 7, 8))
#' normalize_data(df)
#' @export
normalize_data <- function(df) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  })
  return(df)
}
