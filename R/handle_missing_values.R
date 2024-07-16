#' Handle Missing Values
#' This function handles missing values in a data frame by either removing rows with missing values or imputing them with the mean, median, or mode.
#' @param df A data frame.
#' @param method A string specifying the method to handle missing values: "remove", "mean", "median", "mode".
#' @return A data frame with missing values handled.
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 2, 3, 4))
#' handle_missing_values(df, method = "mean")
#' @export
handle_missing_values <- function(df, method = "mean") {
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (method == "remove") {
    return(na.omit(df))
  } else if (method %in% c("mean", "median", "mode")) {
    df <- df %>%
      mutate(across(everything(), ~ ifelse(is.na(.), get(method)(., na.rm = TRUE), .)))
    return(df)
  } else {
    stop("method must be 'remove', 'mean', 'median', or 'mode'")
  }
}
