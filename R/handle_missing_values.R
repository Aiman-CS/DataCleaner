#' Handle Missing Values
#'
#' This function handles missing values in a data frame by either removing rows with missing values or imputing them with the mean, median, or mode.
#' @param df A data frame.
#' @param method A string specifying the method to handle missing values: "remove", "mean", "median", "mode".
#' @return A data frame with missing values handled.
#' @examples
#' library(dplyr)
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 2, 3, 4))
#' # Remove rows with missing values
#' handle_missing_values(df, method = "remove")
#'
#' # Impute missing values with mean
#' handle_missing_values(df, method = "mean")
#'
#' # Impute missing values with median
#' handle_missing_values(df, method = "median")
#'
#' # Impute missing values with mode
#' handle_missing_values(df, method = "mode")
#' @export
handle_missing_values <- function(df, method = "mean") {
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (method == "remove") {
    return(na.omit(df))
  } else if (method %in% c("mean", "median")) {
    df <- df %>%
      mutate(across(everything(), ~ ifelse(is.na(.), match.fun(method)(., na.rm = TRUE), .)))
    return(df)
  } else if (method == "mode") {
    mode_function <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    df <- df %>%
      mutate(across(everything(), ~ ifelse(is.na(.), mode_function(.), .)))
    return(df)
  } else {
    stop("method must be 'remove', 'mean', 'median', or 'mode'")
  }
}
