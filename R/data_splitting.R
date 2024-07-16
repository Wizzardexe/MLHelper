#' Split data into training and testing sets
#'
#' This function splits a dataset into training and testing sets.
#' A seed is used to ensure reproducibility.
#' @param data A data frame containing the dataset
#' @param split_ratio A numeric value representing the proportion of data to be used for training (default is 0.7)
#' @param seed An integer seed for reproducibility (default is 123)
#' @return A list with two components:
#' \item{train}{A data frame containing the training set}
#' \item{test}{A data frame containing the testing set}
#' @examples
#' split_data <- data_split(mtcars, split_ratio = 0.7)
#'
#' train_data <- split_data$train
#' test_data <- split_data$test
#'
#' head(train_data)
#' head(test_data)
#' @export
data_split <- function(data, split_ratio = 0.7, seed = 123) {
  set.seed(seed)
  train_indices <- sample(seq_len(nrow(data)), size = split_ratio * nrow(data))
  train <- data[train_indices, ]
  test <- data[-train_indices, ]
  list(train = train, test = test)
}
