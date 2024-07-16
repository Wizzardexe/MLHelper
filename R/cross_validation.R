#' Perform k-fold cross-validation
#'
#' This function performs k-fold cross-validation on a given model and dataset.
#' A seed is used to ensure reproducibility.
#' @param model A model object (e.g., lm, glm)
#' @param data A data frame containing the dataset
#' @param k An integer representing the number of folds (default is 5)
#' @param seed An integer seed for reproducibility (default is 123)
#' @return A list with the following components:
#' \item{predictions}{A list of numeric vectors, where each vector contains the predictions for one fold}
#' @examples
#' data(mtcars)
#'
#' model <- lm(mpg ~ ., data = mtcars)
#' cv_results <- cross_validation(model, mtcars, k = 5)
#'
#' head(cv_results$predictions)
#' @export
cross_validation <- function(model, data, k = 5, seed = 123) {
  set.seed(seed)
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  results <- sapply(1:k, function(i) {
    test_indices <- which(folds == i, arr.ind = TRUE)
    test_data <- data[test_indices, ]
    train_data <- data[-test_indices, ]
    model_fit <- update(model, data = train_data)
    predict(model_fit, newdata = test_data)
  })
  list(predictions = results)
}
