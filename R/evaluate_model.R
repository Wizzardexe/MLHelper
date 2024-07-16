#' Calculate evaluation metrics for a model
#'
#' This function calculates RMSE and MAE for model predictions.
#' @param actual A numeric vector of actual values
#' @param predicted A numeric vector of predicted values
#' @return A list with the following components:
#' \item{RMSE}{A numeric value representing the Root Mean Squared Error of the predictions}
#' \item{MAE}{A numeric value representing the Mean Absolute Error of the predictions}
#' @examples
#' actual <- mtcars$mpg
#' predicted <- predict(lm(mpg ~ ., data = mtcars), mtcars)
#' metrics <- evaluate_model(actual, predicted)
#'
#' cat("RMSE:", metrics$RMSE, "\n")
#' cat("MAE:", metrics$MAE, "\n")
#' @export
evaluate_model <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  list(RMSE = rmse, MAE = mae)
}
