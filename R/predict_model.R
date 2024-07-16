#' Make predictions using a trained model
#'
#' This function makes predictions on new data using a trained model.
#' @param model A trained model object
#' @param newdata A data frame containing the new data
#' @return A numeric vector of predictions. The length of the vector is equal to the number of rows in newdata.
#' @examples
#' model <- train_model(mpg ~ ., mtcars, method = "lm")
#' predictions <- predict_model(model, newdata = mtcars)
#'
#' head(predictions)
#' @export
predict_model <- function(model, newdata) {
  predictions <- predict(model, newdata)
  return(predictions)
}
