#' Train a machine learning model
#'
#' This function trains a machine learning model using the provided formula and data.
#' @param formula A formula specifying the model
#' @param data A data frame containing the dataset
#' @param method A character string specifying the model type (e.g., "lm", "rpart")
#' @return A trained model object. The type of the returned object depends on the method used:
#' \item{lm}{If method is "lm", returns an object of class "lm"}
#' \item{rpart}{If method is "rpart", returns an object of class "rpart"}
#' @examples
#'
#' model <- train_model(mpg ~ ., mtcars, method = "lm")
#' summary(model)
#'
#' model <- train_model(mpg ~ ., mtcars, method = "rpart")
#' summary(model)
#'
#' @export
train_model <- function(formula, data, method = "lm") {
  if (method == "lm") {
    model <- lm(formula, data)
  } else if (method == "rpart") {
    library(rpart)
    model <- rpart(formula, data)
  } else {
    stop("Unsupported method")
  }
  return(model)
}
