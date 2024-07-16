#' Calculate and display feature importance for models
#'
#' This function calculates and displays feature importance for models that support it.
#' @param model A trained model object
#' @return A data frame with two columns:
#' \item{Feature}{The name of the feature}
#' \item{Importance}{The importance score of the feature}
#' @examples
#' model <- train_model(mpg ~ ., mtcars, method = "rpart")
#' importance <- feature_importance(model)
#'
#' print(importance)
#' @export
feature_importance <- function(model) {
  if (inherits(model, "rpart")) {
    library(caret)
    library(ggplot2)
    library(lattice)
    importance <- as.data.frame(caret::varImp(model))
  } else {
    stop("Feature importance not supported for this model type")
  }
  return(importance)
}
