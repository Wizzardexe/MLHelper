#' Plot feature importance for models
#'
#' This function calculates and plots feature importance for models that support it.
#' @param model A trained model object
#' @return A ggplot object showing feature importance
#' @examples
#' model <- train_model(mpg ~ ., mtcars, method = "rpart")
#' plot_feature_importance(model)
#' @export
plot_feature_importance <- function(model) {
  if (inherits(model, "rpart")) {
    library(caret)
    importance <- as.data.frame(varImp(model))
    importance$Feature <- rownames(importance)
    importance <- importance[order(importance$Overall, decreasing = TRUE), ]
    library(ggplot2)
    p <- ggplot(
        data = importance,
        mapping = aes(
          x = reorder(Feature, Overall),
          y = Overall
          )
        ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = "Feature Importance",
          x = "Feature",
          y = "Importance"
        ) +
        theme_minimal()
    return(p)
  } else {
    stop("Feature importance plotting not supported for this model type")
  }
}
