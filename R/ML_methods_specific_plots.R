#### FUNCTIONS IN THIS SCRIPT

# RF_variable_importance_plot
# OOB_error_plot
# RF_partial_plot
# SVM_2_class_decision_boundary

#####################################################################################
# RF_variable_importance_plot
#####################################################################################

#' Random forest variable importance plot
#'
#' @param randomForest_res randomForest object, resulting from training
#' @param color_scale color scale for the plot
#' @return a ggplot2 object of random forest variable importance plot
#' @export
#' @examples
#' data(Pima_data)
#' Pima_data=Pima_data[complete.cases(Pima_data), ]
#' X = Pima_data[, names(Pima_data) != "outcome"]
#' y = Pima_data[, "outcome"]
#' randomForest_res = randomForest::randomForest(x = X, y = y)
#' RF_variable_importance_plot(randomForest_res)
#'
RF_variable_importance_plot = function(randomForest_res, color_scale = NULL) {
  random_forest_res_importance_data = data.frame(
    varnames = rownames(randomForest_res$importance),
    randomForest_res$importance
  )

  if (inherits(randomForest_res$y, "factor")) {
    if (!is.null(color_scale)) {
      random_forest_res_importance_plot = ggplot2::ggplot(
        random_forest_res_importance_data
      ) +
        ggplot2::geom_bar(
          ggplot2::aes(x = varnames, y = MeanDecreaseGini),
          stat = "identity",
          width = 0.1,
          color = color_scale[1],
          fill = color_scale[2]
        )
    } else {
      random_forest_res_importance_plot = ggplot2::ggplot(
        random_forest_res_importance_data
      ) +
        ggplot2::geom_bar(
          ggplot2::aes(x = varnames, y = MeanDecreaseGini),
          stat = "identity",
          width = 0.1,
          color = "red",
          fill = "black"
        )
    }
  } else if (inherits(randomForest_res$y, "numeric")) {
    if (!is.null(color_scale)) {
      random_forest_res_importance_plot = ggplot2::ggplot(
        random_forest_res_importance_data
      ) +
        ggplot2::geom_bar(
          ggplot2::aes(x = varnames, y = X.IncMSE),
          stat = "identity",
          width = 0.1,
          color = color_scale[1],
          fill = color_scale[2]
        )
    } else {
      random_forest_res_importance_plot = ggplot2::ggplot(
        random_forest_res_importance_data
      ) +
        ggplot2::geom_bar(
          ggplot2::aes(x = varnames, y = X.IncMSE),
          stat = "identity",
          width = 0.1,
          color = "red",
          fill = "black"
        )
    }
  }

  random_forest_res_importance_plot = random_forest_res_importance_plot +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5)
    ) +
    ggplot2::ggtitle("Importance plot")

  return(random_forest_res_importance_plot)
}


#####################################################################################
# OOB_error_plot
#####################################################################################

#' Random forest out-of-bag error plot
#'
#' @param randomForest_res randomForest object, resulting from training
#' @param color_scale color scale for the plot
#' @return a ggplot2 object of random forest out of bag error plot
#' @export
#' @examples
#' data(Pima_data)
#' Pima_data=Pima_data[complete.cases(Pima_data), ]
#' X = Pima_data[, names(Pima_data) != "outcome"]
#' y = Pima_data[, "outcome"]
#' randomForest_res = randomForest::randomForest(x = X, y = y)
#' OOB_error_plot(randomForest_res)
#'
OOB_error_plot = function(randomForest_res, color_scale = NULL) {
  if (inherits(randomForest_res$y, "factor")) {
    OOB_error_data = as.data.frame(randomForest_res$err.rate)
    OOB_error_data$n_trees = seq_len(nrow(OOB_error_data))
    melted_OOB_error_data = reshape2::melt(
      OOB_error_data,
      id.vars = "n_trees",
      variable.name = "error_type",
      value.name = "value"
    )
    OOB_error_plot = ggplot2::ggplot(melted_OOB_error_data) +
      ggplot2::geom_line(
        ggplot2::aes(x = n_trees, y = value, color = error_type),
        linewidth = 0.8
      )

    if (!is.null(color_scale)) {
      OOB_error_plot = OOB_error_plot +
        ggplot2::scale_colour_manual(values = color_scale)
    }
  } else {
    OOB_error_data = randomForest_res$mse
    melted_OOB_error_data = data.frame(
      n_trees = 1:length(OOB_error_data),
      value = OOB_error_data
    )

    if (!is.null(color_scale)) {
      OOB_error_plot = ggplot2::ggplot(melted_OOB_error_data) +
        ggplot2::geom_line(
          ggplot2::aes(x = n_trees, y = value),
          linewidth = 0.8,
          color = color_scale[1]
        )
    } else {
      OOB_error_plot = ggplot2::ggplot(melted_OOB_error_data) +
        ggplot2::geom_line(
          ggplot2::aes(x = n_trees, y = value),
          linewidth = 0.8,
          color = "black"
        )
    }
  }

  return(OOB_error_plot)
}


#####################################################################################
# RF_partial_plot
#####################################################################################

#' Plot a random forest partial dependence curve
#'
#' @param partial_plot_res Output from \code{randomForest::partialPlot(..., plot = FALSE)}.
#' @param color_scale Optional color scale. The first color is used for the curve.
#' @return a ggplot2 object of a random forest partial dependence plot
#' @export
#' @examples
#' data(Pima_data)
#' Pima_data=Pima_data[complete.cases(Pima_data), ]
#' X = Pima_data[, names(Pima_data) != "outcome"]
#' y = Pima_data[, "outcome"]
#'
#' randomForest_res=randomForest::randomForest(x=X, y=y,
#'                               ntree=100 ,mtry=3,
#'                               importance=TRUE, proximity=TRUE)
#'
#' partial_plot_res = randomForest::partialPlot(
#'   randomForest_res,
#'   pred.data = X,
#'   x.var = "glucose",
#'   plot = FALSE
#' )
#'
#' RF_partial_plot(partial_plot_res)
#'
RF_partial_plot = function(partial_plot_res, color_scale = NULL) {
  partial_plot_data = data.frame(
    x = partial_plot_res$x,
    y = partial_plot_res$y
  )

  line_color = if (!is.null(color_scale)) {
    color_scale[1]
  } else {
    "black"
  }

  fill_color = if (!is.null(color_scale) && length(color_scale) >= 2) {
    color_scale[2]
  } else {
    "grey70"
  }

  if (is.numeric(partial_plot_data$x)) {
    partial_plot = ggplot2::ggplot(partial_plot_data) +
      ggplot2::geom_line(
        ggplot2::aes(x = x, y = y, group = 1),
        linewidth = 0.8,
        color = line_color
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = x, y = y),
        size = 1.5,
        color = line_color
      )
  } else {
    partial_plot_data$x = factor(
      partial_plot_data$x,
      levels = partial_plot_data$x
    )

    partial_plot = ggplot2::ggplot(partial_plot_data) +
      ggplot2::geom_col(
        ggplot2::aes(x = x, y = y),
        color = line_color,
        fill = fill_color
      )
  }

  partial_plot = partial_plot +
    ggplot2::labs(
      x = "Predictor value",
      y = "Partial dependence"
    )

  return(partial_plot)
}


#####################################################################################
# SVM_2_class_decision_boundary
#####################################################################################

#' Plot 2D SVM Decision Boundary with Support Vectors
#'
#' This function generates a 2D plot visualizing the decision boundary of
#' a trained Support Vector Machine (SVM) model.
#' The plot includes support vectors, real classes of the data points, and the decision regions for each class.
#'
#' @param data A data frame containing at least three columns: two numeric features (to be used as axes in the plot)
#' and a factor column representing the real class of each data point.
#' @param svm_res An object of class \code{svm}, returned by the \code{e1071::svm()} function,
#'  representing the trained SVM model.
#'
#' @details
#' The function first identifies which data points
#' are support vectors based on the indices provided by \code{svm_res$index}.
#' It then constructs a grid of values to predict the class labels for each grid point using the trained SVM model,
#' allowing the decision boundary to be plotted.
#'
#' The plot displays the decision boundary by using a color gradient to represent different predicted classes.
#' Data points are overlaid on the decision boundary with a different shape to distinguish support vectors.
#' The real class of each data point is indicated by color.
#'
#' @return A ggplot2 object showing the 2D decision boundary, data points, and support vectors.
#'
#' @export
#'
#' @import stats
#' @import rlang
#' @import ggplot2
#'
#' @examples
#' data(Pima_data)
#' Pima_data=Pima_data[complete.cases(Pima_data), ]
#' data_decision_boundary=Pima_data[, c("bmi", "age", "outcome")]
#' decision_boundary_svm_res=e1071::svm(formula=outcome ~ bmi + age, data=data_decision_boundary)
#' print(SVM_2_class_decision_boundary(data_decision_boundary, decision_boundary_svm_res))
#'
SVM_2_class_decision_boundary = function(data, svm_res) {
  grid_x1 <- seq(min(data[, 1]) - 0.5, max(data[, 1]) + 0.5, length.out = 500)
  grid_x2 <- seq(min(data[, 2]) - 0.5, max(data[, 2]) + 0.5, length.out = 500)

  full_grid <- stats::setNames(
    expand.grid(grid_x1, grid_x2),
    colnames(data)[1:2]
  )

  full_grid = cbind(full_grid, predict(svm_res, newdata = full_grid))
  colnames(full_grid)[3] = colnames(data)[3]

  data$is_SV = factor(
    ifelse(1:nrow(data) %in% svm_res$index, "Yes", "No"),
    levels = c("No", "Yes")
  )

  out_plot = ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[names(data)[1]]],
      y = .data[[names(data)[2]]],
      color = .data[[names(data)[3]]]
    )
  ) +
    ggplot2::geom_tile(
      data = full_grid,
      ggplot2::aes(fill = .data[[names(data)[3]]]),
      color = NA,
      alpha = 0.2
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = is_SV), size = 2, stroke = 1.5) +
    ggplot2::scale_shape_manual(
      "Is Support Vector?",
      values = c('Yes' = 4, 'No' = 16)
    ) +
    ggplot2::scale_fill_manual(
      "Predicted class",
      values = stats::setNames(c("#E41A1C", "#377EB8"), levels(data[, 3]))
    ) +
    ggplot2::scale_color_manual(
      "Real class",
      values = stats::setNames(c("#E41A1C", "#377EB8"), levels(data[, 3]))
    )

  return(out_plot)
}
