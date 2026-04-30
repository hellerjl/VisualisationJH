#####################################################################################
# density_plot_JH
#####################################################################################

#' Density plot using ggplot2 package, with or without stratification by a factor variable
#'
#' @param data Numeric data for the density plot.
#' @param factor Optional factor variable for stratification.
#' @param factor_name Optional name of the factor variable.
#' @param color_scale Optional hexadecimal color scale for the density(ies).
#' @param custom_theme Optional custom ggplot theme.
#' @param na.rm Remove the NA values from the density calculation.
#'
#' @return A ggplot2 object of the density plot
#' @importFrom magrittr %>%
#' @export
#' @examples
#' data("Pima_data")
#' Pima_data=Pima_data[complete.cases(Pima_data), ]
#' density_plot_JH(Pima_data[,"bmi"],
#'                     color_scale = NULL)
#' density_plot_JH(Pima_data[,"bmi", drop=FALSE], factor=Pima_data$type,
#'                     factor_name="Type")
#' density_plot_JH(Pima_data[,"bmi", drop=FALSE],
#'                     color_scale = "#0307fc")
#'
density_plot_JH = function(
  data,
  factor = NULL,
  factor_name = NULL,
  color_scale = NULL,
  custom_theme = NULL,
  na.rm = FALSE
) {
  x_lab = if (!is.null(dim(data))) {
    colnames(data)[1]
  } else {
    deparse(substitute(data))
  }

  if (!is.null(dim(data))) {
    data = data[, 1]
  }

  if (is.null(factor)) {
    density_object = stats::density(data, na.rm = na.rm)
    density_data = data.frame(x = density_object$x, y = density_object$y)
    density_plot = ggplot2::ggplot(density_data) +
      ggplot2::xlab(x_lab) +
      ggplot2::ylab("Density") +
      custom_theme

    if (is.null(color_scale)) {
      density_plot = density_plot +
        ggplot2::geom_line(ggplot2::aes(x = x, y = y), size = 1, color = "navy")
    } else {
      density_plot = density_plot +
        ggplot2::geom_line(
          ggplot2::aes(x = x, y = y),
          size = 1,
          color = color_scale[1]
        )
    }

    return(density_plot)
  } else {
    density_list = data %>%
      base::by(INDICES = factor, FUN = function(x) {
        stats::density(x, na.rm = na.rm)
      })
    factors_vector = NULL
    for (k in 1:length(names(density_list))) {
      factors_vector = c(
        factors_vector,
        rep(names(density_list)[k], length(density_list[[k]]$x))
      )
    }

    density_data = data.frame(
      x = unlist(lapply(density_list, function(x) {
        x$x
      })),
      y = unlist(lapply(density_list, function(x) {
        x$y
      })),
      factor = as.factor(factors_vector)
    )

    density_plot = ggplot2::ggplot(density_data) +
      ggplot2::xlab(x_lab) +
      ggplot2::ylab("Density") +
      custom_theme

    if (is.null(color_scale)) {
      density_plot = density_plot +
        ggplot2::geom_line(
          ggplot2::aes(x = x, y = y, color = factor),
          size = 1
        ) +
        ggplot2::labs(colour = factor_name)
    } else {
      density_plot = density_plot +
        ggplot2::geom_line(
          ggplot2::aes(x = x, y = y, color = factor),
          size = 1
        ) +
        ggplot2::scale_color_manual(
          factor_name,
          labels = levels(factor),
          values = color_scale
        )
    }

    return(density_plot)
  }
}


#####################################################################################
# histogram_plot_JH
#####################################################################################

#' Histogram plot using the ggplot2 package
#'
#' @description
#' Histogram plot using the ggplot2 package, with the possibility of adding a density line
#'
#' @details
#' The histogram is scaled to density so an optional density curve can be overlaid.
#'
#' @param data numeric data for the histogram (can be a vector, or a single column data frame).
#' @param na.rm remove the NA values from the density calculation? (default: FALSE)
#' @param n_bins number of bins for the histogram. (default: 30)
#' @param plot_density plot density on top of the histogram? (default: TRUE)
#' @param color_scale optional color scale. (default: NULL)
#'
#' @return
#' a ggplot2 object that can be printed
#'
#' @export
#'
#' @examples
#' data("Pima_data")
#' Pima_data=Pima_data[complete.cases(Pima_data), ]
#' color_scale=c("red", "darkgrey", "navy")
#'
#' histogram_plot_JH(Pima_data$glucose, na.rm = FALSE, n_bins=30, plot_density=TRUE,
#'                             color_scale=color_scale)
#' histogram_plot_JH(Pima_data$glucose, na.rm = FALSE, n_bins=30, plot_density=FALSE)
#'
histogram_plot_JH = function(
  data,
  na.rm = FALSE,
  n_bins = 30,
  plot_density = TRUE,
  color_scale = NULL
) {
  data = as.data.frame(data)
  x_var = colnames(data)[1]

  if (plot_density == TRUE) {
    density_object = stats::density(data[, 1], na.rm = na.rm)
    density_data = data.frame(x = density_object$x, y = density_object$y)

    if (!is.null(color_scale)) {
      histogram_density_plot = ggplot2::ggplot(data) +
        ggplot2::geom_histogram(
          ggplot2::aes(
            x = .data[[x_var]],
            y = ggplot2::after_stat(density)
          ),
          bins = n_bins,
          color = color_scale[1],
          fill = color_scale[2]
        ) +
        ggplot2::geom_line(
          data = density_data,
          ggplot2::aes(x = x, y = y),
          color = color_scale[3],
          linewidth = 1
        )
    } else {
      histogram_density_plot = ggplot2::ggplot(data) +
        ggplot2::geom_histogram(
          ggplot2::aes(
            x = .data[[x_var]],
            y = ggplot2::after_stat(density)
          ),
          bins = n_bins,
          color = "black"
        ) +
        ggplot2::geom_line(
          data = density_data,
          ggplot2::aes(x = x, y = y),
          color = "red",
          linewidth = 1
        )
    }
  } else {
    if (!is.null(color_scale)) {
      histogram_density_plot = ggplot2::ggplot(data) +
        ggplot2::geom_histogram(
          ggplot2::aes(
            x = .data[[x_var]],
            y = ggplot2::after_stat(density)
          ),
          bins = n_bins,
          color = color_scale[1],
          fill = color_scale[2]
        )
    } else {
      histogram_density_plot = ggplot2::ggplot(data) +
        ggplot2::geom_histogram(
          ggplot2::aes(
            x = .data[[x_var]],
            y = ggplot2::after_stat(density)
          ),
          bins = n_bins,
          color = "black"
        )
    }
  }

  histogram_density_plot = histogram_density_plot

  return(histogram_density_plot)
}
