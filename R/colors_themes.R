#### FUNCTIONS IN THIS SCRIPT

# theme_JH
# theme_publication
# distinct_scale_set1
# distinct_scale_dark
# distinct_scale_bar
# paired_scale

#####################################################################################
# theme_JH
#####################################################################################

#' A theme for ggplot2
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return A theme for ggplot2
#' @export
#' @examples
#' data(Pima_data)
#' library(ggplot2)
#' ggplot(Pima_data) + geom_point(aes(x=bmi,y=glucose)) + ggtitle("Plot title") + theme_JH()
#'
theme_JH <- function(base_size = 14, base_family = "sans") {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = base_family,
        face = "bold",
        color = 'black',
        hjust = 0.5,
        vjust = 0.5,
        size = ggplot2::rel(1.25)
      ),
      panel.background = ggplot2::element_rect(
        fill = '#f9f9f9',
        colour = 'white'
      ),
      axis.title = ggplot2::element_text(
        family = base_family,
        face = "bold",
        size = ggplot2::rel(1.1),
        hjust = 0.5
      ),
      axis.line = ggplot2::element_line(colour = "#747474"),
      legend.background = ggplot2::element_rect(
        fill = '#f9f9f9',
        colour = 'black'
      ),
      legend.text = ggplot2::element_text(family = base_family, hjust = 0.5),
      legend.title = ggplot2::element_text(
        family = base_family,
        face = "bold",
        size = ggplot2::rel(1.0),
        hjust = 0.5
      ),
      legend.key.size = grid::unit(base_size / 14, "lines"),
      plot.margin = ggplot2::margin(
        t = base_size * 0.5,
        r = base_size * 0.5,
        b = base_size * 0.5,
        l = base_size * 0.5,
        unit = "pt"
      )
    )
}

#####################################################################################
# theme_publication
#####################################################################################

#' A theme for ggplot2
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return A theme for ggplot2
#' @export
#' @examples
#' data(Pima_data)
#' library(ggplot2)
#' ggplot(Pima_data) + geom_point(aes(x=bmi,y=glucose)) + ggtitle("Plot title") + theme_publication()
#'
theme_publication <- function(base_size = 14, base_family = 'sans') {
  (ggthemes::theme_foundation(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.3),
        hjust = 0.5
      ),
      text = ggplot2::element_text(),
      panel.background = ggplot2::element_rect(colour = NA),
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.title.y = ggplot2::element_text(
        angle = 90,
        vjust = 2,
        face = "bold",
        size = ggplot2::rel(1.15)
      ),
      axis.title.x = ggplot2::element_text(
        vjust = -0.2,
        face = "bold",
        size = ggplot2::rel(1.15)
      ),
      axis.text = ggplot2::element_text(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = grid::unit(0.2, "cm"),
      legend.spacing = grid::unit(0, "cm"),
      legend.title = ggplot2::element_text(face = "italic"),
      plot.margin = grid::unit(c(10, 5, 5, 5), "mm"),
      strip.background = ggplot2::element_rect(
        colour = "#f0f0f0",
        fill = "#f0f0f0"
      ),
      strip.text = ggplot2::element_text(face = "bold")
    ))
}

#####################################################################################
# distinct_scale_set1
#####################################################################################

#' A nice set of colors for plots
#'
#' @return A nice set of colors for plots
#' @export
#' @examples
#' library(ggplot2)
#' data(color_scales_example_data)
#' color_scales_example_data=
#'   color_scales_example_data[color_scales_example_data$class%in%c(1:length(distinct_scale_set1())), ]
#' ggplot(color_scales_example_data) + geom_point(aes(x=Var_1,y=Var_2, color=class)) +
#'   scale_color_manual("Class", values=distinct_scale_set1())
#'
distinct_scale_set1 <- function() {
  return(
    distinct_scale_set1 = c(
      "#E41A1C",
      "#377EB8",
      "#4DAF4A",
      "#984EA3",
      "#FF7F00",
      "#A6CEE3",
      "#A65628",
      "#F781BF"
    )
  )
}

#####################################################################################
# distinct_scale_dark
#####################################################################################

#' A nice set of colors for plots
#'
#' @return A nice set of colors for plots
#' @export
#' @examples
#' library(ggplot2)
#' data(color_scales_example_data)
#' color_scales_example_data=
#'   color_scales_example_data[color_scales_example_data$class%in%c(1:length(distinct_scale_dark())), ]
#' ggplot(color_scales_example_data) + geom_point(aes(x=Var_1,y=Var_2, color=class)) +
#'   scale_color_manual("Class", values=distinct_scale_dark())
#'
distinct_scale_dark <- function() {
  return(c(
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  ))
}


#####################################################################################
# distinct_scale_bar
#####################################################################################

#' A nice set of colors for plots
#'
#' @return A nice set of colors for plots
#' @export
#' @examples
#' library(ggplot2)
#' data(color_scales_example_data)
#' color_scales_example_data=
#'   color_scales_example_data[color_scales_example_data$class%in%c(1:length(distinct_scale_bar())), ]
#' ggplot(color_scales_example_data) + geom_point(aes(x=Var_1,y=Var_2, color=class)) +
#'   scale_color_manual("Class", values=distinct_scale_bar())
#'
distinct_scale_bar <- function() {
  return(c("navy", "tomato", "forestgreen", "gold", "orange", "skyblue"))
}

#####################################################################################
# distinct_scale_chat_gpt
#####################################################################################

#' A nice set of colors for plots
#'
#' @return A nice set of colors for plots
#' @export
#' @examples
#' library(ggplot2)
#' data(color_scales_example_data)
#' color_scales_example_data=
#'   color_scales_example_data[
#'     color_scales_example_data$class%in%c(1:length(distinct_scale_chat_gpt())), ]
#' ggplot(color_scales_example_data) + geom_point(aes(x=Var_1,y=Var_2, color=class)) +
#'   scale_color_manual("Class", values=distinct_scale_chat_gpt())

distinct_scale_chat_gpt <- function() {
  return(c(
    "#386cb0",
    "#fdb462",
    "#7fc97f",
    "#ef3b2c",
    "#662506",
    "#a6cee3",
    "#fb9a99",
    "#984ea3",
    "#ffff33"
  ))
}


#####################################################################################
# paired_scale
#####################################################################################

#' A nice set of colors for plots
#'
#' @return A nice set of colors for plots
#' @export
#' @examples
#' library(ggplot2)
#' data(color_scales_example_data)
#' color_scales_example_data=
#'   color_scales_example_data[color_scales_example_data$class%in%c(1:length(paired_scale())), ]
#' ggplot(color_scales_example_data) + geom_point(aes(x=Var_1,y=Var_2, color=class)) +
#'   scale_color_manual("Class", values=paired_scale())
#'
paired_scale <- function() {
  return(c(
    "#A6CEE3",
    "#1F78B4",
    "#B2DF8A",
    "#33A02C",
    "#FB9A99",
    "#E31A1C",
    "#FDBF6F",
    "#FF7F00",
    "#CAB2D6",
    "#6A3D9A"
  ))
}
