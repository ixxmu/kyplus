#' Goldfish Color Palettes
#'
#' A collection of goldfish-inspired color palettes for data visualization.
#' These palettes are designed to work well for both categorical and continuous variables.
#'
#' @details
#' Available palettes:
#' \itemize{
#'   \item \code{classic}: Original goldfish palette with aqua-to-orange gradient
#'   \item \code{warm}: Warm goldfish palette emphasizing yellow-orange tones
#'   \item \code{balanced}: Balanced palette from gray to deep goldfish colors
#'   \item \code{bright}: Bright and vibrant goldfish palette
#'   \item \code{sunset}: Sunset-inspired goldfish palette
#' }
#'
#' @name goldfish_palettes
NULL

# Define the goldfish color palettes
goldfish_palettes_list <- list(
  classic = c("#69d2e7", "#a7dbd8", "#e0e4cc", "#f38630", "#fa6900"),
  warm = c("#EA450B", "#FE5A1D", "#FFA324", "#FFB300", "#FFCF21"),
  balanced = c("#f5f5f5", "#e0e0e0", "#c8c8c8", "#FFB84D", "#FF8C42", "#E53E3E", "#B91C1C"),
  bright = c("#ffffff", "#ffe4b5", "#ffa324", "#ff8c00", "#ff4500", "#dc143c"),
  sunset = c("#fef9e7", "#fed976", "#fd8d3c", "#e6550d", "#a63603", "#7f2704")
)

#' Get Goldfish Color Palette
#'
#' Retrieve colors from goldfish-inspired palettes.
#'
#' @param palette Name of the palette. One of: "classic", "warm", "balanced", "bright", "sunset".
#' @param n Number of colors to return. If NULL, returns all colors in the palette.
#' @param type Type of palette usage. Either "discrete" for categorical data or "continuous" for continuous data.
#' @param reverse Logical, whether to reverse the order of colors.
#'
#' @return A character vector of hex color codes.
#'
#' @examples
#' # Get all colors from classic palette
#' goldfish_colors("classic")
#'
#' # Get 3 colors from warm palette
#' goldfish_colors("warm", n = 3)
#'
#' # Get colors for continuous scale
#' goldfish_colors("balanced", n = 10, type = "continuous")
#'
#' # Reverse the color order
#' goldfish_colors("sunset", reverse = TRUE)
#'
#' @export
goldfish_colors <- function(palette = "balanced", n = NULL, type = "discrete", reverse = FALSE) {

  # Check if palette exists
  if (!palette %in% names(goldfish_palettes_list)) {
    stop("Palette '", palette, "' not found. Available palettes: ",
         paste(names(goldfish_palettes_list), collapse = ", "))
  }

  # Get the palette colors
  colors <- goldfish_palettes_list[[palette]]

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # If n is NULL, return all colors
  if (is.null(n)) {
    return(colors)
  }

  # Handle different types
  if (type == "discrete") {
    # For discrete data, repeat or subset colors as needed
    if (n <= length(colors)) {
      return(colors[1:n])
    } else {
      # If more colors needed, interpolate
      return(colorRampPalette(colors)(n))
    }
  } else if (type == "continuous") {
    # For continuous data, interpolate between colors
    return(colorRampPalette(colors)(n))
  } else {
    stop("Type must be either 'discrete' or 'continuous'")
  }
}

#' Show Goldfish Color Palettes
#'
#' Display all available goldfish color palettes.
#'
#' @param palette Optional specific palette name to display. If NULL, shows all palettes.
#' @param n Number of colors to display per palette. Default is 7.
#'
#' @return A ggplot object showing the color palettes.
#'
#' @examples
#' \dontrun{
#' # Show all palettes
#' show_goldfish_palettes()
#'
#' # Show specific palette
#' show_goldfish_palettes("balanced")
#'
#' # Show with more colors
#' show_goldfish_palettes(n = 10)
#' }
#'
#' @export
show_goldfish_palettes <- function(palette = NULL, n = 7) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }

  # Select palettes to show
  if (is.null(palette)) {
    palettes_to_show <- names(goldfish_palettes_list)
  } else {
    if (!palette %in% names(goldfish_palettes_list)) {
      stop("Palette '", palette, "' not found. Available palettes: ",
           paste(names(goldfish_palettes_list), collapse = ", "))
    }
    palettes_to_show <- palette
  }

  # Create data for plotting
  plot_data <- data.frame()

  for (i in seq_along(palettes_to_show)) {
    pal_name <- palettes_to_show[i]
    colors <- goldfish_colors(pal_name, n = n, type = "continuous")

    temp_data <- data.frame(
      palette = pal_name,
      color = colors,
      x = 1:length(colors),
      y = i,
      stringsAsFactors = FALSE
    )

    plot_data <- rbind(plot_data, temp_data)
  }

  # Create the plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, fill = color)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      breaks = seq_along(palettes_to_show),
      labels = palettes_to_show,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:n,
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Goldfish Color Palettes",
      subtitle = paste("Displaying", n, "colors per palette"),
      x = "Color Index",
      y = "Palette"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(hjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  return(p)
}

#' Goldfish Color Scale for ggplot2 (Discrete)
#'
#' ggplot2 color scale using goldfish palettes for discrete/categorical variables.
#'
#' @param palette Name of the goldfish palette to use.
#' @param reverse Logical, whether to reverse the color order.
#' @param ... Additional arguments passed to \code{ggplot2::discrete_scale}.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # For color aesthetic
#' ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_goldfish()
#'
#' # For fill aesthetic
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_goldfish("warm")
#' }
#'
#' @export
scale_color_goldfish <- function(palette = "balanced", reverse = FALSE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }

  pal_func <- function(n) {
    goldfish_colors(palette, n = n, type = "discrete", reverse = reverse)
  }

  ggplot2::discrete_scale("colour", palette = pal_func, ...)
}

#' @rdname scale_color_goldfish
#' @export
scale_colour_goldfish <- scale_color_goldfish

#' @rdname scale_color_goldfish
#' @export
scale_fill_goldfish <- function(palette = "balanced", reverse = FALSE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }

  pal_func <- function(n) {
    goldfish_colors(palette, n = n, type = "discrete", reverse = reverse)
  }

  ggplot2::discrete_scale("fill", palette = pal_func, ...)
}

#' Goldfish Color Scale for ggplot2 (Continuous)
#'
#' ggplot2 color scale using goldfish palettes for continuous variables.
#'
#' @param palette Name of the goldfish palette to use.
#' @param reverse Logical, whether to reverse the color order.
#' @param ... Additional arguments passed to \code{ggplot2::scale_color_gradientn}.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # For continuous color scale
#' ggplot(mtcars, aes(x = mpg, y = hp, color = wt)) +
#'   geom_point() +
#'   scale_color_goldfish_c()
#'
#' # For continuous fill scale
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_goldfish_c("sunset")
#' }
#'
#' @export
scale_color_goldfish_c <- function(palette = "balanced", reverse = FALSE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }

  colors <- goldfish_colors(palette, reverse = reverse)

  ggplot2::scale_color_gradientn(colors = colors, ...)
}

#' @rdname scale_color_goldfish_c
#' @export
scale_colour_goldfish_c <- scale_color_goldfish_c

#' @rdname scale_color_goldfish_c
#' @export
scale_fill_goldfish_c <- function(palette = "balanced", reverse = FALSE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }

  colors <- goldfish_colors(palette, reverse = reverse)

  ggplot2::scale_fill_gradientn(colors = colors, ...)
}

#' Goldfish Color Scale for ggplot2 (Binned)
#'
#' ggplot2 color scale using goldfish palettes for binned continuous variables.
#'
#' @param palette Name of the goldfish palette to use.
#' @param reverse Logical, whether to reverse the color order.
#' @param ... Additional arguments passed to \code{ggplot2::scale_color_stepsn}.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # For binned color scale
#' ggplot(mtcars, aes(x = mpg, y = hp, color = wt)) +
#'   geom_point() +
#'   scale_color_goldfish_b()
#' }
#'
#' @export
scale_color_goldfish_b <- function(palette = "balanced", reverse = FALSE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }

  colors <- goldfish_colors(palette, reverse = reverse)

  ggplot2::scale_color_stepsn(colors = colors, ...)
}

#' @rdname scale_color_goldfish_b
#' @export
scale_colour_goldfish_b <- scale_color_goldfish_b

#' @rdname scale_color_goldfish_b
#' @export
scale_fill_goldfish_b <- function(palette = "balanced", reverse = FALSE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function")
  }

  colors <- goldfish_colors(palette, reverse = reverse)

  ggplot2::scale_fill_stepsn(colors = colors, ...)
}
