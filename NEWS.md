# kyplus 0.1.2

## New Features
* Implemented non-interrupting project creation functionality
* Added automated package installation system
* Created comprehensive project structure templates
* Introduced CPU core optimization for package installation

## Improvements
* Streamlined project loading process
* Enhanced configuration management
* Added detailed documentation

## Initial Setup
* Established basic package structure
* Created core functionality
* Added testing framework

## how to use color palettes
library(kyplus)
library(ggplot2)

# View all available palettes
show_goldfish_palettes()

# Get colors from specific palette
goldfish_colors("balanced")        # All colors
goldfish_colors("warm", n = 20)     # 5 colors
goldfish_colors("sunset", reverse = TRUE)  # Reversed colors

# Use in ggplot2 - categorical data
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_goldfish("bright")

# Use in ggplot2 - continuous data
ggplot(mtcars, aes(x = mpg, y = hp, color = wt)) +
  geom_point(size = 3) +
  scale_color_goldfish_c("balanced")
