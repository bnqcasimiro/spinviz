# Heat map of Injuries on Human Body

Creates a heat map projected onto a diagram of the human body, based on
the number of injuries on body parts visible on the diagram. Diagrams
can be viewed from the front, back, or both views

## Usage

``` r
heatmap_diagram(
  injury_data,
  selected_sport,
  view_choice,
  sex = c("male", "female"),
  palette = NULL,
  opacity = 0.8,
  show_labels = TRUE,
  show_values = TRUE,
  show_scale = TRUE
)
```

## Arguments

- injury_data:

  a data frame with the body region, area the injury occurred, and the
  frequency of the injury

- selected_sport:

  the name of the sport, corresponding to the header of the frequency
  column in the data frame.

- view_choice:

  which view to render the diagram. One of ("front", "back", or "both")

- sex:

  which body diagram to use: `"male"` or `"female"`.

- palette:

  Optional colour palette.

  - A single palette name supported by
    [`diagram_colours`](https://bnqcasimiro.github.io/spinviz/reference/diagram_colours.md)
    (viridis or HCL), e.g. `"magma"` or `"Reds"`.

  - A character vector of hex colours, e.g. `c("#FFFFFF", "#FF0000")`.

  - HCL palette names can be listed with
    [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html).

- opacity:

  Numeric between `0` and `1`. Opacity of the filled body regions.

- show_labels:

  Logical. If `TRUE`, show region names.

- show_values:

  Logical. If `TRUE`, show injury values.

- show_scale:

  Logical. If `TRUE`, show the colour scale (legend).

## Value

A plot in R-studio viewer

## See also

[`body_categories`](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md)
for the built-in region_area/subcategory taxonomy, and
[`heatmap_diagram_default`](https://bnqcasimiro.github.io/spinviz/reference/heatmap_diagram_default.md)
for a wrapper that only needs a vector of counts.
[`diagram_colours`](https://bnqcasimiro.github.io/spinviz/reference/diagram_colours.md)
to preview supported palette names.
[`test_colour`](https://bnqcasimiro.github.io/spinviz/reference/test_colour.md)
to visualise palettes.
[`save_diagram`](https://bnqcasimiro.github.io/spinviz/reference/save_diagram.md)
to save the plot to file with the correct width:height ratio
automatically applied.

## Examples

``` r
# Easiest: start from the built-in body_categories taxonomy (its
# region_area/subcategory columns are already filled in) and just add
# your own counts, matching its row order. See ?body_categories, or
# just run print(body_categories) to see the order directly.
df <- body_categories
df$boxing <- c(15, 5, 18, 20, 6, 14, 9, 11, 12, 18, 22, 10, 9, 16, 13, 7, 8, 3, 24)
# Generate a plot for front view, male
p1 <- heatmap_diagram(df, "boxing", "front", sex = "male", show_values = FALSE)
# You can customise the colour palette by:
p2 <- heatmap_diagram(df, "boxing", "front", sex = "female", palette = "plasma")
# You can add your own plot title by:
p2 + ggplot2::labs(title = "Boxing Injury Heatmap (Front)")
```
