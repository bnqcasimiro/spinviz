# Heatmap diagram using the built-in body region taxonomy

A thin wrapper around
[`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
for using the standard tissue/pathology taxonomy
([body_categories](https://thomas-fung.github.io/spinviz/reference/body_categories.md))
and plugging in a set of injury counts.

## Usage

``` r
heatmap_diagram_default(
  counts,
  view_choice,
  selected_sport = "injury_count",
  sex = c("male", "female"),
  ...
)
```

## Arguments

- counts:

  A numeric vector of exactly `nrow(body_categories)` values (currently
  19), in the same row order as
  [body_categories](https://thomas-fung.github.io/spinviz/reference/body_categories.md),
  i.e. `counts[i]` is the injury count for
  `body_categories[i, c("region_area", "subcategory")]`. See
  [`?body_categories`](https://thomas-fung.github.io/spinviz/reference/body_categories.md)
  for the full numbered row order, or just run `print(body_categories)`
  directly.

- view_choice:

  which view to render the diagram. One of ("front", "back", or "both"),
  same as
  [`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md).

- selected_sport:

  A string used as the injury-count column's name when building the
  underlying data frame. Default "injury_count".

- sex:

  which body diagram to use: `"male"` or `"female"`.

- ...:

  Passed straight through to
  [`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md).
  Every other argument (`palette`, `opacity`, `show_labels`,
  `show_values`, `show_scale`, etc.) works exactly the same way.

## Value

A plot in R-studio viewer, identical to what
[`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
itself returns.

## See also

[body_categories](https://thomas-fung.github.io/spinviz/reference/body_categories.md)
for the taxonomy this merges your counts into.
[`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
for the full version that accepts any region_area/subcategory taxonomy,
if you need something other than the built-in one.
[`save_diagram()`](https://thomas-fung.github.io/spinviz/reference/save_diagram.md)
to save the result to file.

## Examples

``` r
boxing <- c(1, 12, 24, 20, 11, 12, 8, 5, 40, 32, 12, 28, 20, 15, 12, 35, 18, 5, 0)
p1 <- heatmap_diagram_default(boxing, "front", sex = "male")
```
