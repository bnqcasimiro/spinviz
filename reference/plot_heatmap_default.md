# Heatmap diagram using the built-in body region taxonomy

A thin wrapper around
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
for using the standard tissue/pathology taxonomy
([body_categories](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md))
and plugging in a set of injury counts.

## Usage

``` r
plot_heatmap_default(
  counts,
  view,
  group = "injury_count",
  sex = c("male", "female"),
  ...
)
```

## Arguments

- counts:

  A numeric vector of exactly `nrow(body_categories)` values (currently
  19), in the same row order as
  [body_categories](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md),
  i.e. `counts[i]` is the injury count for
  `body_categories[i, c("body_region", "body_area")]`. See
  [`?body_categories`](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md)
  for the full numbered row order, or just run `print(body_categories)`
  directly.

- view:

  which view to render the diagram. One of ("anterior", "posterior", or
  "both"), same as
  [`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md).

- group:

  A string used as the injury-count column's name when building the
  underlying data frame. Default "injury_count".

- sex:

  which body diagram to use: `"male"` or `"female"`.

- ...:

  Passed straight through to
  [`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md).
  Every other argument (`palette`, `opacity`, `show_labels`,
  `show_values`, `show_legend`, etc.) works exactly the same way.

## Value

A plot in R-studio viewer, identical to what
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
itself returns.

## See also

[body_categories](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md)
for the taxonomy this merges your counts into.
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
for the full version that accepts any body_region/body_area taxonomy, if
you need something other than the built-in one.
[`save_diagram()`](https://bnqcasimiro.github.io/spinviz/reference/save_diagram.md)
to save the result to file.

## Examples

``` r
boxing <- c(1, 12, 24, 20, 11, 12, 8, 5, 40, 32, 12, 28, 20, 15, 12, 35, 18, 5, 0)
p1 <- plot_heatmap_default(boxing, "anterior", sex = "male")
```
