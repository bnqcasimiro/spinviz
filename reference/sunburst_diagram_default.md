# Sunburst diagram using the built-in injury taxonomy

A thin wrapper around
[`sunburst_diagram_echarts()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md)
for using the standard tissue/pathology taxonomy
([injury_categories](https://thomas-fung.github.io/spinviz/reference/injury_categories.md))
and plugging in a set of injury counts.

## Usage

``` r
sunburst_diagram_default(counts, column_name = "injury_count", ...)
```

## Arguments

- counts:

  A numeric vector of exactly `nrow(injury_categories)` values
  (currently 25), in the same row order as
  [injury_categories](https://thomas-fung.github.io/spinviz/reference/injury_categories.md),
  i.e. `counts[i]` is the injury count for
  `injury_categories[i, c("tissue", "pathology")]`.

- column_name:

  A string used as the injury-count column's name when building the
  underlying data frame. Default "injury_count".

- ...:

  Passed straight through to
  [`sunburst_diagram_echarts()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md).
  Every other argument (`palette`, `plot_title`, `include_non_specific`,
  radius/label tuning, etc.) works exactly the same way.

## Value

An echarts4r htmlwidget, identical to what
[`sunburst_diagram_echarts()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md)
itself returns.

## See also

[injury_categories](https://thomas-fung.github.io/spinviz/reference/injury_categories.md)
for the taxonomy this merges your counts into.
[`sunburst_diagram_echarts()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md)
for the full version that accepts any tissue/pathology taxonomy, if you
need something other than the built-in one (i.e. a different
classification system).

## Examples

``` r
boxing <- c(20,0,0,10,31,18,16,16,20,20,27,46,67,31,54,20,27,30,96,82,48,26,33,34,24)
p1 <- sunburst_diagram_default(boxing, plot_title = "Boxing Injuries")
```
