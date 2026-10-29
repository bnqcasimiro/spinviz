# Sunburst diagram using the built-in injury taxonomy

A thin wrapper around
[`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md)
for using the standard tissue_type/pathology_type taxonomy
([injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md))
and plugging in a set of injury counts.

## Usage

``` r
plot_sunburst_default(counts, group = "injury_count", ...)
```

## Arguments

- counts:

  A numeric vector of exactly `nrow(injury_categories)` values
  (currently 25), in the same row order as
  [injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md),
  i.e. `counts[i]` is the injury count for
  `injury_categories[i, c("tissue_type", "pathology_type")]`.

- group:

  A string used as the injury-count column's name when building the
  underlying data frame. Default "injury_count".

- ...:

  Passed straight through to
  [`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md).
  Every other argument (`palette`, `plot_title`, `include_non_specific`,
  radius/label tuning, etc.) works exactly the same way.

## Value

An echarts4r htmlwidget, identical to what
[`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md)
itself returns.

## See also

[injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md)
for the taxonomy this merges your counts into.
[`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md)
for the full version that accepts any tissue_type/pathology_type
taxonomy, if you need something other than the built-in one (i.e. a
different classification system).

## Examples

``` r
boxing <- c(20,0,0,10,31,18,16,16,20,20,27,46,67,31,54,20,27,30,96,82,48,26,33,34,24)
p1 <- plot_sunburst_default(boxing, plot_title = "Boxing Injuries")
```
