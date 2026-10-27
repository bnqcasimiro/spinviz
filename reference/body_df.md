# Example boxing injury counts by body area

An example dataset for
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md):
the standard
[body_categories](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md)
taxonomy with a column of (fictional) boxing injury counts attached.

## Usage

``` r
body_df
```

## Format

A data frame with 19 rows and 3 columns:

- body_region:

  Body region, e.g. "Upper limb", "Trunk".

- body_area:

  Body area, e.g. "Shoulder", "Chest".

- boxing:

  Number of recorded boxing injuries in that body area.

## Details

Row order matches
[body_categories](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md),
so the counts can be reused with other taxonomies built on the same
ordering.

## See also

[body_categories](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md)
for the taxonomy without counts,
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
to plot this dataset, and
[tissue_df](https://bnqcasimiro.github.io/spinviz/reference/tissue_df.md)
for the equivalent example dataset for
[`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md).

## Examples

``` r
plot_heatmap(body_df, "boxing", "anterior", sex = "male")
```
