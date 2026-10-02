# Create a template CSV file for sunburst injury data

Writes a CSV file in the format expected by
[`sunburst_diagram_echarts()`](https://bnqcasimiro.github.io/spinviz/reference/sunburst_diagram_echarts.md):
one row per entry in the built-in
[injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md)
taxonomy, with `tissue` and `pathology` columns and one empty column per
sport for the user to fill in with injury frequencies.

[injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md)
is the single source of truth for the taxonomy, so the template, the
reader's validation, and the diagram cannot drift apart.

## Usage

``` r
create_sunburst_template(path, sports = "sport1")
```

## Arguments

- path:

  File path where the CSV should be written.

- sports:

  Character vector of sport names; each becomes an empty frequency
  column in the template.

## Value

The path (invisibly), so the call can be used in a pipe.

## See also

[`read_sunburst_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_sunburst_data.md)
to read a completed file back in,
[`create_injury_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_injury_template.md)
for the heatmap equivalent.

## Examples

``` r
path <- tempfile(fileext = ".csv")
create_sunburst_template(path, sports = c("boxing", "judo"))
```
