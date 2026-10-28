# Create a template CSV file for injury data

Writes a CSV file in the format expected by
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md):
one row per body area in the
[body_categories](https://bnqcasimiro.github.io/spinviz/reference/body_categories.md)
taxonomy (including "Unspecified", which
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
reports as a count below the diagram), with `body_region` and
`body_area` columns pre-filled and one empty column per sport for the
user to fill in with injury frequencies.

## Usage

``` r
create_injury_template(path, sports = "sport1")
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

[`read_injury_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_injury_data.md)
to read a completed file back in.

## Examples

``` r
path <- tempfile(fileext = ".csv")
create_injury_template(path, sports = c("boxing", "judo"))
#> Error in create_injury_template(path, sports = c("boxing", "judo")): unused argument (sports = c("boxing", "judo"))
```
