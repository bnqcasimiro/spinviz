# Read and validate sunburst injury data from a CSV file

Reads a CSV file (e.g. one created with
[`create_sunburst_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_sunburst_template.md)
and filled in) and checks that it has the structure
[`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md)
expects: `tissue_type` and `pathology_type` columns plus at least one
sport column of injury frequencies.

Blank cells in the `tissue_type` column are filled with the last seen
tissue_type value (carry-down), matching the behaviour of
[`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md),
so compact hand-edited files are accepted.

## Usage

``` r
read_sunburst_data(path)
```

## Arguments

- path:

  File path of the CSV to read.

## Value

A data frame with `tissue_type`, `pathology_type`, and one column per
sport (coerced to numeric), ready to pass to
[`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md).

## See also

[`create_sunburst_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_sunburst_template.md)
to generate a correctly formatted file,
[`read_injury_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_injury_data.md)
for the heatmap equivalent.

## Examples

``` r
path <- tempfile(fileext = ".csv")
create_sunburst_template(path, sports = "boxing")
#> Error in create_sunburst_template(path, sports = "boxing"): unused argument (sports = "boxing")
read_sunburst_data(path)
#> Error: File does not exist: /tmp/Rtmpdbj6x5/file1de6c2db510.csv
```
