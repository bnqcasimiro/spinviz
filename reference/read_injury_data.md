# Read and validate injury data from a CSV file

Reads a CSV file (e.g. one created with
[`create_injury_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_injury_template.md)
and filled in) and checks that it has the structure
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
expects: `body_region` and `body_area` columns plus at least one sport
column of injury frequencies.

## Usage

``` r
read_injury_data(path)
```

## Arguments

- path:

  File path of the CSV to read.

## Value

A data frame with `body_region`, `body_area`, and one column per sport
(coerced to numeric).

## See also

[`create_injury_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_injury_template.md)
to generate a correctly formatted file.

## Examples

``` r
path <- tempfile(fileext = ".csv")
create_injury_template(path, sports = "boxing")
#> Error in create_injury_template(path, sports = "boxing"): unused argument (sports = "boxing")
read_injury_data(path)
#> Error: File does not exist: /tmp/Rtmpdbj6x5/file1de65482684f.csv
```
