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
read_injury_data(path)
#>      body_region      body_area boxing
#> 1  Head and neck           Head     NA
#> 2  Head and neck           Neck     NA
#> 3     Upper limb       Shoulder     NA
#> 4     Upper limb      Upper arm     NA
#> 5     Upper limb          Elbow     NA
#> 6     Upper limb        Forearm     NA
#> 7     Upper limb          Wrist     NA
#> 8     Upper limb           Hand     NA
#> 9          Trunk          Chest     NA
#> 10         Trunk Thoracic spine     NA
#> 11         Trunk    Lumbosacral     NA
#> 12         Trunk        Abdomen     NA
#> 13    Lower limb    Hip / groin     NA
#> 14    Lower limb          Thigh     NA
#> 15    Lower limb           Knee     NA
#> 16    Lower limb      Lower leg     NA
#> 17    Lower limb          Ankle     NA
#> 18    Lower limb           Foot     NA
#> 19   Unspecified    Unspecified     NA
```
