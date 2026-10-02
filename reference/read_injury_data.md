# Read and validate injury data from a CSV file

Reads a CSV file (e.g. one created with
[`create_injury_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_injury_template.md)
and filled in) and checks that it has the structure
[`heatmap_diagram()`](https://bnqcasimiro.github.io/spinviz/reference/heatmap_diagram.md)
expects: `region_area` and `subcategory` columns plus at least one sport
column of injury frequencies.

## Usage

``` r
read_injury_data(path)
```

## Arguments

- path:

  File path of the CSV to read.

## Value

A data frame with `region_area`, `subcategory`, and one column per sport
(coerced to numeric).

## See also

[`create_injury_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_injury_template.md)
to generate a correctly formatted file.

## Examples

``` r
path <- tempfile(fileext = ".csv")
create_injury_template(path, sports = "boxing")
read_injury_data(path)
#>    region_area    subcategory boxing
#> 1         <NA>           Head     NA
#> 2         <NA>           Neck     NA
#> 3         <NA>       Shoulder     NA
#> 4         <NA>      Upper Arm     NA
#> 5         <NA>          Elbow     NA
#> 6         <NA>        Forearm     NA
#> 7         <NA>          Wrist     NA
#> 8         <NA>           Hand     NA
#> 9         <NA>          Chest     NA
#> 10        <NA> Thoracic Spine     NA
#> 11        <NA>    Lumbosacral     NA
#> 12        <NA>        Abdomen     NA
#> 13        <NA>      Hip Groin     NA
#> 14        <NA>          Thigh     NA
#> 15        <NA>           Knee     NA
#> 16        <NA>      Lower Leg     NA
#> 17        <NA>          Ankle     NA
#> 18        <NA>           Foot     NA
```
