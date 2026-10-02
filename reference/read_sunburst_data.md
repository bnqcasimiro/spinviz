# Read and validate sunburst injury data from a CSV file

Reads a CSV file (e.g. one created with
[`create_sunburst_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_sunburst_template.md)
and filled in) and checks that it has the structure
[`sunburst_diagram_echarts()`](https://bnqcasimiro.github.io/spinviz/reference/sunburst_diagram_echarts.md)
expects: `tissue` and `pathology` columns plus at least one sport column
of injury frequencies.

Blank cells in the `tissue` column are filled with the last seen tissue
value (carry-down), matching the behaviour of
[`sunburst_diagram_echarts()`](https://bnqcasimiro.github.io/spinviz/reference/sunburst_diagram_echarts.md),
so compact hand-edited files are accepted.

## Usage

``` r
read_sunburst_data(path)
```

## Arguments

- path:

  File path of the CSV to read.

## Value

A data frame with `tissue`, `pathology`, and one column per sport
(coerced to numeric), ready to pass to
[`sunburst_diagram_echarts()`](https://bnqcasimiro.github.io/spinviz/reference/sunburst_diagram_echarts.md).

## See also

[`create_sunburst_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_sunburst_template.md)
to generate a correctly formatted file,
[`read_injury_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_injury_data.md)
for the heatmap equivalent.

## Examples

``` r
path <- tempfile(fileext = ".csv")
create_sunburst_template(path, sports = "boxing")
read_sunburst_data(path)
#>                  tissue                   pathology boxing
#> 1       Muscle / Tendon               Muscle strain     NA
#> 2       Muscle / Tendon            Muscle contusion     NA
#> 3       Muscle / Tendon        Compartment syndrome     NA
#> 4       Muscle / Tendon                Tendinopathy     NA
#> 5       Muscle / Tendon              Tendon rupture     NA
#> 6               Nervous Brain or spinal cord injury     NA
#> 7               Nervous     Peripheral nerve injury     NA
#> 8                  Bone                    Fracture     NA
#> 9                  Bone          Bone stress injury     NA
#> 10                 Bone              Bone contusion     NA
#> 11                 Bone          Avascular necrosis     NA
#> 12                 Bone               Physis injury     NA
#> 13 Cartilage / Synovium            Cartilage injury     NA
#> 14 Cartilage / Synovium                   Arthritis     NA
#> 15 Cartilage / Synovium      Synovitis / Capsulitis     NA
#> 16 Cartilage / Synovium                    Bursitis     NA
#> 17             Ligament                Joint sprain     NA
#> 18             Ligament         Chronic instability     NA
#> 19   Superficial tissue                   Contusion     NA
#> 20   Superficial tissue                  Laceration     NA
#> 21   Superficial tissue                    Abrasion     NA
#> 22               Vessel             Vascular trauma     NA
#> 23                Stump                Stump injury     NA
#> 24       Internal organ                Organ injury     NA
#> 25          Unspecified                 Unspecified     NA
```
