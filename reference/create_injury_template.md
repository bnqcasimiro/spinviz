# Create a template CSV file for injury data

Writes a CSV file in the format expected by
[`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md):
one row per recognised body subcategory, with `region_area` and
`subcategory` columns and one empty column per sport for the user to
fill in with injury frequencies.

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

[`read_injury_data()`](https://thomas-fung.github.io/spinviz/reference/read_injury_data.md)
to read a completed file back in.

## Examples

``` r
path <- tempfile(fileext = ".csv")
create_injury_template(path, sports = c("boxing", "judo"))
```
