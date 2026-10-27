# Importing data from a CSV file

If your injury data lives in a CSV file, `spinviz` provides helpers for
the full workflow: create a template, fill it in, read it back, and
plot.

## Heatmap (body injury) data

**1. Create a template CSV** with all 18 recognised body areas and one
empty column per sport:

``` r

create_injury_template("injuries.csv", sports = c("boxing", "judo"))
```

**2. Fill in the file** in your favourite spreadsheet editor, entering
the injury frequencies and the `body_region` grouping for each row.

**3. Read the file back in.**
[`read_injury_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_injury_data.md)
checks that the required columns are present, that at least one sport
column exists, and that sport values are numeric (with a warning for
values that fail to parse):

``` r

df <- read_injury_data("injuries.csv")
```

**4. Plot as usual:**

``` r

plot_heatmap(df, "boxing", "anterior", sex = "male")
```

## Sunburst (tissue_type/pathology_type) data

The same workflow exists for the sunburst format.
[`create_sunburst_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_sunburst_template.md)
writes a CSV pre-filled with the 25-row `injury_categories` taxonomy
(`tissue_type` and `pathology_type` columns), and
[`read_sunburst_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_sunburst_data.md)
validates it — including filling blank `tissue_type` cells down from the
row above, so compact hand-edited files work:

``` r

create_sunburst_template("tissue_injuries.csv", sports = c("boxing", "judo"))
df <- read_sunburst_data("tissue_injuries.csv")
plot_sunburst(df, "boxing")
```
