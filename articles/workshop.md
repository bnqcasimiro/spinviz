# Workshop materials

## Downloads

Materials for the spinviz workshop, covering the full workflow from
loading data to saving a body heatmap and a tissue/pathology sunburst:

- [Workshop slides
  (PDF)](https://bnqcasimiro.github.io/spinviz/workshop/spinviz_workshop_slides1.pdf)
- [R
  script](https://bnqcasimiro.github.io/spinviz/workshop/spinviz_workshop.R):
  step-by-step script with activities and answers
- [body_injuries_example.csv](https://bnqcasimiro.github.io/spinviz/workshop/body_injuries_example.csv):
  example data for
  [`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
  (boxing and judo)
- [tissue_injuries_example.csv](https://bnqcasimiro.github.io/spinviz/workshop/tissue_injuries_example.csv):
  example data for
  [`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md)
  (boxing and judo)

To download a file, right-click the link and choose **Save link as…**.

## Setup

Install spinviz from GitHub, then load it:

``` r

# install.packages("pak")
pak::pak("bnqcasimiro/spinviz")
library(spinviz)
```

Saving a sunburst as an image also needs the optional `chromote` and
`base64enc` packages, plus Google Chrome:

``` r

install.packages(c("chromote", "base64enc"))
```

## Loading the example data

The example CSV files follow the same layout as the templates created by
[`create_injury_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_injury_template.md)
or
[`create_sunburst_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_sunburst_template.md).
You can either:

- Download a CSV and read it in with
  [`read_injury_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_injury_data.md)
  or
  [`read_sunburst_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_sunburst_data.md)
- Use the built-in copies, `body_df` and `tissue_df`, which hold the
  same data

``` r

download.file(
  "https://bnqcasimiro.github.io/spinviz/workshop/body_injuries_example.csv",
  destfile = "body_injuries_example.csv"
)
body_data <- read_injury_data("body_injuries_example.csv")

download.file(
  "https://bnqcasimiro.github.io/spinviz/workshop/tissue_injuries_example.csv",
  destfile = "tissue_injuries_example.csv"
)
tissue_data <- read_sunburst_data("tissue_injuries_example.csv")

# Or skip the CSV files:
body_data <- body_df
tissue_data <- tissue_df
```

See [Importing data from a CSV
file](https://bnqcasimiro.github.io/spinviz/articles/importing-csv-data.md)
for creating and filling in your own templates.

## Quick start

``` r

plot_heatmap(data = body_data, group = "boxing", view = "both", sex = "male")
plot_sunburst(data = tissue_data, group = "judo", show_values = TRUE)
```

The R script walks through each argument with fill-in-the-blank
activities.

## Data source

The example data are competition injury counts from the Beijing 2008,
London 2012 and Rio de Janeiro 2016 Olympic Games:

Lystad RP, Alevras A, Rudy I, Soligard T, Engebretsen L. Injury
incidence, severity and profile in Olympic combat sports: a comparative
analysis of 7712 athlete exposures from three consecutive Olympic Games.
*Br J Sports Med* 2021;55:1077–1083.
[doi:10.1136/bjsports-2020-102958](https://doi.org/10.1136/bjsports-2020-102958)
