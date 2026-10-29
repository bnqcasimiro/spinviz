# Workshop materials

## Downloads

Materials for the spinviz workshop, covering the full workflow from
loading data to saving a body heatmap and a tissue/pathology sunburst:

- [Workshop slides
  (PDF)](https://bnqcasimiro.github.io/spinviz/workshop/spinviz_workshop_slides.pdf)
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
[`create_injury_template()`](https://bnqcasimiro.github.io/spinviz/reference/create_injury_template.md).
You can either:

- Download a CSV and read it in with
  [`read_injury_data()`](https://bnqcasimiro.github.io/spinviz/reference/read_injury_data.md),
  or
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
for creating and filling in your
