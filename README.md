

<!-- README.md is generated from README.qmd. Please edit that file -->

# spinviz <img src="man/figures/spinviz_logo.png" align="right" height="110"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/thomas-fung/spinviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomas-fung/spinviz/actions/workflows/R-CMD-check.yaml)
[![Ask
DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/bnqcasimiro/spinviz)
[![Codecov test
coverage](https://codecov.io/gh/thomas-fung/spinviz/graph/badge.svg)](https://app.codecov.io/gh/thomas-fung/spinviz)
<!-- badges: end -->

## About

The `spinviz` packages provides tools for visualising sporting injury
frequencies as a **heatmap projected onto human-body SVG diagrams** in
`R`.

The main function, `heatmap_diagram()`, colours anatomical regions by
injury frequency and can render **front**, **back**, or **both** views,
using **male** or **female** body templates.

The package also produces interactive **sunburst diagrams** of injury
tissue/pathology classifications via `sunburst_diagram_echarts()`,
built-in category taxonomies (`body_categories`, `injury_categories`)
with one-call convenience wrappers (`heatmap_diagram_default()`,
`sunburst_diagram_default()`), and `save_diagram()` for exporting either
diagram type to file at the correct aspect ratio.

## Examples

**Heatmap diagram**

<details>

<summary>

<b><code>Example Diagrams</code></b>
</summary>

| Front (Male) | Back (Male) | Both Views (Male) |
|----|----|----|
| <img src="man/figures/injury-heatmap-front.png" height="300" /> | <img src="man/figures/injury-heatmap-back.png" height="300" /> | <img src="man/figures/injury-heatmap-both.png" height="300" /> |

</details>

**Sunburst diagram**

<details>

<summary>

<b><code>Example Diagram</code></b>
</summary>

| Tissue/Pathology Sunburst                                   |
|-------------------------------------------------------------|
| <img src="man/figures/sunburst-example.png" height="400" /> |

</details>

## Features

### Injury Heatmap

The heatmap uses **ggplot2** to display a body diagram with regions
coloured by injury frequency.

Available options:

- View: `"front"`, `"back"`, or `"both"`
- Diagram sex: `"male"` or `"female"`
- Opacity control
- Optional labels (region names) and/or values
- Optional legend scale
- Custom palettes:
  - Named palettes supported by `diagram_colours()` (viridis or HCL
    palettes)
  - Or a custom vector of hex colours
- Rows with an `"Unspecified"` subcategory are shown as a label below
  the diagram (they have no body region to colour)

### Injury Sunburst

The sunburst uses **echarts4r** to display an interactive two-ring
diagram: tissue types on the inner ring, pathologies on the outer ring.

Available options:

- Colour per tissue type, with pathology slices automatically lightened
- Optional exclusion of “Unspecified”/“Non-specific” tissue rows
- `depth = 1` to show only the tissue-level ring
- Label/leader-line, radius, font and title tuning
- Custom palettes as above

### Convenience Wrappers and Taxonomies

If you use the package’s standard classifications, you don’t need to
retype them:

- `body_categories`: the 19-row `region_area`/`subcategory` taxonomy
  used by the heatmap
- `injury_categories`: the 25-row `tissue`/`pathology` taxonomy used by
  the sunburst
- `heatmap_diagram_default(counts, view_choice)`: plot from just a
  vector of counts (matched by row position to `body_categories`)
- `sunburst_diagram_default(counts)`: same idea for the sunburst

### Saving Diagrams

`save_diagram(plot, file, width = ...)` detects whether `plot` is a
heatmap or a sunburst and applies the matching export logic, deriving
the other dimension from the correct width:height ratio automatically.
Heatmaps are saved via `ggplot2::ggsave()` (PNG, PDF, SVG, JPG, …);
sunbursts via a headless Chromium browser through the optional
`chromote` package (PNG, JPG, PDF).

## Installation

As this package is not currently on CRAN, install from GitHub:

``` r
# install.packages("pak")
pak::pak("bnqcasimiro/spinviz")
```

## Getting Started

The available functions in this package are:

- `heatmap_diagram()`: render injury heatmaps on body SVG diagrams
- `sunburst_diagram_echarts()`: render interactive tissue/pathology
  sunburst diagrams
- `heatmap_diagram_default()`, `sunburst_diagram_default()`: one-call
  wrappers using the built-in taxonomies
- `body_categories`, `injury_categories`: built-in region and
  tissue/pathology taxonomies
- `save_diagram()`: export either diagram type to file at the correct
  aspect ratio
- `create_injury_template()`: write a template CSV in the expected data
  format
- `read_injury_data()`: read and validate an injury-data CSV file
- `diagram_colours()`: return colours from supported palette names
- `test_colour()`: visualise palettes (named or custom)

### Data Format

This package expects data to be provided in a data frame, with columns
in the following order:

- region_area
- subcategory
- Sport column containing injury frequencies

#### Heatmap Diagram (Body Injuries)

| region_area   | subcategory | sport1 |
|---------------|-------------|--------|
| Head and Neck | Head        | 10     |
| Head and Neck | Neck        | 20     |
| Upper Limb    | Shoulder    | 5      |
| Upper Limb    | Upper Arm   | 8      |

#### Sunburst Diagram (Tissue/Pathology)

For the sunburst, the first two columns are `tissue` and `pathology`,
followed by one or more sport columns:

| tissue          | pathology      | sport1 |
|-----------------|----------------|--------|
| Muscle / Tendon | Muscle strain  | 20     |
| Muscle / Tendon | Tendon rupture | 10     |
| Bone            | Fracture       | 16     |
| Bone            | Bone contusion | 6      |

### Importing Data from a CSV File

If your injury data lives in a CSV file, the package provides helpers
for the full workflow: create a template, fill it in, read it back, and
plot.

**1. Create a template CSV** with all 18 recognised body subcategories
and one empty column per sport:

``` r
create_injury_template("injuries.csv", sports = c("boxing", "judo"))
```

**2. Fill in the file** in your favourite spreadsheet editor, entering
the injury frequencies and the `region_area` grouping for each row.

**3. Read the file back in.** `read_injury_data()` checks that the
required columns are present, that at least one sport column exists, and
that sport values are numeric (with a warning for values that fail to
parse):

``` r
df <- read_injury_data("injuries.csv")
```

**4. Plot as usual:**

``` r
heatmap_diagram(df, "boxing", "front", sex = "male")
```

### Example Code

#### Heatmap Example

Start by creating a data frame:

``` r
subcategory <- c("Head","Neck","Shoulder","Chest","Upper Arm","Elbow",
                 "Abdomen","Forearm","Hip Groin","Wrist","Hand",
                 "Thigh","Knee","Lower Leg","Ankle","Foot","Thoracic Spine","Lumbosacral")

region_area <- rep("Example", length(subcategory))
boxing <- c(15, 5, 18, 12, 20, 6, 10, 14, 9, 9, 11, 3, 16, 13, 7, 8, 18, 22)

df <- data.frame(region_area, subcategory, boxing)
```

Then run `heatmap_diagram(injury_data, selected_sport, view_choice)`
where:

- `injury_data`: the data to be used, i.e. the data frame above
- `selected_sport`: the column name to use
- `view_choice`: the view of the diagram. One of: `"front"`, `"back"`,
  `"both"`

``` r
heatmap_diagram(df, "boxing", "front", sex = "male", show_scale = FALSE)
```

<details>

<summary>

<b><code>Example Code Run</code></b>
</summary>

<img src="man/figures/injury-heatmap-front.png" height="600" />
</details>

Run `?heatmap_diagram` for more detail.

#### Sunburst Example

The sunburst expects `tissue` and `pathology` columns followed by sport
column(s). The quickest way to get a correctly-ordered taxonomy is the
built-in `injury_categories`:

``` r
df <- injury_categories
df$boxing <- c(20, 0, 0, 10, 31, 18, 16, 16, 20, 20, 27, 46,
               67, 31, 54, 20, 27, 30, 96, 82, 48, 26, 33, 34, 24)

p <- sunburst_diagram_echarts(df, "boxing", plot_title = "Boxing Injuries")
p
```

<details>

<summary>

<b><code>Example Output</code></b>
</summary>

<img src="man/figures/sunburst-example.png" height="500" />
</details>

Or skip the data frame entirely with the wrapper:

``` r
sunburst_diagram_default(df$boxing, plot_title = "Boxing Injuries")
```

Run `?sunburst_diagram_echarts` for more detail.

#### Saving Diagrams

`save_diagram()` works with both diagram types and derives the height
from the width automatically:

``` r
p_heat <- heatmap_diagram(df, "boxing", "front", sex = "male")
save_diagram(p_heat, file = "heatmap.png", width = 1600, units = "px")

p_sun <- sunburst_diagram_echarts(df, "boxing")
save_diagram(p_sun, file = "sunburst.png", width = 1200)  # requires the chromote package
```

### Palette Support

You can supply palette as:

- A palette name supported by `diagram_colours()` (viridis or HCL),
  e.g. `"magma"`, `"plasma"`, `"Reds"`
- A character vector of hex colours,
  e.g. `c("#FFFFFF", "#FFFF00", "#FF0000")`

To preview supported palettes, run `?diagram_colours` and
`?test_colour`.

### Dependencies

Key packages used:

- Data wrangling: `dplyr`, `tidyr`, `rlang`, `stringr`
- SVG handling: `xml2`
- Iteration/utilities: `purrr`, `magrittr`
- Raster + plotting: `magick`, `ggplot2`, `grDevices`
- Combining plots: `patchwork`
- Interactive sunbursts: `echarts4r`, `htmlwidgets`
- Optional (sunburst file export only): `chromote`, `base64enc`
