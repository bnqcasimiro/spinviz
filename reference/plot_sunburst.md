# Sunburst diagram

Creates a two-level sunburst diagram (Tissue type -\> Pathology type)
using the echarts4r package, with a dark-to-light colour scheme (dark
inner ring per tissue, lighter shades for its pathology children), a
colour-coded legend for tissue type, and an optional toggle to exclude
"Unspecified" tissue entries.

The diagram can be hovered over for more granular detail. Inner and
outer ring labels can be formatted independently.

## Usage

``` r
plot_sunburst(
  data,
  column_name = "",
  replace_names = "",
  depth = -1,
  palette = "Dark 3",
  transparency = 1,
  plot_title = "",
  inner_font_size = 12,
  inner_font_style = "normal",
  inner_font_color = "#FFFFFF",
  inner_font_family = "Arial",
  outer_font_size = 12,
  outer_font_style = "normal",
  outer_font_color = "#333333",
  outer_font_family = "Arial",
  title_size = 18,
  title_color = "#333333",
  title_x = "center",
  title_y = "20%",
  title_family = "Arial",
  include_non_specific = TRUE,
  inner_radius = "10%",
  mid_radius = "40%",
  outer_radius = "50%",
  label_line_length = 40,
  label_line_length2 = 80,
  min_label_angle = 2,
  outer_label_width = 90,
  show_outer_labels = TRUE,
  widget_width = "1200px",
  widget_height = "1500px",
  warnings = "text"
)
```

## Arguments

- data:

  (Required) A data frame with the Tissue type, Pathology type, and
  sporting injury counts. Expected to follow the Olympic Committee
  standards of reporting epidemiological data on injury and illness in
  sport. Expects at least 3 columns; must be in the following order, but
  column name is not important:

  1: Tissue, in string format, e.g. "Muscle/tendon"

  2: Pathology type, in string format, e.g. "Muscle Injury"

  3...n: Injury frequency, positive integer.

  Rather than typing out the Tissue/Pathology columns yourself, you can
  start from the built-in
  [injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md)
  taxonomy (which already has columns 1 and 2 filled in with the
  standard classification) and just add your own injury counts as
  column 3. See
  [`plot_sunburst_default()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst_default.md).

- column_name:

  A string of the name of the column that contains the injury frequency.
  Will use the 3rd column if unspecified.

- replace_names:

  A vector of strings that contains the original and replacement values
  of tissue/pathology names, e.g. c("Muscle/tendon"="Muscle"). Applied
  to the data before the tree is built.

- depth:

  An integer: 1 shows tissue-level rings only, anything else (default
  -1) shows both tissue and pathology rings.

- palette:

  A string or vector of hex colours - the BASE (tissue-level) colour
  palette. Can be a value from hcl.pals()/viridis, or a vector of hex
  values, one per tissue type (excluding "Unspecified", which is always
  grey). Pathology (outer ring) colours are automatically derived as
  lightened versions of their parent tissue's colour.

- transparency:

  A number between 0 and 1 for diagram opacity. Default 1.

- plot_title:

  A string title for the diagram.

- inner_font_size:

  Numeric font size for inner-ring tissue labels. Default 12.

- inner_font_style:

  Font style for inner-ring tissue labels. Accepts "normal" or "italic".
  Default "normal".

- inner_font_color:

  Text colour for inner-ring tissue labels, specified as a colour name
  or hexadecimal value. Default "#FFFFFF" (white) for improved contrast
  against darker tissue colours.

- inner_font_family:

  Font family for inner-ring tissue labels. Default "Arial".

- outer_font_size:

  Numeric font size for outer-ring pathology labels. Default 12.

- outer_font_style:

  Font style for outer-ring pathology labels. Accepts "normal" or
  "italic". Default "normal".

- outer_font_color:

  Text colour for outer-ring pathology labels, specified as a colour
  name or hexadecimal value. Default "#333333" (dark grey). Can be
  changed independently of the inner-ring text colour.

- outer_font_family:

  Font family for outer-ring pathology labels. Default "Arial".

- title_size, title_color, title_x, title_y, title_family:

  Text/position styling for the plot title. title_x/title_y accept
  ECharts position keywords ("left"/"center"/"right",
  "top"/"middle"/"bottom") or px/%.

- include_non_specific:

  Logical toggle. If FALSE, rows where Tissue is "Unspecified" are
  excluded from the diagram entirely. If TRUE (default), they are
  included and always coloured grey, regardless of `palette`.

- inner_radius, mid_radius, outer_radius:

  Strings (ECharts radius units, e.g. "15%") controlling, respectively:
  the size of the blank centre hole, the outer edge of the tissue
  ring/inner edge of the pathology ring, and the outer edge of the
  pathology ring. Defaults: "10%", "40%", "50%".

- label_line_length, label_line_length2:

  Numbers (pixels) controlling the two segments of the leader line
  connecting each outer-ring label back to its slice:
  `label_line_length` is the segment hugging the slice edge,
  `label_line_length2` is the segment running out to the label text.
  Increase these (and/or shrink `outer_radius`) to pull labels further
  out and reduce crowding. Defaults: 40, 80.

- min_label_angle:

  Minimum slice angle in degrees for displaying outer-ring pathology
  labels. Default 2.

- outer_label_width:

  Maximum width in pixels for outer-ring pathology labels before text
  wrapping. Default 90.

- show_outer_labels:

  Logical. If `FALSE`, hides the outer-ring (pathology) labels and their
  leader lines entirely. The slices themselves still render and are
  still hoverable via tooltip. If `TRUE` (default), labels show as
  usual.

- widget_width, widget_height:

  Dimensions of the rendered diagram, specified as CSS size strings.
  Defaults: "1200px" and "1500px".

- warnings:

  A string to determine how the program displays warnings: default
  "text" for text only, pass `warnings = "all"` for pop-up + text.

## Value

An echarts4r htmlwidget.

## See also

[`save_diagram()`](https://bnqcasimiro.github.io/spinviz/reference/save_diagram.md)
to export this chart directly to a specific file type at a chosen size.

## Examples

``` r
# Easiest: start from the built-in injury_categories taxonomy
# (Tissue/Pathology columns are already filled in) and add your
# own injury counts. See ?injury_categories for the category order.
df <- injury_categories

df$boxing <- c(
  10, 19, 19, 41, 31, 18, 16, 16, 10, 10,
  27, 46, 67, 31, 54, 10, 27, 10, 96, 82,
  48, 26, 33, 34, 24
)

df$judo <- c(
  25, 45, 37, 31, 19, 41, 48, 14, 49, 26,
  42, 24, 14, 16, 37, 41, 23, 36, 33, 44,
  48, 41, 31, 25, 48
)

p1 <- plot_sunburst(df, "boxing")
p2 <- plot_sunburst(df, "judo")

# Customise inner and outer label formatting independently
p_custom <- plot_sunburst(
  df,
  "boxing",
  palette = "Dark 3",
  inner_font_size = 14,
  inner_font_style = "normal",
  inner_font_color = "#FFFFFF",
  inner_font_family = "Arial",
  outer_font_size = 11,
  outer_font_style = "italic",
  outer_font_color = "#222222",
  outer_font_family = "Arial"
)

# Alternatively, supply the complete Tissue and Pathology names
# directly. Categories that are not required can be omitted.
# This example excludes the unspecified injury category.
Tissue <- c(
  "Muscle/Tendon", "Muscle/Tendon", "Muscle/Tendon",
  "Muscle/Tendon", "Muscle/Tendon",
  "Nervous", "Nervous",
  "Bone", "Bone", "Bone", "Bone", "Bone",
  "Cartilage/Synovium/Bursa", "Cartilage/Synovium/Bursa",
  "Cartilage/Synovium/Bursa", "Cartilage/Synovium/Bursa",
  "Ligament/Joint capsule", "Ligament/Joint capsule",
  "Superficial tissues/skin", "Superficial tissues/skin",
  "Superficial tissues/skin",
  "Vessels",
  "Stump",
  "Internal organs"
)

Pathology <- c(
  "Muscle injury",
  "Muscle contusion",
  "Muscle compartment syndrome",
  "Tendinopathy",
  "Tendon rupture",
  "Brain/Spinal cord injury",
  "Peripheral nerve Injury",
  "Fracture",
  "Bone stress injury",
  "Bone contusion",
  "Avascular necrosis",
  "Physis injury",
  "Cartilage injury",
  "Arthritis",
  "Synovitis/Capsulitis",
  "Bursitis",
  "Joint sprain (ligament tear or acute instability episode)",
  "Chronic instability",
  "Contusion (superficial)",
  "Laceration",
  "Abrasion",
  "Vascular trauma",
  "Stump injury",
  "Organ trauma"
)

boxing2 <- c(
  10, 19, 19, 41, 31, 18, 16, 16, 10, 10,
  27, 46, 67, 31, 54, 10, 27, 10, 96, 82,
  48, 26, 33, 34
)

df2 <- data.frame(
  Tissue,
  Pathology,
  boxing = boxing2
)

p3 <- plot_sunburst(df2, "boxing")
```
