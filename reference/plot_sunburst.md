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
  opacity = 1,
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
  show_values = FALSE,
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

- opacity:

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

- show_values:

  Logical. If `TRUE`, each inner (tissue) and outer (pathology) label is
  followed by its injury count as "(*n* = ...)". If `FALSE` (default),
  labels show names only.

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
# Easiest: use the built-in tissue_df example dataset (the
# injury_categories taxonomy with boxing and judo injury counts).
# To use your own counts instead, start from the built-in
# injury_categories taxonomy (Tissue/Pathology columns are already
# filled in) and add your own counts. See ?injury_categories.
(p1 <- plot_sunburst(tissue_df, "boxing"))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"title":{"text":"","left":"center","top":"20%","textStyle":{"fontSize":18,"color":"#333333","fontFamily":"Arial"}},"legend":{"type":"scroll","orient":"vertical","right":10,"top":"middle","data":["Muscle / Tendon","Nervous","Bone","Cartilage / Synovium","Ligament","Superficial tissue","Vessel","Stump","Internal organ"]},"tooltip":{"trigger":"item","formatter":"{b}: {c}"},"toolbox":{"show":true,"feature":{"saveAsImage":{"show":true,"type":"jpeg","title":"Save as JPG","backgroundColor":"#ffffff"}}},"series":[{"type":"sunburst","radius":[0,"100%"],"sort":null,"nodeClick":false,"emphasis":{"label":{"show":false}},"data":[{"name":"Muscle / Tendon","value":120,"itemStyle":{"color":"#DD64BE","opacity":1},"children":[{"name":"Muscle strain","value":10,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Muscle contusion","value":19,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Compartment syndrome","value":19,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Tendinopathy","value":41,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Tendon rupture","value":31,"itemStyle":{"color":"#F0B9E2","opacity":1}}]},{"name":"Nervous","value":34,"itemStyle":{"color":"#B675E0","opacity":1},"children":[{"name":"Brain or spinal cord injury","value":18,"itemStyle":{"color":"#DEC1F1","opacity":1}},{"name":"Peripheral nerve injury","value":16,"itemStyle":{"color":"#DEC1F1","opacity":1}}]},{"name":"Bone","value":109,"itemStyle":{"color":"#5991E4","opacity":1},"children":[{"name":"Fracture","value":16,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Bone stress injury","value":10,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Bone contusion","value":10,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Avascular necrosis","value":27,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Physis injury","value":46,"itemStyle":{"color":"#B4CEF3","opacity":1}}]},{"name":"Cartilage / Synovium","value":162,"itemStyle":{"color":"#00A6CA","opacity":1},"children":[{"name":"Cartilage injury","value":67,"itemStyle":{"color":"#8CD7E7","opacity":1}},{"name":"Arthritis","value":31,"itemStyle":{"color":"#8CD7E7","opacity":1}},{"name":"Synovitis / Capsulitis","value":54,"itemStyle":{"color":"#8CD7E7","opacity":1}},{"name":"Bursitis","value":10,"itemStyle":{"color":"#8CD7E7","opacity":1}}]},{"name":"Ligament","value":37,"itemStyle":{"color":"#00AD9A","opacity":1},"children":[{"name":"Joint sprain","value":27,"itemStyle":{"color":"#8CDAD2","opacity":1}},{"name":"Chronic instability","value":10,"itemStyle":{"color":"#8CDAD2","opacity":1}}]},{"name":"Superficial tissue","value":226,"itemStyle":{"color":"#00AA5A","opacity":1},"children":[{"name":"Contusion","value":96,"itemStyle":{"color":"#8CD9B5","opacity":1}},{"name":"Laceration","value":82,"itemStyle":{"color":"#8CD9B5","opacity":1}},{"name":"Abrasion","value":48,"itemStyle":{"color":"#8CD9B5","opacity":1}}]},{"name":"Vessel","value":26,"itemStyle":{"color":"#6F9F00","opacity":1},"children":[{"name":"Vascular trauma","value":26,"itemStyle":{"color":"#BED48C","opacity":1}}]},{"name":"Stump","value":33,"itemStyle":{"color":"#AA9000","opacity":1},"children":[{"name":"Stump injury","value":33,"itemStyle":{"color":"#D9CD8C","opacity":1}}]},{"name":"Internal organ","value":34,"itemStyle":{"color":"#CE7D3B","opacity":1},"children":[{"name":"Organ injury","value":34,"itemStyle":{"color":"#E9C5A7","opacity":1}}]},{"name":"Unspecified","value":24,"itemStyle":{"color":"#B0B0B0","opacity":1},"children":[{"name":"Unspecified","value":24,"itemStyle":{"color":"#D4D4D4","opacity":1}}]}],"label":{"color":"#FFFFFF","fontSize":12,"fontFamily":"Arial","fontStyle":"normal","formatter":"{b}","rich":{"n":{"fontStyle":"italic","color":"#FFFFFF","fontSize":12,"fontFamily":"Arial"}}},"itemStyle":{"borderWidth":1,"borderColor":"#fff"},"levels":[[],{"r0":"10%","r":"40%","label":{"rotate":"radial","position":"inside","align":"center","verticalAlign":"middle","overflow":"truncate"}},{"r0":"40%","r":"50%","label":{"show":false}}]},{"type":"pie","radius":["40%","50%"],"center":["50%","50%"],"silent":true,"sort":null,"data":[{"name":"Muscle strain","value":10},{"name":"Muscle contusion","value":19},{"name":"Compartment syndrome","value":19},{"name":"Tendinopathy","value":41},{"name":"Tendon rupture","value":31},{"name":"Brain or spinal cord injury","value":18},{"name":"Peripheral nerve injury","value":16},{"name":"Fracture","value":16},{"name":"Bone stress injury","value":10},{"name":"Bone contusion","value":10},{"name":"Avascular necrosis","value":27},{"name":"Physis injury","value":46},{"name":"Cartilage injury","value":67},{"name":"Arthritis","value":31},{"name":"Synovitis / Capsulitis","value":54},{"name":"Bursitis","value":10},{"name":"Joint sprain","value":27},{"name":"Chronic instability","value":10},{"name":"Contusion","value":96},{"name":"Laceration","value":82},{"name":"Abrasion","value":48},{"name":"Vascular trauma","value":26},{"name":"Stump injury","value":33},{"name":"Organ injury","value":34},{"name":"Unspecified","value":24}],"itemStyle":{"color":"transparent","borderWidth":0},"emphasis":{"disabled":true},"avoidLabelOverlap":true,"minShowLabelAngle":2,"label":{"show":true,"position":"outside","color":"#333333","fontSize":12,"fontFamily":"Arial","fontStyle":"normal","overflow":"break","width":90,"formatter":"{b}","rich":{"n":{"fontStyle":"italic","color":"#333333","fontSize":12,"fontFamily":"Arial"}}},"labelLine":{"show":true,"length":40,"length2":80,"smooth":false,"lineStyle":{"width":1,"color":"#999999"}}}]},"dispose":true},"evals":[],"jsHooks":[]}(p2 <- plot_sunburst(tissue_df, "judo"))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"title":{"text":"","left":"center","top":"20%","textStyle":{"fontSize":18,"color":"#333333","fontFamily":"Arial"}},"legend":{"type":"scroll","orient":"vertical","right":10,"top":"middle","data":["Muscle / Tendon","Nervous","Bone","Cartilage / Synovium","Ligament","Superficial tissue","Vessel","Stump","Internal organ"]},"tooltip":{"trigger":"item","formatter":"{b}: {c}"},"toolbox":{"show":true,"feature":{"saveAsImage":{"show":true,"type":"jpeg","title":"Save as JPG","backgroundColor":"#ffffff"}}},"series":[{"type":"sunburst","radius":[0,"100%"],"sort":null,"nodeClick":false,"emphasis":{"label":{"show":false}},"data":[{"name":"Muscle / Tendon","value":157,"itemStyle":{"color":"#DD64BE","opacity":1},"children":[{"name":"Muscle strain","value":25,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Muscle contusion","value":45,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Compartment syndrome","value":37,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Tendinopathy","value":31,"itemStyle":{"color":"#F0B9E2","opacity":1}},{"name":"Tendon rupture","value":19,"itemStyle":{"color":"#F0B9E2","opacity":1}}]},{"name":"Nervous","value":89,"itemStyle":{"color":"#B675E0","opacity":1},"children":[{"name":"Brain or spinal cord injury","value":41,"itemStyle":{"color":"#DEC1F1","opacity":1}},{"name":"Peripheral nerve injury","value":48,"itemStyle":{"color":"#DEC1F1","opacity":1}}]},{"name":"Bone","value":155,"itemStyle":{"color":"#5991E4","opacity":1},"children":[{"name":"Fracture","value":14,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Bone stress injury","value":49,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Bone contusion","value":26,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Avascular necrosis","value":42,"itemStyle":{"color":"#B4CEF3","opacity":1}},{"name":"Physis injury","value":24,"itemStyle":{"color":"#B4CEF3","opacity":1}}]},{"name":"Cartilage / Synovium","value":108,"itemStyle":{"color":"#00A6CA","opacity":1},"children":[{"name":"Cartilage injury","value":14,"itemStyle":{"color":"#8CD7E7","opacity":1}},{"name":"Arthritis","value":16,"itemStyle":{"color":"#8CD7E7","opacity":1}},{"name":"Synovitis / Capsulitis","value":37,"itemStyle":{"color":"#8CD7E7","opacity":1}},{"name":"Bursitis","value":41,"itemStyle":{"color":"#8CD7E7","opacity":1}}]},{"name":"Ligament","value":59,"itemStyle":{"color":"#00AD9A","opacity":1},"children":[{"name":"Joint sprain","value":23,"itemStyle":{"color":"#8CDAD2","opacity":1}},{"name":"Chronic instability","value":36,"itemStyle":{"color":"#8CDAD2","opacity":1}}]},{"name":"Superficial tissue","value":125,"itemStyle":{"color":"#00AA5A","opacity":1},"children":[{"name":"Contusion","value":33,"itemStyle":{"color":"#8CD9B5","opacity":1}},{"name":"Laceration","value":44,"itemStyle":{"color":"#8CD9B5","opacity":1}},{"name":"Abrasion","value":48,"itemStyle":{"color":"#8CD9B5","opacity":1}}]},{"name":"Vessel","value":41,"itemStyle":{"color":"#6F9F00","opacity":1},"children":[{"name":"Vascular trauma","value":41,"itemStyle":{"color":"#BED48C","opacity":1}}]},{"name":"Stump","value":31,"itemStyle":{"color":"#AA9000","opacity":1},"children":[{"name":"Stump injury","value":31,"itemStyle":{"color":"#D9CD8C","opacity":1}}]},{"name":"Internal organ","value":25,"itemStyle":{"color":"#CE7D3B","opacity":1},"children":[{"name":"Organ injury","value":25,"itemStyle":{"color":"#E9C5A7","opacity":1}}]},{"name":"Unspecified","value":48,"itemStyle":{"color":"#B0B0B0","opacity":1},"children":[{"name":"Unspecified","value":48,"itemStyle":{"color":"#D4D4D4","opacity":1}}]}],"label":{"color":"#FFFFFF","fontSize":12,"fontFamily":"Arial","fontStyle":"normal","formatter":"{b}","rich":{"n":{"fontStyle":"italic","color":"#FFFFFF","fontSize":12,"fontFamily":"Arial"}}},"itemStyle":{"borderWidth":1,"borderColor":"#fff"},"levels":[[],{"r0":"10%","r":"40%","label":{"rotate":"radial","position":"inside","align":"center","verticalAlign":"middle","overflow":"truncate"}},{"r0":"40%","r":"50%","label":{"show":false}}]},{"type":"pie","radius":["40%","50%"],"center":["50%","50%"],"silent":true,"sort":null,"data":[{"name":"Muscle strain","value":25},{"name":"Muscle contusion","value":45},{"name":"Compartment syndrome","value":37},{"name":"Tendinopathy","value":31},{"name":"Tendon rupture","value":19},{"name":"Brain or spinal cord injury","value":41},{"name":"Peripheral nerve injury","value":48},{"name":"Fracture","value":14},{"name":"Bone stress injury","value":49},{"name":"Bone contusion","value":26},{"name":"Avascular necrosis","value":42},{"name":"Physis injury","value":24},{"name":"Cartilage injury","value":14},{"name":"Arthritis","value":16},{"name":"Synovitis / Capsulitis","value":37},{"name":"Bursitis","value":41},{"name":"Joint sprain","value":23},{"name":"Chronic instability","value":36},{"name":"Contusion","value":33},{"name":"Laceration","value":44},{"name":"Abrasion","value":48},{"name":"Vascular trauma","value":41},{"name":"Stump injury","value":31},{"name":"Organ injury","value":25},{"name":"Unspecified","value":48}],"itemStyle":{"color":"transparent","borderWidth":0},"emphasis":{"disabled":true},"avoidLabelOverlap":true,"minShowLabelAngle":2,"label":{"show":true,"position":"outside","color":"#333333","fontSize":12,"fontFamily":"Arial","fontStyle":"normal","overflow":"break","width":90,"formatter":"{b}","rich":{"n":{"fontStyle":"italic","color":"#333333","fontSize":12,"fontFamily":"Arial"}}},"labelLine":{"show":true,"length":40,"length2":80,"smooth":false,"lineStyle":{"width":1,"color":"#999999"}}}]},"dispose":true},"evals":[],"jsHooks":[]}
# Customise inner and outer label formatting independently
p_custom <- plot_sunburst(
  tissue_df,
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

(p3 <- plot_sunburst(df2, "boxing"))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"title":{"text":"","left":"center","top":"20%","textStyle":{"fontSize":18,"color":"#333333","fontFamily":"Arial"}},"legend":{"type":"scroll","orient":"vertical","right":10,"top":"middle","data":["Muscle/Tendon","Nervous","Bone","Cartilage/Synovium/Bursa","Ligament/Joint capsule","Superficial tissues/skin","Vessels","Stump","Internal organs"]},"tooltip":{"trigger":"item","formatter":"{b}: {c}"},"toolbox":{"show":true,"feature":{"saveAsImage":{"show":true,"type":"jpeg","title":"Save as JPG","backgroundColor":"#ffffff"}}},"series":[{"type":"sunburst","radius":[0,"100%"],"sort":null,"nodeClick":false,"emphasis":{"label":{"show":false}},"data":[{"name":"Muscle/Tendon","value":120,"itemStyle":{"color":"#DA65C3","opacity":1},"children":[{"name":"Muscle injury","value":10,"itemStyle":{"color":"#EEBAE4","opacity":1}},{"name":"Muscle contusion","value":19,"itemStyle":{"color":"#EEBAE4","opacity":1}},{"name":"Muscle compartment syndrome","value":19,"itemStyle":{"color":"#EEBAE4","opacity":1}},{"name":"Tendinopathy","value":41,"itemStyle":{"color":"#EEBAE4","opacity":1}},{"name":"Tendon rupture","value":31,"itemStyle":{"color":"#EEBAE4","opacity":1}}]},{"name":"Nervous","value":34,"itemStyle":{"color":"#A87BE4","opacity":1},"children":[{"name":"Brain/Spinal cord injury","value":18,"itemStyle":{"color":"#D8C4F3","opacity":1}},{"name":"Peripheral nerve Injury","value":16,"itemStyle":{"color":"#D8C4F3","opacity":1}}]},{"name":"Bone","value":109,"itemStyle":{"color":"#009ADE","opacity":1},"children":[{"name":"Fracture","value":16,"itemStyle":{"color":"#8CD2F0","opacity":1}},{"name":"Bone stress injury","value":10,"itemStyle":{"color":"#8CD2F0","opacity":1}},{"name":"Bone contusion","value":10,"itemStyle":{"color":"#8CD2F0","opacity":1}},{"name":"Avascular necrosis","value":27,"itemStyle":{"color":"#8CD2F0","opacity":1}},{"name":"Physis injury","value":46,"itemStyle":{"color":"#8CD2F0","opacity":1}}]},{"name":"Cartilage/Synovium/Bursa","value":162,"itemStyle":{"color":"#00AAB7","opacity":1},"children":[{"name":"Cartilage injury","value":67,"itemStyle":{"color":"#8CD9DF","opacity":1}},{"name":"Arthritis","value":31,"itemStyle":{"color":"#8CD9DF","opacity":1}},{"name":"Synovitis/Capsulitis","value":54,"itemStyle":{"color":"#8CD9DF","opacity":1}},{"name":"Bursitis","value":10,"itemStyle":{"color":"#8CD9DF","opacity":1}}]},{"name":"Ligament/Joint capsule","value":37,"itemStyle":{"color":"#00AC79","opacity":1},"children":[{"name":"Joint sprain (ligament tear or acute instability episode)","value":27,"itemStyle":{"color":"#8CDAC3","opacity":1}},{"name":"Chronic instability","value":10,"itemStyle":{"color":"#8CDAC3","opacity":1}}]},{"name":"Superficial tissues/skin","value":226,"itemStyle":{"color":"#50A315","opacity":1},"children":[{"name":"Contusion (superficial)","value":96,"itemStyle":{"color":"#B0D696","opacity":1}},{"name":"Laceration","value":82,"itemStyle":{"color":"#B0D696","opacity":1}},{"name":"Abrasion","value":48,"itemStyle":{"color":"#B0D696","opacity":1}}]},{"name":"Vessels","value":26,"itemStyle":{"color":"#9F9400","opacity":1},"children":[{"name":"Vascular trauma","value":26,"itemStyle":{"color":"#D4CF8C","opacity":1}}]},{"name":"Stump","value":33,"itemStyle":{"color":"#CB7F2F","opacity":1},"children":[{"name":"Stump injury","value":33,"itemStyle":{"color":"#E8C5A1","opacity":1}}]},{"name":"Internal organs","value":34,"itemStyle":{"color":"#E16A86","opacity":1},"children":[{"name":"Organ trauma","value":34,"itemStyle":{"color":"#F2BCC9","opacity":1}}]}],"label":{"color":"#FFFFFF","fontSize":12,"fontFamily":"Arial","fontStyle":"normal","formatter":"{b}","rich":{"n":{"fontStyle":"italic","color":"#FFFFFF","fontSize":12,"fontFamily":"Arial"}}},"itemStyle":{"borderWidth":1,"borderColor":"#fff"},"levels":[[],{"r0":"10%","r":"40%","label":{"rotate":"radial","position":"inside","align":"center","verticalAlign":"middle","overflow":"truncate"}},{"r0":"40%","r":"50%","label":{"show":false}}]},{"type":"pie","radius":["40%","50%"],"center":["50%","50%"],"silent":true,"sort":null,"data":[{"name":"Muscle injury","value":10},{"name":"Muscle contusion","value":19},{"name":"Muscle compartment syndrome","value":19},{"name":"Tendinopathy","value":41},{"name":"Tendon rupture","value":31},{"name":"Brain/Spinal cord injury","value":18},{"name":"Peripheral nerve Injury","value":16},{"name":"Fracture","value":16},{"name":"Bone stress injury","value":10},{"name":"Bone contusion","value":10},{"name":"Avascular necrosis","value":27},{"name":"Physis injury","value":46},{"name":"Cartilage injury","value":67},{"name":"Arthritis","value":31},{"name":"Synovitis/Capsulitis","value":54},{"name":"Bursitis","value":10},{"name":"Joint sprain (ligament tear or acute instability episode)","value":27},{"name":"Chronic instability","value":10},{"name":"Contusion (superficial)","value":96},{"name":"Laceration","value":82},{"name":"Abrasion","value":48},{"name":"Vascular trauma","value":26},{"name":"Stump injury","value":33},{"name":"Organ trauma","value":34}],"itemStyle":{"color":"transparent","borderWidth":0},"emphasis":{"disabled":true},"avoidLabelOverlap":true,"minShowLabelAngle":2,"label":{"show":true,"position":"outside","color":"#333333","fontSize":12,"fontFamily":"Arial","fontStyle":"normal","overflow":"break","width":90,"formatter":"{b}","rich":{"n":{"fontStyle":"italic","color":"#333333","fontSize":12,"fontFamily":"Arial"}}},"labelLine":{"show":true,"length":40,"length2":80,"smooth":false,"lineStyle":{"width":1,"color":"#999999"}}}]},"dispose":true},"evals":[],"jsHooks":[]}
```
