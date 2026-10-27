# Regenerate the example figures shown in README.qmd (man/figures/).
# Run from the package root with the package loaded, e.g.:
#   devtools::load_all()
#   source("data-raw/readme-figures.R")

library(spinviz)

# Heatmap figures ---------------------------------------------------------

# Same example counts as the README/plot_heatmap() examples
df_heatmap <- body_categories
df_heatmap$boxing <- c(
  15,
  5,
  18,
  20,
  6,
  14,
  9,
  11,
  12,
  18,
  22,
  10,
  9,
  16,
  13,
  7,
  8,
  3,
  24
)

figure_height_px <- 1400

save_diagram(
  plot_heatmap(df_heatmap, "boxing", "anterior", sex = "male", show_legend = FALSE),
  "man/figures/injury-heatmap-front.png",
  height = figure_height_px
)

save_diagram(
  plot_heatmap(df_heatmap, "boxing", "posterior", sex = "male", show_legend = FALSE),
  "man/figures/injury-heatmap-back.png",
  height = figure_height_px
)

save_diagram(
  plot_heatmap(df_heatmap, "boxing", "both", sex = "male", show_legend = FALSE),
  "man/figures/injury-heatmap-both.png",
  height = figure_height_px
)

# Sunburst figure ---------------------------------------------------------

# Requires chromote (+ a Chrome install) for the screenshot export.
df_sunburst <- data.frame(
  tissue_type = c(
    "Muscle/tendon",
    "Muscle/tendon",
    "Nervous",
    "Bone",
    "Bone",
    "Cartilage/Synovium/Bursa",
    "Ligament/Joint capsule",
    "Superficial tissues/skin",
    "Vessels",
    "Internal organs"
  ),
  pathology_type = c(
    "Muscle injury",
    "Tendon rupture",
    "Peripheral nerve injury",
    "Fracture",
    "Bone contusion",
    "Cartilage injury",
    "Joint sprain",
    "Laceration",
    "Vascular trauma",
    "Organ trauma"
  ),
  injuries = c(20, 10, 16, 16, 10, 67, 27, 82, 26, 34)
)

if (requireNamespace("chromote", quietly = TRUE)) {
  save_diagram(
    plot_sunburst(df_sunburst, "injuries"),
    "man/figures/sunburst-example.png",
    width = 1500
  )
} else {
  message(
    "Skipping sunburst-example.png: package 'chromote' is not installed."
  )
}
