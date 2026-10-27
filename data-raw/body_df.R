# Build the `body_df` example dataset used in the plot_heatmap()
# examples: the body_categories taxonomy plus boxing injury counts.
# Run from the package root:
#   source("data-raw/body_df.R")

body_df <- spinviz::body_categories
df$boxing <- c(
  20,
  NA,
  0,
  10,
  31,
  18,
  30,
  16,
  20,
  30,
  27,
  46,
  67,
  31,
  54,
  20,
  27,
  30,
  0
)

usethis::use_data(body_df, overwrite = TRUE)
