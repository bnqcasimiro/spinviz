# Build the `tissue_df` example dataset used in the plot_sunburst()
# examples: the injury_categories taxonomy plus boxing and judo injury
# counts. Run from the package root:
#   source("data-raw/tissue_df.R")

tissue_df <- spinviz::injury_categories

tissue_df$boxing <- c(
  10,
  19,
  19,
  41,
  31,
  18,
  16,
  16,
  10,
  10,
  27,
  46,
  67,
  31,
  54,
  10,
  27,
  10,
  96,
  82,
  48,
  26,
  33,
  34,
  24
)

tissue_df$judo <- c(
  25,
  45,
  37,
  31,
  19,
  41,
  48,
  14,
  49,
  26,
  42,
  24,
  14,
  16,
  37,
  41,
  23,
  36,
  33,
  44,
  48,
  41,
  31,
  25,
  48
)

usethis::use_data(tissue_df, overwrite = TRUE)
