# Customising and saving diagrams

## Colour palettes

Both diagram functions accept a palette as either:

- A palette name supported by
  [`diagram_colours()`](https://thomas-fung.github.io/spinviz/reference/diagram_colours.md)
  (viridis or HCL palettes), e.g. `"magma"`, `"plasma"`, `"Reds"`
- A character vector of hex colours,
  e.g. `c("#FFFFFF", "#FFFF00", "#FF0000")`

Run
[`?diagram_colours`](https://thomas-fung.github.io/spinviz/reference/diagram_colours.md)
to see supported palette names, and use
[`test_colour()`](https://thomas-fung.github.io/spinviz/reference/test_colour.md)
to visualise a named or custom palette before applying it.

``` r

test_colour("magma")
test_colour(c("#FFFFFF", "#FFFF00", "#FF0000"))
```

## Heatmap display options

[`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
supports:

- View: `"front"`, `"back"`, or `"both"`
- Diagram sex: `"male"` or `"female"`
- Opacity control
- Optional labels (region names) and/or values
- Optional legend scale

``` r

heatmap_diagram(df, "boxing", "both", sex = "female",
                opacity = 0.8, show_scale = TRUE)
```

See
[`?heatmap_diagram`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
for the full argument list.

## Sunburst display options

[`sunburst_diagram_echarts()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md)
supports:

- Colour per tissue type, with pathology slices automatically lightened
- Optional exclusion of “Unspecified”/“Non-specific” tissue rows
- `depth = 1` to show only the tissue-level ring
- Label/leader-line, radius, font and title tuning

``` r

sunburst_diagram_echarts(df_sb, "boxing", depth = 1,
                         plot_title = "Tissue types only")
```

See
[`?sunburst_diagram_echarts`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md)
for the full argument list.

## Saving diagrams

`save_diagram(plot, file, width = ...)` detects whether `plot` is a
heatmap or a sunburst and applies the matching export logic, deriving
the height from the width at the correct aspect ratio automatically:

``` r

p_heat <- heatmap_diagram(df, "boxing", "front", sex = "male")
save_diagram(p_heat, file = "heatmap.png", width = 1600, units = "px")

p_sun <- sunburst_diagram_echarts(df_sb, "boxing")
save_diagram(p_sun, file = "sunburst.png", width = 1200)
```

Heatmaps are saved via
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
(PNG, PDF, SVG, JPG, …). Sunbursts are saved via a headless Chromium
browser through the optional `chromote` package (PNG, JPG, PDF), so
install `chromote` and `base64enc` if you need sunburst file export.
