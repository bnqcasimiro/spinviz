# Save a spinviz diagram to file

Saves either kind of plot this package produces: a heatmap from
[`heatmap_diagram`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
or a sunburst from
[`sunburst_diagram_echarts`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md),
detecting which one `plot` is from its class and applying the matching
export logic and width:height ratio.


    p1 <- heatmap_diagram(df, "boxing", "front", sex = "male")
    save_diagram(p1, file = "heatmap.png", width = 1600, units = "px")

    p2 <- sunburst_diagram_echarts(df, "boxing")
    save_diagram(p2, file = "sunburst.jpg", width = 1500)

**Supply `width` OR `height`** for either diagram type, whichever you
don't supply is derived from the relevant fixed ratio so the diagram
keeps its proportions. If you supply both and they don't match that
ratio: for heatmaps, see `enforce_ratio`; for sunbursts, both are used
exactly as given (no enforcement).

**Export format** is set by the extension in `file`. Heatmaps go through
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html), which
accepts any format it supports (.png, .pdf, .svg, .jpg/.jpeg,
.tiff/.tif, .bmp, ...). Sunbursts are restricted to
.jpg/.jpeg/.png/.pdf, via `chromote` directly (Chrome's
`Page.captureScreenshot` for raster, `Page.printToPDF` for PDF.

**How each type's ratio is derived** (kept separate, since the two
diagrams aren't built the same way):

- **Heatmap**: the body diagram panel itself is always drawn at a fixed
  height:width of 1.1:1 (`theme(aspect.ratio = 1.1)` inside
  [`heatmap_diagram`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)).
  What varies is extra width outside that panel: labels/values reserve a
  data-width of 1.35 vs. 1.00 with neither shown (exact, from
  `coord_cartesian(xlim = ...)`), the legend adds an estimated ~15%
  extra width if shown (**not** measured from a real render, a
  placeholder worth checking against actual output), and
  `view_choice = "both"` uses the combined front+gap+back width (1 :
  0.10 : 1, exact, from `plot_layout(widths = ...)`).

- **Sunburst**: no fixed ratio is computed at all.
  [`sunburst_diagram_echarts`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md)
  records the exact `widget_width`/`widget_height` it was built with,
  and that recorded ratio is scaled directly.

## Usage

``` r
save_diagram(
  plot,
  file,
  width = NULL,
  height = NULL,
  view_choice = NULL,
  show_labels = NULL,
  show_values = NULL,
  show_scale = NULL,
  units = "px",
  dpi = 300,
  enforce_ratio = TRUE,
  bg = "white",
  delay = 1,
  ...
)
```

## Arguments

- plot:

  The plot/widget object returned by
  [`heatmap_diagram`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
  or
  [`sunburst_diagram_echarts`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md).

- file:

  Output file path. See Description for which extensions are valid for
  each diagram type.

- width, height:

  Size of the saved file. Supply at least one, the other is derived from
  the relevant ratio. Both default to `NULL`.

- view_choice:

  **Heatmap only.** One of `"front"`, `"back"`, or `"both"`. Left as
  `NULL` (default) to auto-detect from `plot`'s recorded metadata.

- show_labels, show_values, show_scale:

  **Heatmap only.** Logical, or `NULL` (default) to auto-detect from
  `plot`'s recorded metadata.

- units:

  **Heatmap only.** Units for `width`/`height`: `"px"` (default),
  `"in"`, `"cm"`, or `"mm"`.

- dpi:

  **Heatmap only.** Resolution in dots per inch, for raster formats only
  (no effect on .pdf/.svg/.eps). Default 300.

- enforce_ratio:

  **Heatmap only.** Logical. If `TRUE` (default) and you supply both
  `width` and `height` with a mismatched ratio, `height` is recalculated
  to match and a warning explains why. Set `FALSE` to deliberately
  stretch the diagram.

- bg:

  **Heatmap only.** Background colour. Default `"white"`.
  [`heatmap_diagram`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md)
  uses `theme_void()`, which has no background fill at all, and many
  image viewers render that transparency as solid grey rather than
  white. Set to `NA` for a genuinely transparent export.

- delay:

  **Sunburst only.** Seconds to wait after the page loads before
  capturing, giving ECharts time to finish drawing (especially its label
  layout pass). Default 1.

- ...:

  **Heatmap only.** Further arguments passed to
  [`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Value

Invisibly, the file path written to.

## See also

[`heatmap_diagram`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md),
[`sunburst_diagram_echarts`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_echarts.md)

## Examples

``` r
subcategory <- c("Head","Neck","Shoulder","Chest","Upper Arm","Elbow",
                  "Abdomen","Forearm","Hip Groin","Wrist","Hand",
                  "Thigh","Knee","Lower Leg","Ankle","Foot","Thoracic Spine","Lumbosacral")
region_area <- rep("Example", length(subcategory))
boxing <- c(15, 5, 18, 12, 20, 6, 10, 14, 9, 9, 11, 3, 16, 13, 7, 8, 18, 22)
df_heatmap <- data.frame(region_area, subcategory, boxing)

tissue <- c('Muscle/tendon','Muscle/tendon','Nervous','Bone','Bone',
            'Cartilage/Synovium/Bursa','Ligament/Joint capsule',
            'Superficial tissues/skin','Vessels','Internal organs')
pathology <- c('Muscle injury','Tendon rupture','Peripheral nerve injury',
               'Fracture','Bone contusion','Cartilage injury','Joint sprain',
               'Laceration','Vascular trauma','Organ trauma')
injuries <- c(20, 10, 16, 16, 10, 67, 27, 82, 26, 34)
df_sunburst <- data.frame(tissue, pathology, injuries)

if (FALSE) { # \dontrun{
# Heatmap: type detected automatically, ratio comes from heatmap_diagram()'s
# own recorded metadata (view_choice, show_labels, show_values, show_scale)
p1 <- heatmap_diagram(df_heatmap, "boxing", "front", sex = "male")
save_diagram(p1, file = "heatmap.png", width = 1600, units = "px")

# "both" view, exported as PDF
p2 <- heatmap_diagram(df_heatmap, "boxing", "both", sex = "male")
save_diagram(p2, file = "heatmap_both.pdf", width = 2400, units = "px")

# Sunburst: ratio comes from sunburst_diagram_echarts()'s own recorded
# widget_width/widget_height instead; no units argument needed (always px)
p3 <- sunburst_diagram_echarts(df_sunburst, "injuries")
save_diagram(p3, file = "sunburst.jpg", width = 1500)

# Sunburst as PDF: same call shape again, just a different extension
save_diagram(p3, file = "sunburst.pdf", width = 1500)

# Overriding recorded metadata explicitly (e.g. plot came from an older
# version of heatmap_diagram() with no attached metadata)
save_diagram(p1, file = "heatmap_custom.png",
             view_choice = "front", show_labels = TRUE,
             show_values = FALSE, show_scale = TRUE,
             width = 1200, units = "px")
} # }
```
