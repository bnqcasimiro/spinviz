# Get started with spinviz

``` r

library(spinviz)
```

`spinviz` produces two kinds of injury visualisations:

- **Heatmaps** of injury frequency projected onto human-body diagrams
  ([`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)),
  with front, back, or both views and male or female body templates.
- **Interactive sunburst diagrams** of tissue_type/pathology_type
  classifications
  ([`plot_sunburst()`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md)),
  built on echarts4r.

## Data format

The package expects a data frame with two descriptor columns followed by
one or more columns of injury frequencies (one per sport or cohort).

### Heatmap data

| body_region   | body_area | sport1 |
|---------------|-----------|--------|
| Head and neck | Head      | 10     |
| Head and neck | Neck      | 20     |
| Upper limb    | Shoulder  | 5      |
| Upper limb    | Upper arm | 8      |

The 18 recognised body areas and their `body_region` groupings, plus a
special `Unspecified` category for injuries that cannot be localised,
are available as the built-in `body_categories` taxonomy (19 rows).

### Sunburst data

For the sunburst, the first two columns are `tissue_type` and
`pathology_type`:

| tissue_type     | pathology_type | sport1 |
|-----------------|----------------|--------|
| Muscle / Tendon | Muscle strain  | 20     |
| Muscle / Tendon | Tendon rupture | 10     |
| Bone            | Fracture       | 16     |
| Bone            | Bone contusion | 6      |

The built-in `injury_categories` taxonomy contains the 25-row
tissue_type/pathology_type classification.

## Heatmap example

``` r

body_area <- c("Head", "Neck", "Shoulder", "Chest", "Upper arm", "Elbow",
                 "Abdomen", "Forearm", "Hip / groin", "Wrist", "Hand",
                 "Thigh", "Knee", "Lower leg", "Ankle", "Foot",
                 "Thoracic spine", "Lumbosacral")

body_region <- rep("Example", length(body_area))
boxing <- c(15, 5, 18, 12, 20, 6, 10, 14, 9, 9, 11, 3, 16, 13, 7, 8, 18, 22)

df <- data.frame(body_region, body_area, boxing)
```

Then call `plot_heatmap(injury_data, selected_sport, view_choice)`,
where `view_choice` is one of `"front"`, `"back"`, or `"both"`:

``` r

plot_heatmap(df, "boxing", "front", sex = "male", show_scale = FALSE)
```

![](spinviz_files/figure-html/unnamed-chunk-4-1.png)

See
[`?plot_heatmap`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md)
for options controlling labels, values, opacity, the legend scale, and
custom palettes. Rows with an `"Unspecified"` body_area are shown as a
label below the diagram (they have no body region to colour).

## Sunburst example

The quickest way to get a correctly-ordered taxonomy is
`injury_categories`:

``` r

df_sb <- injury_categories
df_sb$boxing <- c(20, 0, 0, 10, 31, 18, 16, 16, 20, 20, 27, 46,
                  67, 31, 54, 20, 27, 30, 96, 82, 48, 26, 33, 34, 24)

plot_sunburst(
  df_sb
)
```

The sunburst is interactive: tissue types sit on the inner ring,
pathologies on the outer ring. See
[`?plot_sunburst`](https://bnqcasimiro.github.io/spinviz/reference/plot_sunburst.md)
for options such as `depth = 1` (tissue_type ring only), excluding
“Unspecified”/“Non-specific” rows, and label/radius/font tuning.

## Convenience wrappers

If you use the package’s standard classifications, you can skip
constructing the descriptor columns entirely and pass just a vector of
counts:

``` r

plot_heatmap_default(c(boxing, unspecified = 4), "front")
```

![](spinviz_files/figure-html/unnamed-chunk-7-1.png)

``` r

plot_sunburst_default(df_sb$boxing)
```

Counts are matched by row position to `body_categories` /
`injury_categories`.

## Next steps

- [Importing CSV
  data](https://bnqcasimiro.github.io/spinviz/articles/importing-csv-data.html)
  — a template-based CSV workflow
- [Customisation](https://bnqcasimiro.github.io/spinviz/articles/customisation.html)
  — palettes, saving diagrams, and display options
