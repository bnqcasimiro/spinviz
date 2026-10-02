# Standard body region/body area injury taxonomy

The standard body region/body area classification used by
[`heatmap_diagram()`](https://bnqcasimiro.github.io/spinviz/reference/heatmap_diagram.md),
with no injury counts attached. Combine it with your own counts via
[`heatmap_diagram_default()`](https://bnqcasimiro.github.io/spinviz/reference/heatmap_diagram_default.md)
rather than retyping the full region_area/subcategory list for every
analysis.

## Usage

``` r
body_categories
```

## Format

A data frame with 19 rows and 2 columns:

- region_area:

  Body region, e.g. "Upper limb", "Trunk".

- subcategory:

  Body area, e.g. "Shoulder", "Chest".

## Details

**Row order matters.**
[`heatmap_diagram_default()`](https://bnqcasimiro.github.io/spinviz/reference/heatmap_diagram_default.md)
(and the manual `df <- body_categories; df$my_counts <- counts` pattern)
line up your counts with this taxonomy purely by row position.
`counts[i]` is assumed to be the count for row `i`. The current order
(also always available by running `print(body_categories)` or
`View(body_categories)` directly, which is the authoritative source if
this list and the live object ever disagree) is:


     1. Head and neck -- Head
     2. Head and neck -- Neck
     3. Upper limb    -- Shoulder
     4. Upper limb    -- Upper arm
     5. Upper limb    -- Elbow
     6. Upper limb    -- Forearm
     7. Upper limb    -- Wrist
     8. Upper limb    -- Hand
     9. Trunk         -- Chest
    10. Trunk         -- Thoracic spine
    11. Trunk         -- Lumbosacral
    12. Trunk         -- Abdomen
    13. Lower limb    -- Hip Groin
    14. Lower limb    -- Thigh
    15. Lower limb    -- Knee
    16. Lower limb    -- Lower leg
    17. Lower limb    -- Ankle
    18. Lower limb    -- Foot
    19. Unspecified   -- Unspecified

## See also

[`heatmap_diagram_default()`](https://bnqcasimiro.github.io/spinviz/reference/heatmap_diagram_default.md)
to build a heatmap diagram from this taxonomy plus your own injury
counts, without needing to retype the region_area/subcategory labels
yourself.
[injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md)
for the equivalent taxonomy used by the sunburst functions.
