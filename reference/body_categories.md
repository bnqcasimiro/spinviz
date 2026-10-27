# Standard Body region/Body area injury taxonomy

The standard Body region/Body area classification used by
[`plot_heatmap()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap.md),
with no injury counts attached. Combine it with your own counts via
[`plot_heatmap_default()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap_default.md)
rather than retyping the full body_region/body_area list for every
analysis.

## Usage

``` r
body_categories
```

## Format

A data frame with 19 rows and 2 columns:

- body_region:

  Body region, e.g. "Upper limb", "Trunk".

- body_area:

  Body area, e.g. "Shoulder", "Chest".

## Details

**Row order matters.**
[`plot_heatmap_default()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap_default.md)
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
    13. Lower limb    -- Hip / groin
    14. Lower limb    -- Thigh
    15. Lower limb    -- Knee
    16. Lower limb    -- Lower leg
    17. Lower limb    -- Ankle
    18. Lower limb    -- Foot
    19. Unspecified   -- Unspecified

## See also

[`plot_heatmap_default()`](https://bnqcasimiro.github.io/spinviz/reference/plot_heatmap_default.md)
to build a heatmap diagram from this taxonomy plus your own injury
counts, without needing to retype the body_region/body_area labels
yourself.
[injury_categories](https://bnqcasimiro.github.io/spinviz/reference/injury_categories.md)
for the equivalent taxonomy used by the sunburst functions.
