# Standard tissue/pathology injury taxonomy

The standard Tissue/Pathology classification used throughout this
package's sunburst and heatmap functions, with no injury counts
attached. Combine it with your own counts via
[`sunburst_diagram_default()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_default.md)
rather than retyping the full tissue/pathology category list for every
analysis.

## Usage

``` r
injury_categories
```

## Format

A data frame with 25 rows and 2 columns:

- tissue:

  Tissue type, e.g. "Bone", "Nervous".

- pathology:

  Pathology type, e.g. "Fracture", "Tendon rupture".

## Details

**Row order matters.**
[`sunburst_diagram_default()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_default.md)
(and the manual `df <- injury_categories; df$my_counts <- counts`
pattern) line up your counts with this taxonomy purely by row position.
`counts[i]` is assumed to be the count for row `i`. The current order
(also always available by running `print(injury_categories)` or
`View(injury_categories)` directly, which is the authoritative source if
this list and the live object ever disagree) is:


     1. Muscle / Tendon       -- Muscle strain
     2. Muscle / Tendon       -- Muscle contusion
     3. Muscle / Tendon       -- Compartment syndrome
     4. Muscle / Tendon       -- Tendinopathy
     5. Muscle / Tendon       -- Tendon rupture
     6. Nervous               -- Brain or spinal cord injury
     7. Nervous               -- Peripheral nerve injury
     8. Bone                  -- Fracture
     9. Bone                  -- Bone stress injury
    10. Bone                  -- Bone contusion
    11. Bone                  -- Avascular necrosis
    12. Bone                  -- Physis injury
    13. Cartilage / Synovium  -- Cartilage injury
    14. Cartilage / Synovium  -- Arthritis
    15. Cartilage / Synovium  -- Synovitis / Capsulitis
    16. Cartilage / Synovium  -- Bursitis
    17. Ligament              -- Joint sprain
    18. Ligament              -- Chronic instability
    19. Superficial tissue    -- Contusion
    20. Superficial tissue    -- Laceration
    21. Superficial tissue    -- Abrasion
    22. Vessel                -- Vascular trauma
    23. Stump                 -- Stump injury
    24. Internal organ        -- Organ injury
    25. Unspecified           -- Unspecified

## See also

[`sunburst_diagram_default()`](https://thomas-fung.github.io/spinviz/reference/sunburst_diagram_default.md)
to build a sunburst diagram from this taxonomy plus your own injury
counts, without needing to retype the tissue/pathology labels yourself.
[body_categories](https://thomas-fung.github.io/spinviz/reference/body_categories.md)
for the equivalent taxonomy used by
[`heatmap_diagram()`](https://thomas-fung.github.io/spinviz/reference/heatmap_diagram.md).
