# spinviz 0.0.1.0000

## Breaking changes

* The required column names for injury data have been standardised to
  snake_case: `Region.area` is now `region_area` and `Subcategory` is now
  `subcategory`. This affects `create_injury_template()`,
  `read_injury_data()`, and `heatmap_diagram()`. Existing CSV files using the
  old names must be updated; `read_injury_data()` now errors on the old
  names with a hint explaining the rename.
* `injury_heatmap()` has been renamed to `heatmap_diagram()`.
* Example sport columns in the README and template now use lowercase names
  (e.g. `sport1` instead of `Sport_1`) for consistency. Sport column names
  remain free-form.

## New features

* New `sunburst_diagram_echarts()` renders an interactive tissue/pathology
  sunburst diagram using echarts4r.
* New built-in taxonomies: `body_categories` (19-row
  region_area/subcategory, used by the heatmap) and `injury_categories`
  (25-row tissue/pathology, used by the sunburst), each with no counts
  attached, ready to combine with your own counts.
* New `heatmap_diagram_default()` and `sunburst_diagram_default()` wrappers
  build a diagram from just a vector of counts matched by row position to
  the corresponding built-in taxonomy.
* `heatmap_diagram()` now displays an "Unspecified" row, if present in the
  data, as a label below the diagram (it has no body region to colour).
* New `save_diagram()` exports heatmaps (via `ggplot2::ggsave()`) and
  sunbursts (via chromote screenshots/PDF) with the correct width:height
  ratio derived automatically from metadata recorded on the plot object.
* New `create_sunburst_template()` writes a template CSV pre-filled with
  the 25-row `injury_categories` tissue/pathology taxonomy and one empty
  column per requested sport, ready to be filled in with injury
  frequencies.
* New `read_sunburst_data()` reads and validates such a CSV, checking for
  the required `tissue`/`pathology` columns and at least one sport column,
  warning on unrecognised (possibly misspelled) tissue/pathology values,
  and carrying blank `tissue` cells down from the row above so compact
  hand-edited files work.

## Bug fixes

* `heatmap_diagram()` single-view labels no longer overlap: adjacent labels
  are now spread to a minimum vertical gap. This also fixed a latent bug
  where each region's label was drawn multiple times (once per underlying
  SVG id, exactly on top of itself); each region now gets a single label.

## Other changes

* Extended the test suite to cover `sunburst_diagram_echarts()`,
  `sunburst_diagram_default()`, and `save_diagram()` (92 tests in total).
  The chromote-based sunburst export test skips gracefully when no
  Chromium-based browser is available (e.g. CRAN or minimal CI images).
* Added `echarts4r`, `htmlwidgets`, and `tools` to Imports, and `chromote`
  and `base64enc` to Suggests (only needed for sunburst export in
  `save_diagram()`).
* `save_diagram()` now validates a sunburst widget's recorded size metadata
  before checking that chromote/base64enc are installed, so the more
  informative error is always the one reported.
* In `sunburst_diagram_echarts()`, the tcltk pop-up `error_messages()`
  helper was replaced with standard `stop()`/`warning()`, and the
  `warnings` argument was removed.
* Updated the README to document the sunburst diagrams, built-in
  taxonomies, convenience wrappers, and `save_diagram()`, and refreshed all
  example figures (heatmaps regenerated with the new code; new sunburst
  figure exported via chromote).
* New `create_injury_template()` writes a template CSV pre-filled with all
  recognised body subcategories and one column per requested sport, ready to
  be filled in with injury frequencies.
* New `read_injury_data()` reads and validates such a CSV, checking for the
  required `region_area`/`subcategory` columns, at least one sport column,
  and unrecognised (possibly misspelled) subcategory values. Files are read
  with `check.names = FALSE` so user-supplied column names are preserved
  exactly as written.
* Added `rsvg` to Imports. It is required at runtime by
  `magick::image_read_svg()` when rendering the SVG body diagrams, and its
  absence caused failures on systems (e.g. CI runners) where it was not
  already installed.
* Removed a duplicate `grDevices` entry from `DESCRIPTION` Imports.
* Added a `tests/testthat` suite covering `diagram_colours()`, `test_colour()`,
  and `heatmap_diagram()`.
* `heatmap_diagram()` no longer calls `print()` on its result before
  returning it, which previously caused the plot to render twice in
  R Markdown/Quarto documents when the call was left unassigned.
* `heatmap_diagram()` now warns when values in the selected sport column
  cannot be converted to numeric, instead of silently coercing them to `NA`.
* `heatmap_diagram()`'s documentation for the `palette` argument is now
  regenerated and no longer shows the stale "WIP" placeholder text.
* `heatmap_diagram()`'s front/back label coordinate tables are now built
  from a single shared lookup (`label_position_lookup()`) instead of two
  hand-duplicated tables, so the views cannot silently drift apart.
* `heatmap_diagram()`'s combined "both views" label table
  (`both_label_position_lookup()`) now validates its front-only/back-only
  regions against the same single source of truth
  (`view_exclusive_regions()`) used by the SVG id and per-view label
  lookups, instead of re-encoding which regions are one-sided a third
  time.
* `test_colour()` now restores the caller's `graphics::par()` settings on
  exit instead of permanently changing the plotting layout (`mfrow`).
* Fixed a documentation typo in `test_colour()` ("coloublind" ->
  "colourblind").
* Documentation moved to a pkgdown site
  (<https://bnqcasimiro.github.io/spinviz/>): a "Get started" vignette with
  runnable examples, articles on importing CSV data and on customising and
  saving diagrams, and a grouped function reference. The README is
  correspondingly slimmer, keeping only installation and quick examples.
