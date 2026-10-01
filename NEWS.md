# spinviz 0.0.1.0000

## Breaking changes

* The required column names for injury data have been standardised to
  snake_case: `Region.area` is now `region_area` and `Subcategory` is now
  `subcategory`. This affects `create_injury_template()`,
  `read_injury_data()`, and `injury_heatmap()`. Existing CSV files using the
  old names must be updated; `read_injury_data()` now errors on the old
  names with a hint explaining the rename.
* Example sport columns in the README and template now use lowercase names
  (e.g. `sport1` instead of `Sport_1`) for consistency. Sport column names
  remain free-form.

## Other changes

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
  and `injury_heatmap()`.
* `injury_heatmap()` no longer calls `print()` on its result before
  returning it, which previously caused the plot to render twice in
  R Markdown/Quarto documents when the call was left unassigned.
* `injury_heatmap()` now warns when values in the selected sport column
  cannot be converted to numeric, instead of silently coercing them to `NA`.
* `injury_heatmap()`'s documentation for the `palette` argument is now
  regenerated and no longer shows the stale "WIP" placeholder text.
* `injury_heatmap()`'s front/back label coordinate tables are now built
  from a single shared lookup (`label_position_lookup()`) instead of two
  hand-duplicated tables, so the views cannot silently drift apart.
* `injury_heatmap()`'s combined "both views" label table
  (`both_label_position_lookup()`) now validates its front-only/back-only
  regions against the same single source of truth
  (`view_exclusive_regions()`) used by the SVG id and per-view label
  lookups, instead of re-encoding which regions are one-sided a third
  time.
* `test_colour()` now restores the caller's `graphics::par()` settings on
  exit instead of permanently changing the plotting layout (`mfrow`).
* Fixed a documentation typo in `test_colour()` ("coloublind" ->
  "colourblind").
