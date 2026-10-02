# heatmap_diagram validates its arguments

    Code
      heatmap_diagram(df, "not_a_column", "front")
    Condition
      Error in `heatmap_diagram()`:
      ! selected_sport %in% names(injury_data) is not TRUE

---

    Code
      heatmap_diagram(df, "boxing", "sideways")
    Condition
      Error in `heatmap_diagram()`:
      ! view_choice %in% c("front", "back", "both") is not TRUE

---

    Code
      heatmap_diagram(df, "boxing", "front", opacity = 2)
    Condition
      Error in `heatmap_diagram()`:
      ! opacity <= 1 is not TRUE

# heatmap_diagram warns when injury values are not numeric

    Code
      invisible(heatmap_diagram(df, "boxing", "front", show_values = FALSE))
    Condition
      Warning:
      There was 1 warning in `transmute()`.
      i In argument: `TotalInjuries = coerce_injury_values(.data[["boxing"]], selected_sport)`.
      Caused by warning:
      ! 1 value(s) in column 'boxing' could not be converted to numeric and were set to NA.

