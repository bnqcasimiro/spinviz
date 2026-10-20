# plot_heatmap validates its arguments

    Code
      plot_heatmap(df, "not_a_column", "front")
    Condition
      Error in `plot_heatmap()`:
      ! selected_sport %in% names(injury_data) is not TRUE

---

    Code
      plot_heatmap(df, "boxing", "sideways")
    Condition
      Error in `plot_heatmap()`:
      ! view_choice %in% c("front", "back", "both") is not TRUE

---

    Code
      plot_heatmap(df, "boxing", "front", opacity = 2)
    Condition
      Error in `plot_heatmap()`:
      ! opacity <= 1 is not TRUE

# plot_heatmap warns when injury values are not numeric

    Code
      invisible(plot_heatmap(df, "boxing", "front", show_values = FALSE))
    Condition
      Warning:
      There was 1 warning in `transmute()`.
      i In argument: `TotalInjuries = coerce_injury_values(.data[["boxing"]], selected_sport)`.
      Caused by warning:
      ! 1 value(s) in column 'boxing' could not be converted to numeric and were set to NA.

