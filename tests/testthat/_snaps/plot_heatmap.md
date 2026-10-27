# plot_heatmap validates its arguments

    Code
      plot_heatmap(df, "not_a_column", "anterior")
    Condition
      Error in `plot_heatmap()`:
      ! group %in% names(data) is not TRUE

---

    Code
      plot_heatmap(df, "boxing", "sideways")
    Condition
      Error in `plot_heatmap()`:
      ! view %in% c("anterior", "posterior", "both") is not TRUE

---

    Code
      plot_heatmap(df, "boxing", "anterior", opacity = 2)
    Condition
      Error in `plot_heatmap()`:
      ! opacity <= 1 is not TRUE

# plot_heatmap warns when injury values are not numeric

    Code
      invisible(plot_heatmap(df, "boxing", "anterior", show_values = FALSE))
    Condition
      Warning:
      There was 1 warning in `transmute()`.
      i In argument: `TotalInjuries = coerce_injury_values(.data[["boxing"]], group)`.
      Caused by warning:
      ! 1 value(s) in column 'boxing' could not be converted to numeric and were set to NA.

