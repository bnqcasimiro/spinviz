#' @title Sunburst diagram
#'
#' @description
#' Creates a two-level sunburst diagram (Tissue type -> Pathology
#' type) using the echarts4r package, with a dark-to-light colour scheme
#' (dark inner ring per tissue, lighter shades for its pathology children),
#' a colour-coded legend for tissue type, and an optional toggle to
#' exclude "Unspecified" tissue entries.
#'
#' The diagram can be hovered over for more granular detail.
#'
#' @param data (Required) A data frame with the Tissue type, Pathology type,
#' and sporting injury counts. Expected to follow the Olympic Committee
#' standards of reporting epidemiological data on injury and illness in sport.
#' Expects at least 3 columns; must be in the following order, but column name
#' is not important:
#'
#' 1: Tissue, in string format, e.g. "Muscle/tendon"
#'
#' 2: Pathology type, in string format, e.g. "Muscle Injury"
#'
#' 3...n: Injury frequency, positive integer.
#'
#' Rather than typing out the Tissue/Pathology columns yourself, you can
#' start from the built-in [injury_categories] taxonomy (which already has
#' columns 1 and 2 filled in with the standard classification) and just add
#' your own injury counts as column 3. See [sunburst_diagram_default()].
#'
#' If your data lives in a CSV file, [create_sunburst_template()] writes a
#' template pre-filled with the [injury_categories] taxonomy, and
#' [read_sunburst_data()] reads a completed file back in, validating the
#' column structure and category labels.
#'
#' @param column_name A string of the name of the column that contains the
#' injury frequency. Will use the 3rd column if unspecified.
#' @param replace_names A vector of strings that contains the original and
#' replacement values of tissue/pathology names, e.g.
#' c("Muscle/tendon"="Muscle"). Applied to the data before the tree is built.
#' @param depth An integer: 1 shows tissue-level rings only, anything else
#' (default -1) shows both tissue and pathology rings.
#' @param palette A string or vector of hex colours - the BASE (tissue-level)
#' colour palette. Can be a value from hcl.pals()/viridis, or a vector of hex
#' values, one per tissue type (excluding "Unspecified", which is always
#' grey). Pathology (outer ring) colours are automatically derived as
#' lightened versions of their parent tissue's colour.
#' @param transparency A number between 0 and 1 for diagram opacity. Default 1.
#' @param plot_title A string title for the diagram.
#' @param font_size,font_style,font_color,font_family Text styling for slice
#' labels (font_style accepts "italic"/"normal", mapped to CSS fontStyle).
#' @param title_size,title_color,title_x,title_y,title_family Text/position
#' styling for the plot title. title_x/title_y accept ECharts position
#' keywords ("left"/"center"/"right", "top"/"middle"/"bottom") or px/%.
#' @param include_non_specific Logical toggle. If FALSE, rows where Tissue is
#' "Unspecified" are excluded from the diagram entirely. If TRUE (default),
#' they are included and always coloured grey, regardless of `palette`.
#' @param inner_radius,mid_radius,outer_radius Strings (ECharts radius units,
#' e.g. "15%") controlling, respectively: the size of the blank centre hole,
#' the outer edge of the tissue ring/inner edge of the pathology ring, and
#' the outer edge of the pathology ring. Defaults: "10%", "40%", "50%".
#' @param label_line_length,label_line_length2 Numbers (pixels) controlling
#' the two segments of the leader line connecting each outer-ring label back
#' to its slice: `label_line_length` is the segment hugging the slice edge,
#' `label_line_length2` is the segment running out to the label text.
#' Increase these (and/or shrink `outer_radius`) to pull labels further out
#' and reduce crowding. Defaults: 40, 80.
#' @param show_outer_labels Logical. If `FALSE`, hides the outer-ring
#' (pathology) labels and their leader lines entirely. The slices
#' themselves still render and are still hoverable via tooltip. If `TRUE`
#' (default), labels show as usual.
#' @param min_label_angle Minimum slice angle (in degrees) below which a
#' slice's label is hidden to reduce clutter. Default 2.
#' @param outer_label_width Maximum width (in pixels) of an outer-ring label
#' before it wraps. Default 90.
#' @param widget_width,widget_height Size of the htmlwidget canvas, e.g.
#' `"1200px"`. Also recorded on the returned widget and used by
#' [save_diagram()] to derive the export ratio. Defaults `"1200px"` and
#' `"1500px"`.
#'
#' @return An echarts4r htmlwidget.
#' @seealso [save_diagram()] to export this chart directly
#' to a specific file type at a chosen size; [create_sunburst_template()]
#' and [read_sunburst_data()] for the CSV template workflow.
#' @export
#'
#' @examples
#' # Easiest: start from the built-in injury_categories taxonomy
#' # (Tissue/Pathology columns are already filled in) and add your
#' # own injury counts. See ?injury_categories for the category order.
#' df <- injury_categories
#'
#' df$boxing <- c(
#'   10, 19, 19, 41, 31, 18, 16, 16, 10, 10,
#'   27, 46, 67, 31, 54, 10, 27, 10, 96, 82,
#'   48, 26, 33, 34, 24
#' )
#'
#' df$judo <- c(
#'   25, 45, 37, 31, 19, 41, 48, 14, 49, 26,
#'   42, 24, 14, 16, 37, 41, 23, 36, 33, 44,
#'   48, 41, 31, 25, 48
#' )
#'
#' p1 <- sunburst_diagram_echarts(df, "boxing")
#' p2 <- sunburst_diagram_echarts(df, "judo")
#'
#' # Alternatively, supply the complete Tissue and Pathology names
#' # directly. Categories that are not required can be omitted.
#' # This example excludes the unspecified injury category.
#' Tissue <- c(
#'   "Muscle/Tendon", "Muscle/Tendon", "Muscle/Tendon",
#'   "Muscle/Tendon", "Muscle/Tendon",
#'   "Nervous", "Nervous",
#'   "Bone", "Bone", "Bone", "Bone", "Bone",
#'   "Cartilage/Synovium/Bursa", "Cartilage/Synovium/Bursa",
#'   "Cartilage/Synovium/Bursa", "Cartilage/Synovium/Bursa",
#'   "Ligament/Joint capsule", "Ligament/Joint capsule",
#'   "Superficial tissues/skin", "Superficial tissues/skin",
#'   "Superficial tissues/skin",
#'   "Vessels",
#'   "Stump",
#'   "Internal organs"
#' )
#'
#' Pathology <- c(
#'   "Muscle injury",
#'   "Muscle contusion",
#'   "Muscle compartment syndrome",
#'   "Tendinopathy",
#'   "Tendon rupture",
#'   "Brain/Spinal cord injury",
#'   "Peripheral nerve Injury",
#'   "Fracture",
#'   "Bone stress injury",
#'   "Bone contusion",
#'   "Avascular necrosis",
#'   "Physis injury",
#'   "Cartilage injury",
#'   "Arthritis",
#'   "Synovitis/Capsulitis",
#'   "Bursitis",
#'   "Joint sprain (ligament tear or acute instability episode)",
#'   "Chronic instability",
#'   "Contusion (superficial)",
#'   "Laceration",
#'   "Abrasion",
#'   "Vascular trauma",
#'   "Stump injury",
#'   "Organ trauma"
#' )
#'
#' boxing2 <- c(
#'   10, 19, 19, 41, 31, 18, 16, 16, 10, 10,
#'   27, 46, 67, 31, 54, 10, 27, 10, 96, 82,
#'   48, 26, 33, 34
#' )
#'
#' df2 <- data.frame(
#'   Tissue,
#'   Pathology,
#'   boxing = boxing2
#' )
#'
#' p3 <- sunburst_diagram_echarts(df2, "boxing")

sunburst_diagram_echarts <- function(
  data,
  column_name = "",
  replace_names = "",
  depth = -1,
  palette = "Dark 3",
  transparency = 1,
  plot_title = "",
  font_size = 12,
  font_style = "normal",
  font_color = "#333333",
  font_family = "Arial",
  title_size = 18,
  title_color = "#333333",
  title_x = "center",
  title_y = "20%",
  title_family = "Arial",
  include_non_specific = TRUE,
  inner_radius = "10%",
  mid_radius = "40%",
  outer_radius = "50%",
  label_line_length = 40,
  label_line_length2 = 80,
  min_label_angle = 2,
  outer_label_width = 90,
  show_outer_labels = TRUE,
  widget_width = "1200px",
  widget_height = "1500px"
) {
  #=========================================================
  # Helper: lighten a hex colour towards white by `factor` (0 = unchanged, 1 = white)
  #=========================================================
  lighten_colour <- function(hex, factor = 0.55) {
    rgb_col <- grDevices::col2rgb(hex) / 255
    blended <- rgb_col * (1 - factor) + 1 * factor
    grDevices::rgb(blended[1], blended[2], blended[3])
  }

  non_specific_label_pattern <- "^(non-specific|unspecified)$"
  non_specific_colour <- "#B0B0B0"
  non_specific_colour_light <- lighten_colour(non_specific_colour, 0.45)

  #=========================================================
  # Data Preparation
  #=========================================================
  tissue <- colnames(data)[1]
  injury <- colnames(data)[2]

  if (column_name == "") {
    current_sport <- colnames(data)[3]
    if (is.na(current_sport)) {
      stop("Dataset contains less than 3 columns", call. = FALSE)
    }
  } else {
    current_sport <- colnames(data)[which(colnames(data) == column_name)]
    if (length(current_sport) == 0) {
      stop("Column name does not exist in dataset", call. = FALSE)
    }
  }

  # Fill in missing values in the tissue column (carry the last seen tissue down)
  if (any(data[tissue] == "")) {
    new_name <- ""
    for (i in 1:nrow(data[tissue])) {
      if (data[i, tissue] != "") {
        new_name <- data[i, tissue]
      } else {
        data[i, tissue] <- new_name
      }
    }
  }

  if (any(is.numeric(data[[current_sport]])) == FALSE) {
    dplyr::mutate_at(data, current_sport, as.numeric)
  }

  # Change null to 0 as nulls cause blank output, 0s are ignored
  tryCatch(
    if (any(is.na(data[current_sport]))) {
      data[current_sport][is.na(data[current_sport])] <- 0
      warning(
        "Caution: null values found in dataset. Changed to 0",
        call. = FALSE
      )
    }
  )

  if (nrow(data[current_sport]) == 0) {
    stop("Column contains 0 rows", call. = FALSE)
  } else if (colSums(data[current_sport]) == 0) {
    stop("Sum of column equals 0", call. = FALSE)
  }

  #=========================================================
  # Variable Initialisation
  #=========================================================
  current_data <- data[, c(tissue, injury, current_sport)]
  current_data <- subset(current_data, current_data[injury] != "")

  # Toggle: drop "Non-specific" tissue rows entirely if requested
  if (!isTRUE(include_non_specific)) {
    keep_rows <- !grepl(
      non_specific_label_pattern,
      trimws(current_data[[tissue]]),
      ignore.case = TRUE
    )
    current_data <- current_data[keep_rows, ]
    if (nrow(current_data) == 0) {
      stop(
        "No rows remain after excluding 'Non-specific' tissue",
        call. = FALSE
      )
    }
  }

  tissue_types <- unique(current_data[[tissue]])
  n_tissue <- length(tissue_types)

  # Base colour per tissue type
  if (is.null(palette) || (length(palette) == 1 && !identical(palette, ""))) {
    base_colours <- diagram_colours(palette, n_tissue)
    if (is.null(base_colours) || length(base_colours) < n_tissue) {
      warning(
        "diagram_colours('",
        palette,
        "', ",
        n_tissue,
        ") returned ",
        length(base_colours),
        " colour(s) for ",
        n_tissue,
        " tissue types; falling back to the 'Dark 3' palette."
      )
      base_colours <- diagram_colours("Dark 3", n_tissue)
    }
  } else {
    base_colours <- rep(palette, length.out = n_tissue)
  }

  show_pathology <- !(identical(depth, 1) || identical(depth, "1"))

  #=========================================================
  # Build the ECharts hierarchical `data` list
  #=========================================================
  tree_data <- vector("list", n_tissue)
  legend_names <- character(n_tissue)
  pathology_flat <- list()

  for (i in seq_len(n_tissue)) {
    current_tissue <- tissue_types[i]
    rows <- current_data[current_data[[tissue]] == current_tissue, ]

    is_non_specific <- grepl(
      non_specific_label_pattern,
      trimws(current_tissue),
      ignore.case = TRUE
    )
    tissue_colour <- if (is_non_specific) {
      non_specific_colour
    } else {
      base_colours[i]
    }
    pathology_colour <- if (is_non_specific) {
      non_specific_colour_light
    } else {
      lighten_colour(tissue_colour, 0.55)
    }

    legend_names[i] <- current_tissue

    children <- NULL
    if (show_pathology) {
      children <- lapply(seq_len(nrow(rows)), function(j) {
        list(
          name = rows[[injury]][j],
          value = rows[[current_sport]][j],
          itemStyle = list(color = pathology_colour, opacity = transparency)
        )
      })
      for (j in seq_len(nrow(rows))) {
        pathology_flat[[length(pathology_flat) + 1]] <- list(
          name = rows[[injury]][j],
          value = rows[[current_sport]][j]
        )
      }
    }

    tree_data[[i]] <- list(
      name = current_tissue,
      value = if (!show_pathology) sum(rows[[current_sport]]) else NULL,
      itemStyle = list(color = tissue_colour, opacity = transparency),
      children = children
    )
  }

  #=========================================================
  # Assemble the full ECharts option and render
  #=========================================================
  opts <- list(
    title = list(
      text = plot_title,
      left = title_x,
      top = title_y,
      textStyle = list(
        fontSize = title_size,
        color = title_color,
        fontFamily = title_family
      )
    ),
    legend = list(
      type = "scroll",
      orient = "vertical",
      right = 10,
      top = "middle",
      data = as.list(legend_names[
        !grepl(non_specific_label_pattern, legend_names, ignore.case = TRUE)
      ])
    ),
    tooltip = list(trigger = "item", formatter = "{b}: {c}"),
    toolbox = list(
      show = TRUE,
      feature = list(
        saveAsImage = list(
          show = TRUE,
          type = "jpeg",
          title = "Save as JPG",
          backgroundColor = "#ffffff"
        )
      )
    ),
    series = NULL
  )

  sunburst_series <- list(
    type = "sunburst",
    radius = list(0, "100%"),
    sort = NULL,
    nodeClick = FALSE,
    emphasis = list(label = list(show = FALSE)),
    data = tree_data,

    label = list(
      color = font_color,
      fontSize = font_size,
      fontFamily = font_family,
      fontStyle = font_style
    ),

    itemStyle = list(
      borderWidth = 1,
      borderColor = "#fff"
    ),

    levels = list(
      list(),

      list(
        r0 = inner_radius,
        r = mid_radius,
        label = list(
          rotate = "radial",
          position = "inside",
          align = "center",
          verticalAlign = "middle",
          overflow = "truncate"
        )
      ),

      list(
        r0 = mid_radius,
        r = outer_radius,
        label = list(show = FALSE)
      )
    )
  )

  all_series <- list(sunburst_series)

  if (show_pathology) {
    pie_series <- list(
      type = "pie",
      radius = list(mid_radius, outer_radius),
      center = list("50%", "50%"),
      silent = TRUE,
      sort = NULL,
      data = pathology_flat,
      itemStyle = list(color = "transparent", borderWidth = 0),
      emphasis = list(disabled = TRUE),
      avoidLabelOverlap = TRUE,
      minShowLabelAngle = min_label_angle,
      label = list(
        show = show_outer_labels,
        position = "outside",
        color = font_color,
        fontSize = font_size,
        fontFamily = font_family,
        fontStyle = font_style,
        overflow = "break",
        width = outer_label_width
      ),
      labelLine = list(
        show = show_outer_labels,
        length = label_line_length,
        length2 = label_line_length2,
        smooth = FALSE,
        lineStyle = list(width = 1, color = "#999999")
      )
    )
    all_series <- c(all_series, list(pie_series))
  }

  opts$series <- all_series

  chart <- echarts4r::e_list(
    echarts4r::e_charts(width = widget_width, height = widget_height),
    opts
  )

  attr(chart, "spinviz_sunburst_meta") <- list(
    widget_width = widget_width,
    widget_height = widget_height
  )

  chart
}
