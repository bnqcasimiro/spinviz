# Coerce a data column to numeric, warning (rather than silently dropping
# data) when non-missing values fail to parse as numbers.
coerce_injury_values <- function(x, column_name) {
  values <- suppressWarnings(as.numeric(x))
  newly_missing <- is.na(values) & !is.na(x)
  if (any(newly_missing)) {
    warning(
      sprintf(
        "%d value(s) in column '%s' could not be converted to numeric and were set to NA.",
        sum(newly_missing),
        column_name
      ),
      call. = FALSE
    )
  }
  values
}

# Single source of truth for which body regions are visible from only one
# side of the body. svg_id_lookup(), label_position_lookup(), and the
# combined "both views" label table (both_label_position_lookup()) each
# need to agree on this, so it is defined once here and each of those
# functions validates its own region set against it, instead of encoding
# the same front-only/back-only knowledge independently three times.
view_exclusive_regions <- function() {
  list(front_only = "Abdomen", back_only = c("Thoracic Spine", "Lumbosacral"))
}

# Shared front/back mapping from a title-cased region_area to the SVG
# element id(s) it should colour. Kept as a single lookup table so the two
# views cannot silently drift apart; view-specific asymmetries (e.g. Hip
# Groin, Thoracic Spine/Lumbosacral) are captured explicitly per view.
svg_id_lookup <- function(view_choice_inner) {
  front <- list(
    "Head" = "Head",
    "Neck" = "Neck",
    "Shoulder" = c("Right_Shoulder", "Left_Shoulder"),
    "Chest" = c("Chest", "Right_Chest", "Left_Chest"),
    "Abdomen" = "Abdomen",
    "Upper Arm" = c("Right_Upper_Arm", "Left_Upper_Arm"),
    "Elbow" = c("Right_Elbow", "Left_Elbow"),
    "Forearm" = c("Right_Forearm", "Left_Forearm"),
    "Thigh" = c("Right_Thigh", "Left_Thigh"),
    "Knee" = c("Right_Knee", "Left_Knee"),
    "Lower Leg" = c("Right_Lower_Leg", "Left_Lower_Leg"),
    "Wrist" = c("Right_Wrist", "Left_Wrist"),
    "Hand" = c("Right_Hand", "Left_Hand"),
    "Ankle" = c("Right_Ankle", "Left_Ankle"),
    "Foot" = c("Right_Foot", "Left_Foot"),
    "Hip Groin" = c("Hip_Groin", "Left_Hip_Groin", "Right_Hip_Groin")
  )

  back <- front
  back[["Abdomen"]] <- NULL
  back[["Hip Groin"]] <- c("Left_Hip_Groin", "Right_Hip_Groin")
  back[["Thoracic Spine"]] <- "Thoracic_Spine"
  back[["Lumbosacral"]] <- "Lumbosacral"

  exclusive <- view_exclusive_regions()
  stopifnot(
    setequal(setdiff(names(front), names(back)), exclusive$front_only),
    setequal(setdiff(names(back), names(front)), exclusive$back_only)
  )

  if (view_choice_inner == "front") front else back
}

# Look up SVG ids for a vector of region_area values, matching the fallback
# behaviour of the case_when() this replaces (unmatched regions map to NA).
map_svg_ids <- function(region, view_choice_inner) {
  lookup <- svg_id_lookup(view_choice_inner)
  lapply(region, function(r) {
    ids <- lookup[[r]]
    if (is.null(ids)) NA_character_ else ids
  })
}

# Shared front/back label placement lookup. Most regions share the same
# label/target coordinates in both views; only the rows that genuinely
# differ between views (Head, Neck, Shoulder, Forearm, Hip Groin), or that
# only exist in one view (Abdomen is front-only; Thoracic Spine and
# Lumbosacral are back-only), need a view-specific override.
label_position_lookup <- function(view_choice_inner) {
  common_positions <- tribble(
    ~region_area , ~label_x , ~label_y , ~target_x , ~target_y ,
    "Chest"      , 0.8      , 0.87     , 0.51      , 0.80      ,
    "Upper Arm"  , 0.2      , 0.82     , 0.45      , 0.79      ,
    "Elbow"      , 0.15     , 0.77     , 0.44      , 0.74      ,
    "Wrist"      , 0.85     , 0.61     , 0.44      , 0.61      ,
    "Hand"       , 0.2      , 0.57     , 0.45      , 0.57      ,
    "Thigh"      , 0.8      , 0.48     , 0.50      , 0.48      ,
    "Knee"       , 0.2      , 0.40     , 0.51      , 0.40      ,
    "Lower Leg"  , 0.8      , 0.31     , 0.51      , 0.31      ,
    "Ankle"      , 0.2      , 0.22     , 0.52      , 0.21      ,
    "Foot"       , 0.75     , 0.18     , 0.50      , 0.17
  )

  front_only <- tribble(
    ~region_area , ~label_x , ~label_y , ~target_x , ~target_y ,
    "Head"       , 0.9      , 1.01     , 0.55      , 1.01      ,
    "Neck"       , 0.8      , 0.97     , 0.55      , 0.91      ,
    "Shoulder"   , 0.2      , 0.92     , 0.45      , 0.86      ,
    "Abdomen"    , 0.8      , 0.73     , 0.52      , 0.70      ,
    "Forearm"    , 0.2      , 0.69     , 0.44      , 0.68      ,
    "Hip Groin"  , 0.90     , 0.65     , 0.50      , 0.65
  )

  back_only <- tribble(
    ~region_area     , ~label_x , ~label_y , ~target_x , ~target_y ,
    "Head"           , 0.9      , 1.05     , 0.54      , 1.01      ,
    "Neck"           , 0.8      , 1.00     , 0.55      , 0.92      ,
    "Shoulder"       , 0.2      , 0.96     , 0.47      , 0.87      ,
    "Thoracic Spine" , 0.75     , 0.91     , 0.57      , 0.83      ,
    "Forearm"        , 0.2      , 0.73     , 0.44      , 0.68      ,
    "Hip Groin"      , 0.90     , 0.69     , 0.50      , 0.66      ,
    "Lumbosacral"    , 0.76     , 0.65     , 0.53      , 0.64
  )

  exclusive <- view_exclusive_regions()
  stopifnot(
    setequal(
      setdiff(front_only$region_area, back_only$region_area),
      exclusive$front_only
    ),
    setequal(
      setdiff(back_only$region_area, front_only$region_area),
      exclusive$back_only
    )
  )

  view_specific <- if (view_choice_inner == "front") front_only else back_only
  bind_rows(common_positions, view_specific)
}

# Label positions for the combined "front + back" layout. The numeric
# coordinates are independently tuned for the narrower three-panel
# composite (they are not a transform of label_position_lookup()'s
# single-view coordinates), but which regions are only visible from one
# side is not: that is validated against view_exclusive_regions() so this
# table cannot silently drift from svg_id_lookup()/label_position_lookup().
both_label_position_lookup <- function() {
  positions <- tribble(
    ~region_area     , ~label_y , ~front_target_x , ~front_target_y , ~back_target_x , ~back_target_y ,
    "Head"           , 1.05     , 0.56            , 1.01            , 0.49           , 1.01           ,
    "Neck"           , 1.00     , 0.56            , 0.91            , 0.50           , 0.92           ,
    "Shoulder"       , 0.96     , 0.64            , 0.87            , 0.42           , 0.87           ,
    "Thoracic Spine" , 0.91     , NA_real_        , NA_real_        , 0.52           , 0.83           ,
    "Chest"          , 0.87     , 0.55            , 0.82            , 0.48           , 0.80           ,
    "Upper Arm"      , 0.82     , 0.64            , 0.79            , 0.41           , 0.79           ,
    "Elbow"          , 0.77     , 0.65            , 0.74            , 0.41           , 0.74           ,
    "Forearm"        , 0.73     , 0.66            , 0.70            , 0.40           , 0.70           ,
    "Abdomen"        , 0.69     , 0.55            , 0.68            , NA_real_       , NA_real_       ,
    "Hip Groin"      , 0.65     , 0.56            , 0.63            , 0.47           , 0.66           ,
    "Lumbosacral"    , 0.61     , NA_real_        , NA_real_        , 0.53           , 0.64           ,
    "Wrist"          , 0.57     , 0.65            , 0.61            , 0.41           , 0.61           ,
    "Hand"           , 0.53     , 0.65            , 0.57            , 0.41           , 0.57           ,
    "Thigh"          , 0.48     , 0.60            , 0.48            , 0.45           , 0.48           ,
    "Knee"           , 0.40     , 0.60            , 0.41            , 0.46           , 0.41           ,
    "Lower Leg"      , 0.31     , 0.60            , 0.31            , 0.46           , 0.31           ,
    "Ankle"          , 0.22     , 0.58            , 0.21            , 0.48           , 0.21           ,
    "Foot"           , 0.18     , 0.60            , 0.17            , 0.46           , 0.17
  )

  exclusive <- view_exclusive_regions()
  stopifnot(
    setequal(
      positions$region_area[is.na(positions$front_target_x)],
      exclusive$back_only
    ),
    setequal(
      positions$region_area[is.na(positions$back_target_x)],
      exclusive$front_only
    )
  )

  positions
}

#' @title Heat map of Injuries on Human Body
#'
#' @description
#' Creates a heat map projected onto a diagram of the human body,
#' based on the number of injuries on body parts visible on the
#' diagram.

#' Diagrams can be viewed from the front, back, or both views

#' @param injury_data a data frame with the body region, area the injury occurred,
#' and the frequency of the injury
#' @param selected_sport the name of the sport, corresponding to the header of
#' the frequency column in the data frame.
#' @param view_choice which view to render the diagram. One of ("front", "back", or "both")
#' @param sex which body diagram to use: `"male"` or `"female"`.
#' @param palette Optional colour palette.
#' \itemize{
#'   \item A single palette name supported by \code{\link{diagram_colours}} (viridis or HCL),
#'   e.g. \code{"magma"} or \code{"Reds"}.
#'   \item A character vector of hex colours, e.g. \code{c("#FFFFFF", "#FF0000")}.
#'   \item HCL palette names can be listed with \code{grDevices::hcl.pals()}.
#' }
#' @param opacity Numeric between `0` and `1`. Opacity of the filled body regions.
#' @param show_labels Logical. If `TRUE`, show region names.
#' @param show_values Logical. If `TRUE`, show injury values.
#' @param show_scale Logical. If `TRUE`, show the colour scale (legend).

#' @importFrom dplyr select rename mutate tribble slice pull left_join rowwise filter transmute bind_rows distinct
#' @importFrom xml2 read_xml xml_root write_xml xml_attr xml_set_attr xml_find_all
#' @importFrom grDevices colorRampPalette as.raster adjustcolor
#' @importFrom ggplot2 ggplot aes geom_tile theme theme_void element_text annotation_raster coord_cartesian margin geom_text unit geom_segment scale_fill_gradientn guide_colorbar
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @importFrom this.path this.dir
#' @importFrom purrr walk
#' @importFrom tibble add_column
#' @importFrom stats na.omit
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom stringr str_to_title
#' @importFrom magick image_read_svg
#' @importFrom rsvg rsvg

#' @return A plot in R-studio viewer
#' @seealso
#' \code{\link{body_categories}} for the built-in region_area/subcategory
#' taxonomy, and \code{\link{heatmap_diagram_default}} for a wrapper that
#' only needs a vector of counts.
#' \code{\link{diagram_colours}} to preview supported palette names.
#' \code{\link{test_colour}} to visualise palettes.
#' \code{\link{save_diagram}} to save the plot to file with the correct
#' width:height ratio automatically applied.
#' @export
#' @examples
#' # Easiest: start from the built-in body_categories taxonomy (its
#' # region_area/subcategory columns are already filled in) and just add
#' # your own counts, matching its row order. See ?body_categories, or
#' # just run print(body_categories) to see the order directly.
#' df <- body_categories
#' df$boxing <- c(15, 5, 18, 20, 6, 14, 9, 11, 12, 18, 22, 10, 9, 16, 13, 7, 8, 3, 24)
#' # Generate a plot for front view, male
#' p1 <- heatmap_diagram(df, "boxing", "front", sex = "male", show_values = FALSE)
#' # You can customise the colour palette by:
#' p2 <- heatmap_diagram(df, "boxing", "front", sex = "female", palette = "plasma")
#' # You can add your own plot title by:
#' p2 + ggplot2::labs(title = "Boxing Injury Heatmap (Front)")

heatmap_diagram <- function(
  injury_data,
  selected_sport,
  view_choice,
  sex = c("male", "female"),
  palette = NULL,
  opacity = 0.8,
  show_labels = TRUE,
  show_values = TRUE,
  show_scale = TRUE
) {
  # ================================
  # 1. DATA PROCESSING
  # ================================

  stopifnot(selected_sport %in% names(injury_data))
  stopifnot(view_choice %in% c("front", "back", "both"))
  sex <- match.arg(sex)
  stopifnot(
    is.numeric(opacity),
    length(opacity) == 1,
    opacity >= 0,
    opacity <= 1
  )
  stopifnot(is.logical(show_labels), length(show_labels) == 1)
  stopifnot(is.logical(show_values), length(show_values) == 1)
  stopifnot(is.logical(show_scale), length(show_scale) == 1)

  # Shared title-cased region/numeric-count table, computed once so all
  # views and the Unspecified label use identical values.
  base_data <- injury_data |>
    transmute(
      region_area = str_to_title(trimws(as.character(.data$subcategory))),
      TotalInjuries = coerce_injury_values(
        .data[[selected_sport]],
        selected_sport
      )
    )

  # Detect an "Unspecified" row (shown as a label below the diagram rather
  # than coloured on it). NULL when the data has no such row; NA counts
  # become 0.
  unspecified_count <- base_data |>
    filter(.data$region_area == "Unspecified") |>
    pull(.data$TotalInjuries)

  if (length(unspecified_count) == 0) {
    unspecified_count <- NULL
  } else {
    unspecified_count <- unspecified_count[1]
    if (is.na(unspecified_count)) {
      unspecified_count <- 0
    }
  }

  get_svg_path <- function(filename) {
    p <- system.file("extdata", filename, package = "spinviz")
    if (p == "") {
      stop("SVG not found in package extdata/: ", filename, call. = FALSE)
    }
    p
  }

  # Enforce a minimum vertical gap between stacked labels so adjacent
  # rows never overlap at the rendered text size. Rows keep their relative
  # order; only crowded rows are pushed down.
  spread_label_y <- function(y, min_gap = 0.055) {
    ord <- order(y, decreasing = TRUE)
    ys <- y[ord]
    for (i in 2:length(ys)) {
      if (ys[i] > ys[i - 1] - min_gap) {
        ys[i] <- ys[i - 1] - min_gap
      }
    }
    y[ord] <- ys
    y
  }

  # Build label strings according to toggles
  make_label_text <- function(region, value) {
    if (!show_labels && !show_values) {
      return("")
    }
    if (show_labels && show_values) {
      return(paste0(
        region,
        " (",
        ifelse(is.na(value), "NA", round(value, 1)),
        ")"
      ))
    }
    if (show_labels && !show_values) {
      return(as.character(region))
    }
    # show_values && !show_labels
    return(ifelse(is.na(value), "NA", as.character(round(value, 1))))
  }

  # Render single view (front or back)
  generate_plot <- function(
    view_choice_inner,
    max_injuries_override = NULL,
    opacity_inner = opacity
  ) {
    # View mapping
    svg_file <- switch(
      paste0(sex, "_", view_choice_inner),
      "male_front" = get_svg_path("MaleFront.svg"),
      "male_back" = get_svg_path("MaleBack.svg"),
      "female_front" = get_svg_path("FemaleFront.svg"),
      "female_back" = get_svg_path("FemaleBack.svg"),
      stop("Invalid sex/view combination.", call. = FALSE)
    )

    processed_injury_data <- base_data |>
      mutate(SVG_ID = map_svg_ids(.data$region_area, view_choice_inner))

    stopifnot("SVG_ID" %in% names(processed_injury_data))

    label_positions <- label_position_lookup(view_choice_inner)

    processed_injury_data <- processed_injury_data |>
      unnest(cols = c(SVG_ID)) |>
      filter(!is.na(.data$SVG_ID))

    # Scale + palette setup
    max_injuries_local <- max(processed_injury_data$TotalInjuries, na.rm = TRUE)
    if (!is.finite(max_injuries_local)) {
      max_injuries_local <- 0
    }
    max_injuries <- if (!is.null(max_injuries_override)) {
      max_injuries_override
    } else {
      max_injuries_local
    }
    # --- Palette builder: always returns light -> dark (low -> high) ---
    build_palette <- function(pal, n) {
      default_palette <- c("#FFFFFF", "#FFFF00", "#FFA500", "#FF0000")

      if (is.null(pal)) {
        pal <- default_palette
      }
      if (is.character(pal) && length(pal) == 1 && !grepl("^#", pal)) {
        cols <- diagram_colours(pal, n_colours = n)
        if (is.null(cols)) {
          warning(
            "Palette '",
            pal,
            "' not recognised. Using default heat palette instead.",
            call. = FALSE
          )
          cols <- colorRampPalette(default_palette)(n)
        }
        return(cols)
      }

      colorRampPalette(pal)(n)
    }

    color_palette <- build_palette(palette, max_injuries + 1)

    # ================================
    # 2. SVG MANIPULATION
    # ================================

    svg_content <- read_xml(svg_file)
    nodes <- xml_find_all(svg_content, "//*[@id]")

    # Adjust SVG canvas height
    root <- xml_root(svg_content)
    current_height <- as.numeric(xml_attr(root, "height"))
    new_height <- current_height * 1.1
    xml_set_attr(root, "height", as.character(new_height))
    xml_set_attr(root, "viewBox", paste0("0 0 1024 ", new_height))

    color_paths <- function(path) {
      path_id <- xml_attr(path, "id")

      injury_match <- processed_injury_data |>
        filter(SVG_ID == path_id) |>
        slice(1) |>
        pull(TotalInjuries)

      # Handle no data match
      if (length(injury_match) == 0) {
        injury_match <- NA_real_
      }

      fill_color <- if (is.na(injury_match)) {
        "#F0F0F0"
      } else {
        val <- round(injury_match)
        val <- max(0, min(val, max_injuries))
        color_palette[val + 1]
      }

      xml_set_attr(
        path,
        "style",
        paste0(
          "fill:",
          fill_color,
          ";fill-opacity:",
          opacity_inner,
          ";opacity:1",
          ";stroke:#333333;stroke-width:0.2;stroke-linejoin:round"
        )
      )
    }

    walk(nodes, color_paths)

    # Rasterise modified SVG for ggplot
    modified_svg <- tempfile(
      pattern = paste0("filtered_body_", view_choice_inner, "_"),
      fileext = ".svg"
    )
    write_xml(svg_content, modified_svg)
    body_image <- image_read_svg(modified_svg)

    # ================================
    # 3. VISUAL ASSEMBLY
    # ================================

    # Single view
    legend_dummy <- data.frame(
      x = c(-1000, -1000),
      y = c(-1000, -1001),
      fill = c(max_injuries, NA_real_),
      alpha = opacity_inner
    )

    label_data <- label_positions |>
      # processed_injury_data has one row per SVG id (2-3 per region);
      # labels need one row per region
      left_join(processed_injury_data, by = "region_area") |>
      distinct(.data$region_area, .keep_all = TRUE)

    label_data$label_text <- mapply(
      make_label_text,
      region = label_data$region_area,
      value = label_data$TotalInjuries,
      USE.NAMES = FALSE
    )

    label_data$label_x <- 0.2
    label_hjust <- 1
    label_data$xend <- 0.85 * label_data$target_x + 0.15 * label_data$label_x

    # Add "Unspecified" below the other labels (no leader segment target)
    if (!is.null(unspecified_count)) {
      unspecified_y <- if (nrow(label_data) > 0) {
        min(label_data$label_y, na.rm = TRUE) - 0.08
      } else {
        0.1
      }
      label_data <- bind_rows(
        label_data,
        data.frame(
          region_area = "Unspecified",
          label_x = 0.2,
          label_y = unspecified_y,
          TotalInjuries = unspecified_count,
          label_text = make_label_text("Unspecified", unspecified_count),
          stringsAsFactors = FALSE
        )
      )
    }

    label_data$label_y <- spread_label_y(label_data$label_y)

    draw_text <- isTRUE(show_labels) || isTRUE(show_values)

    legend_palette <- adjustcolor(color_palette, alpha.f = opacity_inner)

    injury_plot <- ggplot() +
      annotation_raster(
        as.raster(body_image),
        xmin = 0,
        xmax = 1,
        ymin = 0,
        ymax = 1.1
      ) +
      geom_tile(
        data = legend_dummy,
        aes(x = .data$x, y = .data$y, fill = .data$fill)
      ) +
      scale_fill_gradientn(
        colors = legend_palette,
        name = NULL,
        limits = c(0, max_injuries),
        breaks = seq(0, max_injuries, length.out = 5),
        labels = round(seq(0, max_injuries, length.out = 5), 1),
        na.value = "#F0F0F0",
        guide = guide_colorbar(
          barheight = unit(5, "cm"),
          barwidth = unit(0.4, "cm"),
        )
      ) +
      coord_cartesian(xlim = c(-0.35, 1.00), ylim = c(0, 1.1), clip = "off") +
      theme_void() +
      theme(legend.position = "right", aspect.ratio = 1.1)

    if (!isTRUE(show_scale)) {
      injury_plot <- injury_plot + theme(legend.position = "none")
    }

    if (draw_text) {
      injury_plot <- injury_plot +
        geom_text(
          data = label_data,
          aes(x = .data$label_x, y = .data$label_y, label = .data$label_text),
          size = 3.2,
          hjust = label_hjust,
          vjust = 0.5,
          fontface = "plain"
        ) +
        geom_segment(
          # Unspecified rows have no leader-line target
          data = filter(label_data, !is.na(.data$target_y)),
          aes(
            x = .data$label_x,
            y = .data$label_y - 0.01,
            xend = .data$xend,
            yend = .data$target_y
          ),
          linewidth = 0.1
        )
    }

    stopifnot(inherits(injury_plot, "ggplot"))
    return(injury_plot)
  }

  guides_mode <- if (isTRUE(show_scale)) "collect" else "keep"
  legend_theme <- if (isTRUE(show_scale)) {
    theme(legend.position = "right")
  } else {
    theme(legend.position = "none")
  }

  # Both view
  if (view_choice == "both") {
    global_max <- max(injury_data[[selected_sport]], na.rm = TRUE)
    if (!is.finite(global_max)) {
      global_max <- 0
    }

    front_plot <- generate_plot(
      "front",
      max_injuries_override = global_max,
      opacity_inner = opacity
    )
    back_plot <- generate_plot(
      "back",
      max_injuries_override = global_max,
      opacity_inner = opacity
    )

    strip_labels <- function(p) {
      keep <- !sapply(p$layers, function(l) {
        inherits(l$geom, "GeomText") || inherits(l$geom, "GeomSegment")
      })
      p$layers <- p$layers[keep]
      p
    }

    front_plot <- strip_labels(front_plot) +
      coord_cartesian(xlim = c(0, 1.00), ylim = c(0, 1.1), clip = "off") +
      theme_void() +
      theme(legend.position = "right", plot.margin = unit(c(0, 0, 0, 0), "pt"))

    back_plot <- strip_labels(back_plot) +
      coord_cartesian(xlim = c(0, 1.00), ylim = c(0, 1.1), clip = "off") +
      theme_void() +
      theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "pt"))

    both_label_positions <- both_label_position_lookup()

    draw_text <- isTRUE(show_labels) || isTRUE(show_values)

    middle_labels <- both_label_positions |>
      left_join(base_data, by = "region_area") |>
      mutate(
        label_text = mapply(
          make_label_text,
          region = region_area,
          value = TotalInjuries,
          USE.NAMES = FALSE
        )
      )

    if (!is.null(unspecified_count)) {
      unspecified_y_both <- if (nrow(middle_labels) > 0) {
        min(middle_labels$label_y, na.rm = TRUE) - 0.08
      } else {
        0.1
      }
      middle_labels <- bind_rows(
        middle_labels,
        data.frame(
          region_area = "Unspecified",
          label_y = unspecified_y_both,
          TotalInjuries = unspecified_count,
          label_text = make_label_text("Unspecified", unspecified_count),
          stringsAsFactors = FALSE
        )
      )
    }

    if (draw_text) {
      front_plot <- front_plot +
        geom_segment(
          data = filter(
            middle_labels,
            !is.na(front_target_x),
            !is.na(front_target_y)
          ),
          aes(
            x = 1.00,
            y = label_y,
            xend = front_target_x,
            yend = front_target_y
          ),
          inherit.aes = FALSE,
          linewidth = 0.3
        )
    }

    if (draw_text) {
      back_plot <- back_plot +
        geom_segment(
          data = filter(
            middle_labels,
            !is.na(back_target_x),
            !is.na(back_target_y)
          ),
          aes(
            x = 0.00,
            y = label_y,
            xend = back_target_x,
            yend = back_target_y
          ),
          inherit.aes = FALSE,
          linewidth = 0.3
        )
    }

    if (!draw_text) {
      middle_plot <- ggplot() + theme_void()
    } else {
      middle_plot <- ggplot(middle_labels) +
        geom_text(
          aes(x = 0.5, y = label_y, label = label_text),
          size = 3.2,
          fontface = "plain",
          hjust = 0.5,
          vjust = 0.5
        ) +
        coord_cartesian(xlim = c(0.45, 0.55), ylim = c(0, 1.1), clip = "off") +
        theme_void() +
        theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
    }

    combined_plot <-
      (front_plot + middle_plot + back_plot) +
      plot_layout(guides = guides_mode, widths = c(1, 0.22, 1), ncol = 3) +
      plot_annotation(
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        )
      ) &
      legend_theme

    attr(combined_plot, "spinviz_heatmap_meta") <- list(
      view_choice = view_choice,
      show_labels = show_labels,
      show_values = show_values,
      show_scale = show_scale
    )

    return(combined_plot)
  } else {
    single_plot <- generate_plot(view_choice) +
      theme(
        plot.margin = margin(t = 15, r = 10, b = 10, l = 10),
        legend.position = if (isTRUE(show_scale)) "right" else "none"
      )

    attr(single_plot, "spinviz_heatmap_meta") <- list(
      view_choice = view_choice,
      show_labels = show_labels,
      show_values = show_values,
      show_scale = show_scale
    )

    return(single_plot)
  }
}
