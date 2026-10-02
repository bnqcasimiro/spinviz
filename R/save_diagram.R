#' @title Save a spinviz diagram to file
#'
#' @description
#' Saves either kind of plot this package produces: a heatmap from
#' \code{\link{heatmap_diagram}} or a sunburst from \code{\link{sunburst_diagram_echarts}},
#' detecting which one \code{plot} is from its class and applying the matching
#' export logic and width:height ratio.
#' \preformatted{
#' p1 <- heatmap_diagram(df, "boxing", "front", sex = "male")
#' save_diagram(p1, file = "heatmap.png", width = 1600, units = "px")
#'
#' p2 <- sunburst_diagram_echarts(df, "boxing")
#' save_diagram(p2, file = "sunburst.jpg", width = 1500)
#' }
#'
#' \strong{Supply \code{width} OR \code{height}} for either diagram type,
#' whichever you don't supply is derived from the
#' relevant fixed ratio so the diagram keeps its proportions. If you supply
#' both and they don't match that ratio: for heatmaps, see
#' \code{enforce_ratio}; for sunbursts, both are used exactly as given (no
#' enforcement).
#'
#' \strong{Export format} is set by the extension in \code{file}. Heatmaps
#' go through \code{\link[ggplot2]{ggsave}}, which accepts any format it
#' supports (.png, .pdf, .svg, .jpg/.jpeg, .tiff/.tif, .bmp, ...). Sunbursts
#' are restricted to .jpg/.jpeg/.png/.pdf, via \code{chromote} directly
#' (Chrome's \code{Page.captureScreenshot} for raster, \code{Page.printToPDF}
#' for PDF.
#'
#' \strong{How each type's ratio is derived} (kept separate, since the two
#' diagrams aren't built the same way):
#' \itemize{
#'   \item \strong{Heatmap}: the body diagram panel itself is always drawn
#'   at a fixed height:width of 1.1:1 (\code{theme(aspect.ratio = 1.1)}
#'   inside \code{\link{heatmap_diagram}}). What
#'   varies is extra width outside that panel: labels/values reserve a
#'   data-width of 1.35 vs. 1.00 with neither shown (exact, from
#'   \code{coord_cartesian(xlim = ...)}), the legend adds an estimated
#'   ~15% extra width if shown (\strong{not} measured from a real render,
#'   a placeholder worth checking against actual output), and
#'   \code{view_choice = "both"} uses the combined front+gap+back width
#'   (1 : 0.10 : 1, exact, from \code{plot_layout(widths = ...)}).
#'   \item \strong{Sunburst}: no fixed ratio is computed at all.
#'   \code{\link{sunburst_diagram_echarts}} records the exact
#'   \code{widget_width}/\code{widget_height} it was built with, and that
#'   recorded ratio is scaled directly.
#' }
#'
#' @param plot The plot/widget object returned by
#' \code{\link{heatmap_diagram}} or \code{\link{sunburst_diagram_echarts}}.
#' @param file Output file path. See Description for which extensions are
#' valid for each diagram type.
#' @param width,height Size of the saved file. Supply at least one, the
#' other is derived from the relevant ratio. Both default to \code{NULL}.
#' @param view_choice \strong{Heatmap only.} One of \code{"front"},
#' \code{"back"}, or \code{"both"}. Left as \code{NULL} (default) to
#' auto-detect from \code{plot}'s recorded metadata.
#' @param show_labels,show_values,show_scale \strong{Heatmap only.}
#' Logical, or \code{NULL} (default) to auto-detect from \code{plot}'s
#' recorded metadata.
#' @param units \strong{Heatmap only.} Units for \code{width}/\code{height}:
#' \code{"px"} (default), \code{"in"}, \code{"cm"}, or \code{"mm"}.
#' @param dpi \strong{Heatmap only.} Resolution in dots per inch, for
#' raster formats only (no effect on .pdf/.svg/.eps). Default 300.
#' @param enforce_ratio \strong{Heatmap only.} Logical. If \code{TRUE}
#' (default) and you supply both \code{width} and \code{height} with a
#' mismatched ratio, \code{height} is recalculated to match and a warning
#' explains why. Set \code{FALSE} to deliberately stretch the diagram.
#' @param bg \strong{Heatmap only.} Background colour. Default
#' \code{"white"}. \code{\link{heatmap_diagram}} uses \code{theme_void()},
#' which has no background fill at all, and many image viewers render that
#' transparency as solid grey rather than white. Set to \code{NA} for a
#' genuinely transparent export.
#' @param delay \strong{Sunburst only.} Seconds to wait after the page
#' loads before capturing, giving ECharts time to finish drawing
#' (especially its label layout pass). Default 1.
#' @param ... \strong{Heatmap only.} Further arguments passed to
#' \code{\link[ggplot2]{ggsave}}.
#'
#' @return Invisibly, the file path written to.
#' @seealso \code{\link{heatmap_diagram}}, \code{\link{sunburst_diagram_echarts}}
#' @export
#'
#' @examples
#' subcategory <- c("Head","Neck","Shoulder","Chest","Upper Arm","Elbow",
#'                   "Abdomen","Forearm","Hip Groin","Wrist","Hand",
#'                   "Thigh","Knee","Lower Leg","Ankle","Foot","Thoracic Spine","Lumbosacral")
#' region_area <- rep("Example", length(subcategory))
#' boxing <- c(15, 5, 18, 12, 20, 6, 10, 14, 9, 9, 11, 3, 16, 13, 7, 8, 18, 22)
#' df_heatmap <- data.frame(region_area, subcategory, boxing)
#'
#' tissue <- c('Muscle/tendon','Muscle/tendon','Nervous','Bone','Bone',
#'             'Cartilage/Synovium/Bursa','Ligament/Joint capsule',
#'             'Superficial tissues/skin','Vessels','Internal organs')
#' pathology <- c('Muscle injury','Tendon rupture','Peripheral nerve injury',
#'                'Fracture','Bone contusion','Cartilage injury','Joint sprain',
#'                'Laceration','Vascular trauma','Organ trauma')
#' injuries <- c(20, 10, 16, 16, 10, 67, 27, 82, 26, 34)
#' df_sunburst <- data.frame(tissue, pathology, injuries)
#'
#' \dontrun{
#' # Heatmap: type detected automatically, ratio comes from heatmap_diagram()'s
#' # own recorded metadata (view_choice, show_labels, show_values, show_scale)
#' p1 <- heatmap_diagram(df_heatmap, "boxing", "front", sex = "male")
#' save_diagram(p1, file = "heatmap.png", width = 1600, units = "px")
#'
#' # "both" view, exported as PDF
#' p2 <- heatmap_diagram(df_heatmap, "boxing", "both", sex = "male")
#' save_diagram(p2, file = "heatmap_both.pdf", width = 2400, units = "px")
#'
#' # Sunburst: ratio comes from sunburst_diagram_echarts()'s own recorded
#' # widget_width/widget_height instead; no units argument needed (always px)
#' p3 <- sunburst_diagram_echarts(df_sunburst, "injuries")
#' save_diagram(p3, file = "sunburst.jpg", width = 1500)
#'
#' # Sunburst as PDF: same call shape again, just a different extension
#' save_diagram(p3, file = "sunburst.pdf", width = 1500)
#'
#' # Overriding recorded metadata explicitly (e.g. plot came from an older
#' # version of heatmap_diagram() with no attached metadata)
#' save_diagram(p1, file = "heatmap_custom.png",
#'              view_choice = "front", show_labels = TRUE,
#'              show_values = FALSE, show_scale = TRUE,
#'              width = 1200, units = "px")
#' }
save_diagram <- function(
  plot,
  file,
  width = NULL,
  height = NULL,
  # heatmap-only arguments
  view_choice = NULL,
  show_labels = NULL,
  show_values = NULL,
  show_scale = NULL,
  units = "px",
  dpi = 300,
  enforce_ratio = TRUE,
  bg = "white",
  # sunburst-only argument
  delay = 1,
  ...
) {
  if (is.null(width) && is.null(height)) {
    stop("Supply at least one of `width` or `height`.", call. = FALSE)
  }

  is_heatmap <- inherits(plot, "ggplot") || inherits(plot, "patchwork")
  is_sunburst <- inherits(plot, "htmlwidget")

  if (!is_heatmap && !is_sunburst) {
    stop(
      "save_diagram() doesn't recognise this object's type. Expected the ",
      "result of heatmap_diagram() (a ggplot/patchwork object) or ",
      "sunburst_diagram_echarts() (an htmlwidget), but got an object of ",
      "class: ",
      paste(class(plot), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # ===========================================================================
  # HEATMAP branch
  # ===========================================================================
  if (is_heatmap) {
    meta <- attr(plot, "spinviz_heatmap_meta")

    if (is.null(view_choice)) {
      if (is.null(meta)) {
        stop(
          "`plot` has no recorded view_choice metadata (it may have come ",
          "from an older version of heatmap_diagram()). Pass view_choice ",
          "explicitly: \"front\", \"back\", or \"both\".",
          call. = FALSE
        )
      }
      view_choice <- meta$view_choice
    }
    view_choice <- match.arg(view_choice, c("front", "back", "both"))

    if (is.null(show_labels)) {
      show_labels <- if (!is.null(meta)) meta$show_labels else TRUE
    }
    if (is.null(show_values)) {
      show_values <- if (!is.null(meta)) meta$show_values else TRUE
    }
    if (is.null(show_scale)) {
      show_scale <- if (!is.null(meta)) meta$show_scale else TRUE
    }

    # dpi only matters for raster formats. Flag it if set on a vector export
    vector_exts <- c("pdf", "svg", "eps")
    file_ext <- tolower(tools::file_ext(file))
    if (file_ext %in% vector_exts && !missing(dpi)) {
      message(
        "Note: dpi has no effect for vector formats like .",
        file_ext,
        "It only applies to raster formats (png, jpg/jpeg, tiff/tif, bmp)."
      )
    }

    # ---- heatmap's fixed export ratio ----------------------------------
    PANEL_HEIGHT <- 1.1
    WIDTH_WITH_LABELS <- 1.35
    WIDTH_NO_LABELS <- 1.00
    LEGEND_WIDTH_ALLOWANCE <- 0.15

    draw_labels <- isTRUE(show_labels) || isTRUE(show_values)

    if (view_choice == "both") {
      # Fixed 16:9 aspect ratio for the complete both-view composition.
      # Changing the export width therefore scales the height proportionally.
      ratio <- 9 / 16
    } else {
      base_width <- if (draw_labels) {
        WIDTH_WITH_LABELS
      } else {
        WIDTH_NO_LABELS
      }

      total_width <-
        base_width +
        if (isTRUE(show_scale)) LEGEND_WIDTH_ALLOWANCE else 0

      ratio <- PANEL_HEIGHT / total_width
    }

    if (!is.null(width) && !is.null(height)) {
      expected_height <- width * ratio
      if (isTRUE(enforce_ratio) && abs(height - expected_height) > 1e-6) {
        warning(
          "height (",
          height,
          ") doesn't match the fixed ratio for this ",
          "view_choice/show_labels/show_values/show_scale combination; ",
          "overriding to ",
          round(expected_height, 3),
          " to keep the body ",
          "diagram proportional. Set enforce_ratio = FALSE to override this ",
          "intentionally.",
          call. = FALSE
        )
        height <- expected_height
      }
    } else if (is.null(height)) {
      height <- width * ratio
    } else {
      width <- height / ratio
    }

    if (
      view_choice == "both" &&
        units == "px" &&
        file_ext %in% c("png", "jpg", "jpeg", "tiff", "tif", "bmp")
    ) {
      native_width <- 3200
      native_height <- 1800

      tmp_file <- tempfile(fileext = paste0(".", file_ext))

      ggplot2::ggsave(
        filename = tmp_file,
        plot = plot,
        width = native_width,
        height = native_height,
        units = "px",
        dpi = dpi,
        bg = bg,
        ...
      )

      img <- magick::image_read(tmp_file)
      img <- magick::image_resize(
        img,
        paste0(round(width), "x", round(height), "!")
      )

      magick::image_write(img, path = file)

      unlink(tmp_file)
    } else {
      ggplot2::ggsave(
        filename = file,
        plot = plot,
        width = width,
        height = height,
        units = units,
        dpi = dpi,
        bg = bg,
        ...
      )
    }

    return(invisible(file))
  }

  # ===========================================================================
  # SUNBURST branch
  # ===========================================================================

  raster_exts <- c("jpg", "jpeg", "png")
  file_ext <- tolower(tools::file_ext(file))
  if (!file_ext %in% c(raster_exts, "pdf")) {
    stop(
      "save_diagram() only supports .jpg/.jpeg/.png/.pdf output for ",
      "sunburst diagrams (got .",
      file_ext,
      ").",
      call. = FALSE
    )
  }
  is_pdf <- file_ext == "pdf"

  # ---- sunburst's own ratio: scaled directly from its recorded build size
  meta <- attr(plot, "spinviz_sunburst_meta")
  if (is.null(meta)) {
    stop(
      "`plot` has no recorded width/height metadata (it may have come ",
      "from an older version of sunburst_diagram_echarts()). This function ",
      "needs the widget's own native build size to render without ",
      "clipping, so this can't proceed without it. Rebuild `plot` with ",
      "a current version of sunburst_diagram_echarts().",
      call. = FALSE
    )
  }

  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop(
      "Package 'chromote' is required to save a sunburst diagram. ",
      "Install it with install.packages('chromote').",
      call. = FALSE
    )
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop(
      "Package 'base64enc' is required to save a sunburst diagram. ",
      "Install it with install.packages('base64enc').",
      call. = FALSE
    )
  }

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop(
      "Package 'magick' is required to save a sunburst diagram. ",
      "Install it with install.packages('magick').",
      call. = FALSE
    )
  }

  to_px <- function(x) {
    if (is.character(x)) as.numeric(sub("px$", "", x)) else x
  }

  native_width <- to_px(meta$widget_width)
  native_height <- to_px(meta$widget_height)
  ratio <- native_height / native_width

  if (is.null(width) && !is.null(height)) {
    width <- to_px(height) / ratio
  } else if (!is.null(width) && is.null(height)) {
    height <- to_px(width) * ratio
  } else {
    width <- to_px(width)
    height <- to_px(height)
  }

  width <- round(width)
  height <- round(height)

  export_widget <- plot
  if (!is.null(export_widget$x$opts$toolbox)) {
    export_widget$x$opts$toolbox$show <- FALSE
  }

  tmp_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(export_widget, tmp_html, selfcontained = TRUE)
  on.exit(unlink(tmp_html), add = TRUE)

  session <- chromote::ChromoteSession$new()
  on.exit(session$close(), add = TRUE)

  session$Emulation$setDeviceMetricsOverride(
    width = native_width,
    height = native_height,
    deviceScaleFactor = 1,
    mobile = FALSE
  )

  session$Page$navigate(paste0("file://", tmp_html), wait_ = TRUE)
  Sys.sleep(delay)

  if (is_pdf) {
    px_to_in <- function(px) px / 96

    print_scale <- width / native_width

    result <- session$Page$printToPDF(
      landscape = FALSE,
      printBackground = TRUE,
      paperWidth = px_to_in(width),
      paperHeight = px_to_in(height),
      marginTop = 0,
      marginBottom = 0,
      marginLeft = 0,
      marginRight = 0,
      scale = print_scale
    )

    writeBin(
      base64enc::base64decode(result$data),
      file
    )
  } else {
    capture_format <- if (file_ext == "png") "png" else "jpeg"

    result <- session$Page$captureScreenshot(
      format = capture_format,
      quality = if (capture_format == "jpeg") 100 else NULL,
      clip = list(
        x = 0,
        y = 0,
        width = native_width,
        height = native_height,
        scale = 1
      ),
      captureBeyondViewport = TRUE
    )

    # Read native screenshot
    native_bytes <- base64enc::base64decode(result$data)
    img <- magick::image_read(native_bytes)

    # Crop empty space around the rendered diagram
    img <- magick::image_trim(img)

    # Add a small white margin back around the cropped diagram
    trimmed_info <- magick::image_info(img)

    padding <- 40

    img <- magick::image_extent(
      img,
      geometry = paste0(
        trimmed_info$width + 2 * padding,
        "x",
        trimmed_info$height + 2 * padding
      ),
      gravity = "center",
      color = "white"
    )

    # Get dimensions after cropping
    cropped_info <- magick::image_info(img)
    cropped_width <- cropped_info$width
    cropped_height <- cropped_info$height
    cropped_ratio <- cropped_height / cropped_width

    # Scale proportionally using whichever dimension the user supplied
    if (!is.null(width) && is.null(height)) {
      width <- round(to_px(width))
      height <- round(width * cropped_ratio)
    } else if (is.null(width) && !is.null(height)) {
      height <- round(to_px(height))
      width <- round(height / cropped_ratio)
    } else {
      width <- round(to_px(width))
      height <- round(to_px(height))
    }

    img <- magick::image_resize(
      img,
      paste0(width, "x", height)
    )

    magick::image_write(
      img,
      path = file,
      format = capture_format
    )
  }

  invisible(file)
}
