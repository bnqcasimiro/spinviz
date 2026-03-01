#' @title Test colour palette(s)
#'
#' @description
#' Function to help test colour palettes by displaying pie chart(s) with the
#' palette name and hexidecimal values next to each colour.
#'
#' Can accept any palette in hcl.pals() or the viridis package,
#' or a custom palette.
#'
#'
#' @param palettes (required) the palette(s) to test. Can be a vector
#' of hexidecimal values (custom palette), a palette available in hcl.pals() or viridis,
#' or a list of palettes. Must be a list if multiple palettes are being tested.
#'
#' Will output a pie chart with 1-10 if a palette cannout be found.
#'
#' @param n_colours (optional) the number of colours to display in each pie chart
#' @param n_colors (optional) localisation of n_colours
#' @return a diagram of pie charts displaying the colour schemes
#' @export
#'
#' @examples
#' # values from hcl.pals()
#' test_colour("Greens")
#' test_colour(list("Reds","Blues"))
#' # values from viridis, localised to color
#' test_colour("Grays")
#'
#' # Custom colour palette
#' custom_palette <- c("#cba6f7", "#f38ba8", "#eba0ac", "#f9e2af", "#fab387",
#' "#a6e3a1", "#94e2d5", "#74c7ec", "#89b4fa", "#b4befe")
#' test_colour(custom_palette)
#'
#' # Multiple custom and existing palettes
#' custom_palette <- c("#cba6f7", "#f38ba8", "#eba0ac", "#f9e2af", "#fab387",
#' "#a6e3a1", "#94e2d5", "#74c7ec", "#89b4fa", "#b4befe")
#' custom_palette_2 <- c("#8839ef", "#d20f39", "#e64553", "#df8e1d", "#fe640b",
#' "#40a02b", "#179299", "#209fb5", "#1e66f5", "#7287fd")
#' test_colour(list(custom_palette,"Blues", "Greens", custom_palette_2))
#' # All hcl colours
#' test_colour(hcl.pals())
#' # All coloublind palettes
#' viridis_palettes <- list("magma", "inferno", "plasma", "viridis", "cividis",
#'  "rocket", "mako", "turbo")
test_colour <- function(palettes = "", n_colors = 10, n_colours = n_colors) {

  is_hex_vec <- function(x) {
    x <- as.character(x)
    any(stringr::str_detect(x, "^#"))
  }

  draw_pie <- function(cols, title) {
    cols <- as.character(cols)
    cols <- cols[!is.na(cols)]
    if (length(cols) == 0) return(invisible(NULL))
    values <- rep(1, length(cols))
    graphics::pie(values, labels = cols, main = title, col = cols)
  }

  # ---- Layout ----
  if (is.list(palettes)) {
    num_palettes <- length(palettes)
    if (num_palettes > 6) {
      graphics::par(mfrow = c(3, 3))
    } else {
      graphics::par(mfrow = c(3, max(1, ceiling(num_palettes / 3))))
    }
  } else {
    graphics::par(mfrow = c(1, 1))
  }

  # ---- Draw ----
  if (is.list(palettes)) {
    i <- 1
    for (p in palettes) {
      if (is_hex_vec(p)) {
        draw_pie(p, paste0("custom palette ", i))
        i <- i + 1
      } else {
        cols <- diagram_colours(p, n_colours = n_colours)
        draw_pie(cols, as.character(p))
      }
    }

  } else if (is_hex_vec(palettes)) {
    # single custom palette vector
    draw_pie(palettes, "custom palette")

  } else if (length(palettes) > 1) {
    # vector of hcl palette names
    for (pal_name in palettes) {
      cols <- grDevices::hcl.colors(n = n_colours, palette = pal_name)
      draw_pie(cols, pal_name)
    }

  } else {
    # single palette name (viridis/hcl)
    cols <- diagram_colours(palettes, n_colours = n_colours)
    draw_pie(cols, palettes)
  }
}
