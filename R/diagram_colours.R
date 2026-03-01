#' @title Available colours in package
#'
#' @description
#' Outputs a vector of hexidecimal colour values if the colour palette is in
#' hcl.pals or viridis.
#'
#' Used to validate the colours that will output in these packages to help
#' Create custom palettes. Advised to output 10 colours, as this is the maximum
#' number of tissue types.
#'
#'
#' @param palette (optional) a string or vector of strings.
#' Either the palette name if using a predefined colour scheme,
#' or a vector or hexidecimal colours if using own colour scheme.
#' Returns a vector of hexidecimal values, or NULL if empty (default colour scheme for plotly)
#' @param n_colours (optional) the number of hexidecimal values to generate
#' standard is 10 as that is the maximum expected tissue types
#' @param n_colors (optional) localisation of n_colours
#'
#' @importFrom viridis viridis
#' @importFrom grDevices hcl.colors hcl.pals
#'
#' @return Returns a vector of hexidecimal colours, or NULL if left blank
#' @export
#' @examples
#' diagram_colours("Reds")




####################################################################
# Function to update colour palette to pre-existing palettes
# Used if the user has not provided their own palette
####################################################################
diagram_colours <- function(palette = "", n_colors = 10, n_colours = n_colors) {
  if (is.null(palette) || identical(palette, "")) return(NULL)

  # Supported viridis options
  viridis_opts <- c("viridis", "magma", "plasma", "inferno",
                    "cividis", "mako", "rocket", "turbo")

  if (is.character(palette) && length(palette) == 1) {
    if (palette %in% viridis_opts) {
      return(viridis::viridis(n = n_colours, option = palette))
    }
    if (palette %in% grDevices::hcl.pals()) {
      return(grDevices::hcl.colors(n = n_colours, palette = palette))
    }
  }

  # Unknown palette name
  NULL
}

# Localisation of diagram_colours.
diagram_colors <- function(palette = "", n_colors = 10, n_colours = n_colors) {
  diagram_colours(palette = palette, n_colours = n_colours)
}

