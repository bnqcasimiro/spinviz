# Available colours in package

Outputs a vector of hexidecimal colour values if the colour palette is
in hcl.pals or viridis.

Used to validate the colours that will output in these packages to help
Create custom palettes. Advised to output 10 colours, as this is the
maximum number of tissue types.

## Usage

``` r
diagram_colours(palette = "", n_colors = 10, n_colours = n_colors)
```

## Arguments

- palette:

  (optional) a string or vector of strings. Either the palette name if
  using a predefined colour scheme, or a vector or hexidecimal colours
  if using own colour scheme. Returns a vector of hexidecimal values, or
  NULL if empty (default colour scheme for plotly)

- n_colors:

  (optional) localisation of n_colours

- n_colours:

  (optional) the number of hexidecimal values to generate standard is 10
  as that is the maximum expected tissue types

## Value

Returns a vector of hexidecimal colours, or NULL if left blank

## Details

Valid palette inputs:

- Viridis options: `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`,
  `"cividis"`, `"mako"`, `"rocket"`, `"turbo"`.

- Any palette name returned by
  [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html).

To see available HCL palettes, run
[`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html).

## Examples

``` r
diagram_colours("Reds")
#>  [1] "#FCF5F2" "#FFE2D7" "#FFC8B5" "#FFAA93" "#FF8870" "#FB604F" "#EB2C31"
#>  [8] "#C11437" "#970031" "#6D0026"
```
