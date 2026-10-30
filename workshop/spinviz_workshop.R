# =============================================================================
#  spinviz workshop
# =============================================================================
#
#  What you'll make today:
#    Part A  - a body HEATMAP   (what body regions injuries occur)
#    Part B  - a SUNBURST chart (which tissues and pathologies are injured)
#
#  The workflow is the same for both:
#    1. Get your data into R, either:
#         a) create a template CSV, fill it in, then read it back in, or
#         b) use the example data frame that comes with spinviz
#    2. Plot it
#    3. Save it with your chosen file type
#
#
#  Before you start: work through section 0 (Setup) below.
#
#  How to use this script:
#    Run it one line at a time (select the line you want to run and press the
#    "Run" button on top, or Ctrl+Enter / Cmd+Enter). Read the comments as you go.
#    Lines marked  # TRY:  are small experiments for you to change.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Setup (do this once)
# -----------------------------------------------------------------------------

# --- Step 1: Install spinviz -------------------------------------------------

# Using Posit Cloud?
# spinviz is already installed in the workshop project,
# so skip this step and go straight to Step 3 (library(spinviz)).

# Using R on your own device?
# Run these two lines. Take out the # at the start of the first line if you
# don't have pak installed yet.
# When prompted "? Do you want to continue (Y/n)", type and enter Y

# install.packages("pak")
pak::pak("bnqcasimiro/spinviz")



# --- Step 2: Packages for saving sunburst images ------------------------------

# Using Posit Cloud?
# Skip this step. Exporting sunbursts as static images is not supported in
# this Posit Cloud workshop environment. Instead, save your sunburst as an
# interactive .html file (see Section B6).

# Using R on your own device?
# Saving a sunburst as .png/.jpg/.pdf needs two extra packages
# AND Google Chrome installed on your computer.
# Remove the # and run the line below to download the needed packages.

# install.packages(c("chromote", "base64enc"))


# --- Step 3: Load spinviz ----------------------------------------------------
# Run this every time you open R or restart your session.

library(spinviz)


# --- Step 4: Check where your files will be saved ----------------------------
# Your CSVs and images are saved in R's working folder. Run this to see it:

getwd()




# =============================================================================
#  PART A: BODY HEATMAP (body injuries)
# =============================================================================

# -----------------------------------------------------------------------------
# A1. See the body categories listed on spinviz
# -----------------------------------------------------------------------------
# Each row is one body area ("body_area") and the broader region it belongs
# to ("body_region"). Your data must use these exact spellings.

body_categories


# -----------------------------------------------------------------------------
# A2. Create a template CSV
# -----------------------------------------------------------------------------
# create_injury_template() writes a CSV with one pre-set row for each body
# area (body_region and body_area columns already filled in), plus one EMPTY
# column for each group you list.
#
# You choose the groups. They control how many empty columns you get:
#   group = "boxing"                      -> 1 empty column
#   group = c("boxing", "judo")           -> 2 empty columns (this example)
#   group = c("male", "female", "total")  -> 3 empty columns
# A group can be a sport, a sex, a year, a team... anything you compare.
#
# IMPORTANT: creating a template OVERWRITES any file with the same name.
# Only run this line ONCE, or you'll wipe the data you've filled in.

create_injury_template("body_injuries_template.csv", group = c("boxing", "judo"))

# Where is the file?
#   Posit Cloud: Files tab (bottom-right pane) > Cloud > project. Click the
#                circular refresh arrow if it isn't showing yet.
#   Own device:  in R's working folder (run getwd() to see where).


# -----------------------------------------------------------------------------
# A3. Fill in the template (outside R)
# -----------------------------------------------------------------------------
# 1. Open the CSV in a spreadsheet editor (e.g. Excel).
#      Posit Cloud: first download it - tick the box next to the file >
#      More (gear icon) > Export.
# 2. Type an injury count for each body area under each group column:
#      - whole numbers only (0, 1, 2, ...) - no decimals or negatives
#      - leave a cell blank if there's no data
#      - do NOT rename or delete the 'body_region' / 'body_area' columns,
#        or change the body area names
# 3. Save it as "CSV (Comma delimited)" - NOT "CSV UTF-8".
# 4. Posit Cloud: click Upload in the Files pane to bring it back.
#
# For today's workshop, SKIP filling it in - we'll use the example data
# in A4 instead.


# -----------------------------------------------------------------------------
# A4. Read your data into R
# -----------------------------------------------------------------------------
# Choose ONE of the two options below. Both give you a data frame called
# body_data, which the rest of Part A uses.
#
# Your own filled-in file would be read the same way as Option 1, e.g.
#   body_data <- read_injury_data("body_injuries_template.csv")


# --- Option 1: read the example CSV ------------------------------------------
# body_injuries_example.csv is the template from A2, already filled in with
# boxing and judo injury counts from Lystad et al. (2021).

# Using Posit Cloud?
# The example CSV is already in your project files (see the Files pane),
# so skip the download and go straight to read_injury_data() below.

# Using R on your own device?
# Run this first to download the example CSV into your working folder
# (or download it manually from the spinviz website):

download.file(
  "https://bnqcasimiro.github.io/spinviz/workshop/body_injuries_example.csv",
  destfile = "body_injuries_example.csv"
)


# Read it in. read_injury_data() checks the columns, flags misspelled body
# areas, and stops if any count isn't a whole number.
body_data <- read_injury_data("body_injuries_example.csv")


# --- Option 2: use the built-in example data frame ---------------------------
# No file needed: body_df comes with spinviz and contains made-up example
# counts, so plots will differ from Option 1.

body_data <- body_df


# Either way, have a look at it:
body_data
names(body_data)   # the group columns you can plot


# -----------------------------------------------------------------------------
# A5. Plot the heatmap
# -----------------------------------------------------------------------------
# plot_heatmap() draws your injury counts onto a body diagram. Darker
# colours = more injuries.

# plot_heatmap(data, group, view, sex, value_format, ...)
#   data         - your data frame (today: body_data)
#   group        - name of the group column to plot, in quotes,
#                  e.g. "boxing", "female", "2026 Olympics"
#   view         - which side of the body: "anterior", "posterior", or "both"
#   sex          - body figure to draw on: "male" or "female"

# Optional styling:
#   palette      - colour scheme, e.g. "viridis", "Reds", "Blues 3", etc.
#                  To see available colours, run ?diagram_colours
#   opacity      - how strong the colours are, from 0 (clear) to 1 (solid)
#   value_format - "frequency" (default) shows injury numbers;
#                  "proportion" shows each area's share of all injuries (%)
#   show_labels  - TRUE/FALSE: show body area names
#   show_values  - TRUE/FALSE: show the numbers
#   show_legend  - TRUE/FALSE: show the colour legend

# Tip: some body areas only appear on one side (e.g. Abdomen is anterior-only,
# Thoracic spine is posterior-only). Use "both" to see everything at once.


# --- Worked example ----------------------------------------------------------
# Run this line and look at the plot in the Plots pane (bottom right).
# Will show a heatmap plot for boxing data, anterior + male view

plot_heatmap(data = body_data, group = "boxing", view = "anterior", sex = "male")


# --- Activity 1: Fill in the blanks -----------------------------------------
# Replace each ___ so the code runs. (Running it with ___ still in it will
# give an error; that's expected!)

# 1a. Plot the POSTERIOR of the body for boxing, on a FEMALE figure.
plot_heatmap(data = body_data, group = "boxing", view = "___", sex = "___")

# 1b. Plot BOTH sides of the body for boxing, on a MALE figure.
plot_heatmap(data = body_data, group = "boxing", view = "___", sex = "___")

# 1c. Same as 1b, but show each area's PROPORTION of all injuries instead
#     of raw counts.
plot_heatmap(data = body_data, group = "boxing", view = "both", sex = "male",
             value_format = "___")


# --- Activity 2: Compare groups ---------------------------------------------
# Using Option 1? body_data has two groups: boxing and judo.
# (body_df only has boxing, so skip this activity if you used Option 2.)

# 2a. Plot BOTH sides of the body for the SECOND group (judo).
plot_heatmap(data = body_data, group = "___", view = "both", sex = "male")


# --- Activity 3: Make it your own -------------------------------------------
# Fill in the blanks to customise the look.

my_heatmap <- plot_heatmap(
  data           = body_data,
  group          = "boxing",
  view           = "___",
  sex            = "___",     # "male" or "female"
  palette        = "___",     # pick a palette name (?diagram_colours)
  opacity        = ___,       # a number between 0 and 1
  show_labels    = ___,       # TRUE or FALSE
  show_values    = ___,       # TRUE or FALSE
  show_legend    = ___        # TRUE or FALSE
)
my_heatmap




# --- Answers (try first, then check) ---------------------------------------
# 1a. plot_heatmap(data = body_data, group = "boxing", view = "posterior", sex = "female")
# 1b. plot_heatmap(data = body_data, group = "boxing", view = "both", sex = "male")
# 1c. plot_heatmap(data = body_data, group = "boxing", view = "both", sex = "male",
#                  value_format = "proportion")

# 2a. plot_heatmap(data = body_data, group = "judo", view = "both", sex = "male")




# -----------------------------------------------------------------------------
# A6. Save the heatmap
# -----------------------------------------------------------------------------
# Step 1: assign the plot to an object.
# Putting "heat_plot <-" in front stores the plot under the name heat_plot
# instead of only showing it. save_diagram() needs this object to save.

heat_plot <- plot_heatmap(data = body_data, group = "boxing", view = "both",
                          sex = "male")
heat_plot   # type the name to show it again

# Want to save your Activity 3 design instead? Use plot = my_heatmap below.

# Step 2: save it.
# Images are saved in the same folder as your CSV files (run getwd() to see
# where). On Posit Cloud, find them in the Files pane and use More > Export
# to download them.
# Give either width or height; spinviz keeps the right proportions.
# Any ggsave format works: .png, .jpg, .pdf, .svg, .tiff ...

save_diagram(plot  = heat_plot,
             file  = "heatmap_boxing.png",
             width = 20, units = "cm")

save_diagram(plot  = heat_plot,
             file  = "heatmap_boxing.pdf",
             width = 2000, units = "px")



# =============================================================================
#  PART B: SUNBURST (tissue and pathology)
# =============================================================================

# -----------------------------------------------------------------------------
# B1. See the tissue/pathology categories listed on spinviz
# -----------------------------------------------------------------------------
# Each row is one pathology type ("pathology_type") and the tissue it
# belongs to ("tissue_type"). Your data must use these exact spellings.

injury_categories


# -----------------------------------------------------------------------------
# B2. Create a template CSV
# -----------------------------------------------------------------------------
# Same idea as A2: create_sunburst_template() writes one pre-set row per
# tissue/pathology type, plus one EMPTY column for each group you list.
# Again, your groups decide how many empty columns you get - here, two.
#
# IMPORTANT: this OVERWRITES any file with the same name. Only run it once.

create_sunburst_template("tissue_injuries_template.csv", group = c("boxing", "judo"))


# -----------------------------------------------------------------------------
# B3. Fill in the template (outside R)
# -----------------------------------------------------------------------------
# Same steps as A3: download (Posit Cloud), open in a spreadsheet editor,
# type whole-number counts under each group, leave blanks where there's no
# data, keep the 'tissue_type' / 'pathology_type' columns and names as
# they are, save as "CSV (Comma delimited)", then upload it back.
#
# For today's workshop, SKIP filling it in - we'll use the example data
# in B4 instead.


# -----------------------------------------------------------------------------
# B4. Read your data into R
# -----------------------------------------------------------------------------
# Choose ONE of the two options below. Both give you a data frame called
# tissue_data, which the rest of Part B uses.
#
# Your own filled-in file would be read the same way as Option 1, e.g.
#   tissue_data <- read_sunburst_data("tissue_injuries_template.csv")


# --- Option 1: read the example CSV ------------------------------------------
# tissue_injuries_example.csv is the template from B2, already filled in with
# boxing and judo injury counts from Lystad et al. (2021).

# Using Posit Cloud?
# The example CSV is already in your project files (see the Files pane),
# so skip the download and go straight to read_sunburst_data() below.

# Using R on your own device?
# Run this first to download the example CSV into your working folder
# (or download it manually from the spinviz website):

download.file(
  "https://bnqcasimiro.github.io/spinviz/workshop/tissue_injuries_example.csv",
  destfile = "tissue_injuries_example.csv"
)


# Read it in. read_sunburst_data() checks the columns, flags misspelled
# tissue/pathology types, and stops if any count isn't a whole number.
tissue_data <- read_sunburst_data("tissue_injuries_example.csv")


# --- Option 2: use the built-in example data frame ---------------------------
# No file needed: tissue_df comes with spinviz and contains made-up example
# counts, so plots will differ from Option 1.

tissue_data <- tissue_df


# Either way, have a look at it:
tissue_data
names(tissue_data)   # the group columns you can plot


# -----------------------------------------------------------------------------
# B5. Plot the sunburst
# -----------------------------------------------------------------------------
# plot_sunburst() draws your injury counts as rings. The inner ring shows
# TISSUE type (e.g. Bone, Ligament); the outer ring breaks each tissue into
# PATHOLOGY types (e.g. Fracture, Joint sprain). Bigger slices = more injuries.
#
# plot_sunburst(data, group, ...)
#   data         - your data frame (today: tissue_data)
#   group        - name of the group column to plot, in quotes, e.g. "boxing"
#
# Optional styling:
#   palette              - colour scheme, e.g. "Dark 3", "viridis", "Set 3", etc.
#                          To see available colours, run ?diagram_colours
#   opacity              - how strong the colours are, from 0 (clear) to 1 (solid)
#   show_outer_labels    - TRUE/FALSE: show pathology names on the outer ring
#   show_values          - TRUE/FALSE: show counts (n = ...) beside labels
#                          (default FALSE)
#   depth                - 1 shows the tissue ring only; 0 or leave out to show
#                          both rings
#   inner_font_size      - text size for tissue labels
#   outer_font_size      - text size for pathology labels
#   inner_font_color     - font colour for tissue labels, e.g. "white"
#   outer_font_color     - font colour for pathology labels, e.g. "black"


# Note: blank cells in the example data are treated as 0, so you'll see the
# message "null values found in dataset. Changed to 0". That's expected.


# --- Worked example ----------------------------------------------------------
# Run this line and look at the chart in the Viewer pane (bottom right).
# Will show a sunburst chart for boxing data, with both rings

plot_sunburst(data = tissue_data, group = "boxing")


# --- Activity 1: Fill in the blanks -----------------------------------------
# Replace each ___ so the code runs. (Running it with ___ still in it will
# give an error; that's expected!)

# 1a. Plot boxing but SHOW the values.
plot_sunburst(data = tissue_data, group = "boxing", show_values = ___)

# 1b. Plot boxing showing the TISSUE ring only.
plot_sunburst(data = tissue_data, group = "boxing", depth = ___)

# 1c. Plot boxing but change the colours to viridis.
plot_sunburst(data = tissue_data, group = "boxing", palette = "___")


# --- Activity 2: Compare groups ---------------------------------------------
# tissue_data has more than one group. Not sure what they are? Run: names(tissue_data)

# 2a. Plot both rings for the SECOND group.
plot_sunburst(data = tissue_data, group = "___")


# --- Activity 3: Make it your own -------------------------------------------
# Fill in the blanks to customise the look.

my_sunburst <- plot_sunburst(
  data                 = tissue_data,
  group                = "boxing",
  palette              = "___",     # pick a palette name (?diagram_colours)
  opacity              = ___,       # a number between 0 and 1
  show_outer_labels    = ___,       # TRUE or FALSE
  show_values          = ___,       # TRUE or FALSE
  depth                = ___,       # 0 or 1
  inner_font_size      = 14,
  outer_font_size      = 11
)
my_sunburst




# --- Answers (try first, then check) ---------------------------------------
# 1a. plot_sunburst(data = tissue_data, group = "boxing", show_values = TRUE)
# 1b. plot_sunburst(data = tissue_data, group = "boxing", depth = 1)
# 1c. plot_sunburst(data = tissue_data, group = "boxing", palette = "viridis")

# 2a. plot_sunburst(data = tissue_data, group = "judo")



# -----------------------------------------------------------------------------
# B6. Save the sunburst
# -----------------------------------------------------------------------------
# Step 1: assign the plot to an object, just like in A6.

sun_plot <- plot_sunburst(data = tissue_data, group = "boxing",
                          show_values = TRUE)
sun_plot   # type the name to show it again

# Want to save your Activity 3 design instead? Use plot = my_sunburst below.


# Step 2: save it.

# --- Option 1: Interactive web page (.html) ---------------------------------
# Using Posit Cloud? Use this option. Option 2 is not supported on Posit Cloud.
# Also recommended for sharing: the saved file keeps hover tooltips and can
# be opened in any web browser.

htmlwidgets::saveWidget(sun_plot, "sunburst_boxing.html")


# --- Option 2: Static image (.png, .jpg, .pdf) ------------------------------
# Using R on your own device? You can use this option (not Posit Cloud).
# Requires the chromote and base64enc packages and a local installation of
# Google Chrome (see Setup, Step 2).

save_diagram(plot  = sun_plot,
             file  = "sunburst_boxing.png",
             width = 2000)

save_diagram(plot  = sun_plot,
             file  = "sunburst_boxing.pdf",
             width = 2000)



# =============================================================================
#  Done! Check your folder:
# =============================================================================
# Your CSVs and images are all saved in R's working folder (run getwd() to
# see where). On Posit Cloud, find them in the Files pane and use
# More > Export to download them.

list.files()




# -----------------------------------------------------------------------------
# Troubleshooting
# -----------------------------------------------------------------------------
# --- Reading your CSV (A4 / B4) ---

# download.file() fails / "cannot open URL"
#   -> Check your internet connection, or download the example CSV from the
#      spinviz website yourself and put it in your working folder. Or skip
#      the CSV and use Option 2 (body_df / tissue_df).

# "File does not exist"
#   -> The file name doesn't match, or the file isn't in your working
#      folder. Check spelling (including .csv) and run list.files().

# "Missing required column(s): ..."
#   -> A column was renamed or deleted in Excel, or the file was saved as
#      "CSV UTF-8" (adds a hidden character to the first column name).
#      Re-save as "CSV (Comma delimited)".

# "No group column found"
#   -> The file has no injury count columns. Check your groups were added
#      in create_*_template(), or add a column with the group name in Excel.

# "must contain whole-number injury counts"
#   -> A cell has a decimal (e.g. 2.5) or a negative number. Injury counts
#      must be whole numbers: 0, 1, 2, ...

# "Unrecognised body_area value(s)"                        (heatmap)
# "Unrecognised tissue_type / pathology_type value(s)"     (sunburst)
#   -> A body area, tissue or pathology name was changed or misspelled.
#      Compare against body_categories or injury_categories.
#      If your CSV is correct but this still appears, reinstall spinviz
#      and restart R (Session > Restart R).

# "unused argument (group = ...)"
#   -> You have an older version of spinviz. Re-run Step 1 of Setup, then
#      Session > Restart R.


# --- Plotting (A5 / B5) ---

# "...could not be converted to numeric and were set to NA"
#   -> A cell has text instead of a number (e.g. "12 injuries", "N/A").
#      Fix it in Excel so each cell is just a number.

# "no non-missing arguments to max; returning -Inf"   (heatmap)
# "Sum of column equals 0"                            (sunburst)
#   -> The group column is empty. Most often you re-ran create_*_template(),
#      which overwrote your filled-in file. Fill it in again and only run
#      read_*() and plot_*().

# "Column name does not exist in dataset"        (sunburst)
#   -> The group name doesn't match a column. Check spelling and capitals
#      with names(tissue_data).

# "null values found in dataset. Changed to 0"   (sunburst)
#   -> Some cells were left blank. That's fine if those really are zero;
#      otherwise fill them in.

# "Palette '...' not recognised"
#   -> Check the spelling. See hcl.pals() for the full list, or use a
#      viridis palette ("viridis", "magma", "plasma", ...).

# --- Saving (A6 / B6) ---

# "object 'heat_plot' not found" / "object 'sun_plot' not found"
#   -> Run the "Step 1: assign the plot" line in A6 / B6 first.

# Sunburst won't save
#   -> Posit Cloud workshop project: static image export isn't supported.
#      Save as .html with htmlwidgets::saveWidget() (see B6).
#   -> Own device: install.packages(c("chromote", "base64enc")) and make
#      sure Google Chrome is installed.
