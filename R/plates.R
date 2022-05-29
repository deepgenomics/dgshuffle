library(readxl)
library(glue)
library(tidyverse)

# Directories containing data files and layouts.
DATA_DIR <- "ELISA_Microtiling 220215 F4041 357 oligos_SGOKKBSAS_HiTS_data and plate layout"
LAYOUT_DIR <- str_interp("${DATA_DIR}/PLATE LAYOUTS FOR ELISA AND BCA MICROTILE 220215")
DATA_WIDTH <- 24
LAYOUT_WIDTH <- 26
NUM_PLATE_CELLS <- 384

# Find the starting row and column of the data table in a readout spreadsheet.
find_offset <- function(filename, key, exact) {
  # Read file as-is.
  df <- read_excel(filename)

  # Loop to find value.
  good_row <- -1
  good_col <- -1
  for (ir in seq(nrow(df))) {
    for (ic in seq(ncol(df))) {
      val <- df[[ir, ic]]
      if (is.na(val)) {
        # Ignore
      } else if (exact && (val == key)) {
        good_row <- ir
        good_col <- ic
        break
      } else if ((!exact) && startsWith(val, key)) {
        good_row <- ir
        good_col <- ic
        break
      }
    }
  }

  # Did we find it?
  if (good_row == -1) {
    stop("Failed to find table header.")
  }

  # Adjust row and column.
  if (good_row > 1) {
    good_row = good_row - 1
  }
  if (good_col > 1) {
    good_col = good_col - 1
  }
  return(list(skip_rows = good_row, start_col = good_col))
}

# Rename columns and rearrange into long format.
rename_and_rearrange <- function(df, leading) {
  colnames(df) <- c(all_of(leading), str_pad(seq(1, 24), 2, side="left", pad="0"))
  result <- df %>% pivot_longer(!leading, names_to="col", values_to="val")
  result
}

# Load a BCA readout.
read_bca_readout <- function(which) {
  filename <- str_interp("${DATA_DIR}/BCA READOUT PLATE ${which} 220215 MICROTILE.xlsx")
  offset <- find_offset(filename, "A", TRUE)
  result <- read_excel(filename, skip=offset$skip_rows) %>%
    select(c(offset$start_col:(offset$start_col + DATA_WIDTH))) %>%
    rename_and_rearrange(c("row")) %>%
    rename(bca=val)
  stopifnot(nrow(result) == NUM_PLATE_CELLS)
  result
}

# Load an ELISA readout.
read_elisa_readout <- function(which) {
  filename <- str_interp("${DATA_DIR}/ELISA READOUT PLATE ${which} 220215 MICROTILE.xlsx")
  offset <- find_offset(filename, "A", TRUE)
  result <- read_excel(filename, skip=offset$skip_rows) %>%
    select(c(offset$start_col:(offset$start_col + DATA_WIDTH))) %>%
    rename_and_rearrange(c("row")) %>%
    rename(elisa=val)
  stopifnot(nrow(result) == NUM_PLATE_CELLS)
  result
}

# Load an ELISA layout.
read_elisa_layout <- function(which) {
  filename <- str_interp("${LAYOUT_DIR}/ELISA LAYOUT PLATE ${which} 220215 MICROTILE.xlsx")
  result <- read_excel(filename, col_types=rep("text", LAYOUT_WIDTH)) %>%
    rename_and_rearrange(c("plate", "row")) %>%
    mutate(val=str_remove(val, regex("\\.0$"))) %>%
    rename(layout=val)
  stopifnot(nrow(result) == NUM_PLATE_CELLS)
  result
}

# Load all three for analysis and bind columns.
read_all <- function(which) {
  bca_readout <- read_bca_readout(which)
  elisa_readout <- read_elisa_readout(which)
  elisa_layout <- read_elisa_layout(which)
  bind_cols(elisa_layout, elisa_readout["elisa"], bca_readout["bca"])
}

ELISA_NORM <- c(
  600, 600, 450, 450, 300, 300, 200, 200, 150, 150,
  100, 100,  75,  75, 37.5, 37.5, 18.75, 18.75, 0, 0
)

# Create standard curve for ELISA.
elisa_standard_curve <- function(df) {
  # Get the values from row O.
  # Discard columns 1, 2, 23, and 24.
  values <- df %>%
    filter(row == "O") %>%
    filter(("02" < col) & (col < "23"))
  # Line up with standard curve normalization values.
  values <- bind_cols(values, elisa_norm=ELISA_NORM)
  # Calculate simple linear fit.
  result <- values %>% lm(formula = elisa ~ elisa_norm)
  result$coefficients
}

BCA_NORM <- c(
  2000, 2000, 1500, 1500, 1000, 1000, 750, 750,
  500, 500, 250, 250, 125, 125, 62.5, 62.5,
  31.25, 31.25, 0, 0
)

# Calculate standard curve for BCA.
bca_standard_curve <- function(df) {
  # Get the values from row P.
  # Discard columns 1, 2, 23, and 24.
  values <- df %>%
    filter(row == "P") %>%
    filter(("02" < col) & (col < "23"))
  # Line up with standard curve normalization values.
  values <- bind_cols(values, bca_norm=BCA_NORM)
  # Calculate simple linear fit.
  result <- values %>% lm(formula = elisa ~ bca_norm)
  result$coefficients
}
