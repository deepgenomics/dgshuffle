library(readxl)
library(tidyverse)

# Directories containing data files and layouts.
DATA_DIR <- "ELISA_Microtiling 220215 F4041 357 oligos_SGOKKBSAS_HiTS_data and plate layout"
LAYOUT_DIR <- str_interp("${DATA_DIR}/PLATE LAYOUTS FOR ELISA AND BCA MICROTILE 220215")

# Rename columns and rearrange into long format.
rename_and_rearrange <- function(df, leading) {
  colnames(df) <- c(leading, str_pad(seq(1, 24), 2, side="left", pad="0"))
  result <- df %>% pivot_longer(!leading, names_to="col", values_to="val")
  result
}

# Load a BCA readout.
read_bca_readout <- function(which) {
  filename <- str_interp("${DATA_DIR}/BCA READOUT PLATE ${which} 220215 MICROTILE.xlsx")
  read_excel(filename, skip=23) %>%
    select(c(1:25)) %>%
    rename_and_rearrange(c("row")) %>%
    rename(bca=val)
}

# Load an ELISA readout.
read_elisa_readout <- function(which) {
  filename <- str_interp("${DATA_DIR}/ELISA READOUT PLATE ${which} 220215 MICROTILE.xlsx")
  read_excel(filename, skip=19) %>%
    select(c(1:25)) %>%
    rename_and_rearrange(c("row")) %>%
    rename(elisa=val)
}

# Load an ELISA layout.
read_elisa_layout <- function(which) {
  filename <- str_interp("${LAYOUT_DIR}/ELISA LAYOUT PLATE ${which} 220215 MICROTILE.xlsx")
  read_excel(filename, col_types=rep("text", 26), skip=1) %>%
    select(1:26) %>%
    rename_and_rearrange(c("plate", "row")) %>%
    mutate(val=str_remove(val, regex("\\.0$"))) %>%
    rename(layout=val)
}

# Load all three for analysis and bind columns.
read_all <- function(which) {
  bca_readout <- read_bca_readout("1")
  elisa_readout <- read_elisa_readout("1")
  elisa_layout <- read_elisa_layout("1")
  bind_cols(elisa_layout, elisa_readout["elisa"], bca_readout["bca"])  
}

View(read_all(""))