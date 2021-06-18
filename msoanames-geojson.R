# Functions to merge MSOA Names data with polygons and points

# Imports ---------------------------------------------------------------------

library(dplyr)
library(jsonlite)
library(purrr)
library(readr)
library(stringr)
library(tibble)

# Constants -------------------------------------------------------------------

INPUTS_DIR <- "inputs"
OUTPUTS_DIR <- "outputs"

MSOANAMES_IN <- file.path(INPUTS_DIR, "msoanames.csv")
POLYGONS_IN <- file.path(INPUTS_DIR, "msoa-2011-polygons-ons.json")
POINTS_IN <- file.path(INPUTS_DIR, "msoa-2011-centroids-ons.json")

POLYGONS_OUT <- file.path(OUTPUTS_DIR, "msoa-2011-polygons-hcl.json")
POINTS_OUT <- file.path(OUTPUTS_DIR, "msoa-2011-centroids-hcl.json")

# Load the msoaname data with combined English and Welsh names ----------------

load_msoanames <- function() {
    
    msoanames <- read_csv(MSOANAMES_IN)
    
    msoalabels <- map2_chr(
        msoanames$msoa11hclnm,
        msoanames$msoa11hclnmw,
        function(english_name, welsh_name) {
            label <- english_name
            if (! is.na(welsh_name) && 
                str_trim(english_name) != str_trim(welsh_name)) {
                label <- str_glue("{welsh_name} / {english_name}")
            }
            label
        })    
    
    tibble(
        msoa11cd = msoanames$msoa11cd,
        msoa11hclnm = msoalabels)
}

# Merge the names with the polygons -------------------------------------------

merge_names_with_polygons <- function() {
    
    msoanames <- load_msoanames()
    polygons_in <- read_file(POLYGONS_IN)
    polygons <- fromJSON(polygons_in)
    polygons$features$properties <- polygons$features$properties %>% 
        as_tibble() %>% 
        select(
            msoa11cd,
            msoa11nm,
            msoa11nmw) %>% 
        left_join(
            msoanames, 
            by = "msoa11cd")
    
    polygons_out <- toJSON(
        polygons, 
        digits = 6,
        auto_unbox = TRUE)
    
    write_file(polygons_out, POLYGONS_OUT)
}

# Merge the names with the points ---------------------------------------------

merge_names_with_points <- function() {
    
    msoanames <- load_msoanames()
    points_in <- read_file(POINTS_IN)
    points <- fromJSON(points_in)
    points$features$properties <- points$features$properties %>% 
        as_tibble() %>% 
        select(
            msoa11cd,
            msoa11nm) %>% 
        left_join(
            msoanames, 
            by = "msoa11cd")
    
    points_out <- toJSON(
        points, 
        digits = 6,
        auto_unbox = TRUE)
    write_file(points_out, POINTS_OUT)
}

# Generate the output files ---------------------------------------------------

merge_names_with_polygons()
merge_names_with_points()
