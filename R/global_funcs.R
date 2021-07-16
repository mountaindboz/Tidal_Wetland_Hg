# Tidal Wetlands Hg Journal Article
# Purpose: Global functions to be used across analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# Define absolute file path for the Tidal Wetlands Hg Journal Article SharePoint site
  # Function creates a filepath to the root of the SharePoint site
  # The optional fp_rel argument allows for more specific file paths beyond the SharePoint
    # root directory
twhg_abs_sp_path <- function(fp_rel = NULL) {
  fp_twhg <- "California Department of Water Resources/Tidal Wetlands Mercury Journal Article - Documents"

  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_twhg))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_twhg, fp_rel))
  }

  return(fp_abs)
}

