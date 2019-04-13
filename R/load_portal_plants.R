#' @title Download scripts and load data from Supp et al 2013
#' @description Download files from https://github.com/weecology/portal-experimental-macroeco and load Portal plant data into workspace. Stores files in `analysis/supp-2013-files`.
#' @param download TRUE/FALSE, do the files need to be downloaded
#' @return list of data frames for winter and summer plant communities
#' @export

load_portal_plants <- function(download = TRUE) {
  if(download) {
    download.file("https://raw.githubusercontent.com/weecology/portal-experimental-macroeco/master/PortalPlants_ms12-0370R2_code.R", paste0(store_path, "PortalPlants_ms12-0370R2_code.R"))
    download.file("https://raw.githubusercontent.com/weecology/portal-experimental-macroeco/master/PortalPlants_fxns.R", paste0(store_path, "PortalPlants_fxns.R"))
    download.file("https://raw.githubusercontent.com/weecology/portal-experimental-macroeco/master/PortalSummerAnnuals_1995_2009.csv", paste0(store_path, "PortalSummerAnnuals_1995_2009.csv"))
    download.file("https://raw.githubusercontent.com/weecology/portal-experimental-macroeco/master/PortalWinterAnnuals_1995_2009.csv", paste0(store_path, "PortalWinterAnnuals_1995_2009.csv"))
  }

  source('analysis/supp-2013-files/PortalPlants_fxns.R')

  # Code from PortalPlants_ms12-0370R2_code.R:

  # load winter and summer annual plant data and subset out bad data
  winter = read.csv("analysis/supp-2013-files/PortalWinterAnnuals_1995_2009.csv")
  winter = subset(winter, plot!=1 & plot!=9 & plot!=24)   #omit spectabs removal (1,9) and misshapen plot (24)

  summer = read.csv("analysis/supp-2013-files/PortalSummerAnnuals_1995_2009.csv")
  summer = subset(summer, plot!=1 & plot!=9 & plot!=24)   #omit spectabs removal (1,9) and misshapen plot (24)

  # add spatial areas needed to get averages for SARs
  winter_new = add_areas(winter)
  summer_new = add_areas(summer)
  summer_new = subset(summer_new, year != 1997 & year != 1998) # yrs where Unid Spp > 10% community


  ########## MACROECOLOGICAL PATTERNS ##########
  ##### The below functions summarize the plant community data, construct the three macroecological patterns,
  ##### and get the parameters used to characterize each pattern as output for analysis

  ##### RADs
  # reshape data for analysis
  winter_wide = reshape_data(winter_new)
  summer_wide = reshape_data(summer_new)

  portal_plants <- list(winter_wide, summer_wide)

  return(portal_plants)
}
