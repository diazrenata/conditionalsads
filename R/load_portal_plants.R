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

  rm_list = ls()
  rm(list = rm_list[ which(rm_list != "portal_plants")])
  rm(rm_list)
  return(portal_plants)
}

#' Process Portal plant data
#'
#' @param portal_plants output of `load_portal_plants`
#'
#' @return matrix ready for use
#' @export
#'
process_portal_plants <- function(portal_plants){

  #Experimental plot identification numbers refer to experimental treatment as follows:
  #Controls (2, 4, 8, 11, 12, 14, 17, 22), Kangaroo rat removals (3, 6, 13, 15, 18, 19, 20, 21),
  #and total rodent removals (5, 7, 10, 16, 23).

  portal_plants[[1]] <- dplyr::mutate(portal_plants[[1]], season = 'winter')
  portal_plants[[2]] <- dplyr::mutate(portal_plants[[2]], season = 'summer')


  assign_treatment <- function(plot_number){
    if(plot_number %in% c(2,4,8,11,12,14,17,22)) {
      return("control")
    } else if(plot_number %in% c(5,7,10,16,23)) {
      return("removal")
    } else if(plot_number %in% c(3, 6, 13, 15, 18, 19, 20, 21)) {
      return('krat_removal')
    }
  }

  sort_plants <- function(plants) {
    plants$trmt = vapply(plants$plot, FUN = assign_treatment, FUN.VALUE = "control")
    plants_sorted <- as.data.frame(plants) %>%
      dplyr::select(-year, -plot, -trmt, -season)
    for(i in 1:nrow(plants_sorted)) {
      this_rad <- sort(plants_sorted[i, ])
      this_rad <- this_rad[which(this_rad >= 1)]
      this_rad <- c(this_rad, rep(NA, times = length(plants_sorted[i, ]) - length(this_rad)))
      plants_sorted[i,] <- this_rad
    }
    colnames(plants_sorted) <- c(1:ncol(plants_sorted))

    plants_sorted <- cbind(dplyr::select(plants, year, season, trmt, plot), plants_sorted)

    return(plants_sorted)
  }

  portal_plants_sorted <- lapply(portal_plants, sort_plants)

  portal_plants_sorted[[1]] <- dplyr::mutate(portal_plants_sorted[[1]],
                                             '44' = NA,
                                             '45' = NA,
                                             '46' = NA,
                                             '47' = NA)

  portal_plants_done <- rbind(portal_plants_sorted[[1]],
                              portal_plants_sorted[[2]])

  portal_plants_metadata <- dplyr::select(portal_plants_done, year, plot, trmt, season)
  portal_plants_abund <- dplyr::select(portal_plants_done, -year, -plot, -trmt, -season)

  return(list(portal_plants_metadata, portal_plants_abund))

}

