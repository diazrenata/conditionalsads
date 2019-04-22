## ----setup---------------------------------------------------------------
#devtools::install_github('diazrenata/feasiblesads')
#devtools::install_github('diazrenata/conditionalsads')
library(conditionalsads)

## ----generate Portal-----------------------------------------------------
# setwd(here::here())
#portal_plants <- process_portal_plants(load_portal_plants(download = F))

#
# rm_list = ls()
# rm(list = rm_list[ which(rm_list != "portal_plants")])
# rm(rm_list)

load('analysis/report/portal_plants.Rds')

plant_abund <- portal_plants[[2]]
plant_abund <- as.matrix(plant_abund)


## ----sample constraints--------------------------------------------------

nsamples <- 100

# constraint_samples <- list()
#
#
# for(i in 1:nrow(plant_abund)) {
#   s = length(which(!is.na(plant_abund[i, ])))
#   n = sum(plant_abund[i,], na.rm = T)
#   this_fs <- sample_feasibleset(s = s, n = n, nsamples)
#   # this_mete <- sample_METE(s = s, n= n, nsamples)
#
#   these_constraint_samples <- list(this_fs) #, this_mete)
#
#   constraint_samples[[i]] <- these_constraint_samples
#
#   rm(this_fs)
#   #rm(this_mete)
#   rm(these_constraint_samples)
#
#   print(i)
#   save(constraint_samples, file = 'constraint_samples.RData')
# }
#
load('analysis/report/constraint_samples.RData')


## ----R2------------------------------------------------------------------
#
# fs_r2 <- list()
# mete_r2 <- list()
#
# fs_evar <- list()
# fs_simp <- list()
# fs_skew <- list()

poilogs <- list()
#
# mete_evar <- list()
# mete_simp <- list()
# mete_skew <- list()

for(i in 1:nrow(plant_abund)) {
  s = length(which(!is.na(plant_abund[i, ])))
  n = sum(plant_abund[i,], na.rm = T)

  if(s == 1) {

    next

  }

  fs_constraint = get_fs_ct(s, n, nsamples = nsamples)

 # mete_constraint = get_mete_prediction(s, n)

  # fs_r2[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                             sampled_rads = constraint_samples[[i]][[1]],
  #                             stat = "r2", constraint = fs_constraint)
  # mete_r2[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[1]],
  #                               stat = "r2", constraint = mete_constraint)

  # fs_evar[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[1]],
  #                               stat = "evar")
  # fs_simp[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[1]],
  #                               stat = "simpson")
  # fs_skew[[i]] <-  get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                                sampled_rads = constraint_samples[[i]][[1]],
  #                                stat = "rad_skew")

  # mete_evar[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[2]],
  #                               stat = "evar")
  # mete_simp[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[2]],
  #                               stat = "simpson")
  # mete_skew[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[2]],
  #                               stat = "rad_skew")

if (sd(as.integer(plant_abund[i, 1:s])) == 0) {
  poilogs[[i]] <- list(NA, NA)
} else {
  poilogs[[i]] <- rad_poilog_cs(as.integer(plant_abund[i, 1:s]))
}
  print(i)
  rm(s)
  rm(n)
}
save.image('analysis/report/plants_stats_list.RData')


## ----get quantiles-------------------------------------------------------
# fs_r2_quantile <- vapply(fs_r2, FUN = test_quantile, FUN.VALUE = 1)
# fs_evar_quantile <- vapply(fs_evar, FUN = test_quantile, FUN.VALUE = 1)
# fs_simp_quantile <- vapply(fs_simp, FUN = test_quantile, FUN.VALUE = 1)
# fs_skew_quantile <- vapply(fs_skew, FUN = test_quantile, FUN.VALUE = 1)
#
#
# mete_r2_quantile <- vapply(mete_r2, FUN = test_quantile, FUN.VALUE = 1)
# mete_evar_quantile  <- vapply(mete_evar, FUN = test_quantile, FUN.VALUE = 1)
# mete_simp_quantile  <- vapply(mete_simp, FUN = test_quantile, FUN.VALUE = 1)
# mete_skew_quantile  <- vapply(mete_skew, FUN = test_quantile, FUN.VALUE = 1)



# Poilog pars

pull_poilog <- function(pl_list, item) {
  if(is.null(pl_list)) {
    return(NA)
    }
  return(pl_list[[item]])
}


poilog_expmu <- vapply(poilogs, FUN = pull_poilog, FUN.VALUE = 1, item = 1)
poilog_sig <- vapply(poilogs, FUN = pull_poilog, FUN.VALUE = 1, item = 2)


plant_abund_results <- cbind(portal_plants[[1]], plant_abund, fs_r2_quantile, fs_evar_quantile, fs_simp_quantile, fs_skew_quantile, poilog_expmu,poilog_sig) # , mete_r2_quantile, mete_evar_quantile, mete_simp_quantile, mete_skew_quantile)

  write.csv(plant_abund_results, "analysis/report/plants_done.csv")

  save.image('analysis/report/plants_done.RData')


