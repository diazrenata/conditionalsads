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

load('portal_plants.Rds')

hist(rowSums(portal_plants[[2]], na.rm = T))

highN <- which(rowSums(portal_plants[[2]], na.rm = T) > 2000)

portal_plants[[1]] <- portal_plants[[1]][-highN, ]
portal_plants[[2]] <- portal_plants[[2]][-highN, ]

plant_abund <- portal_plants[[2]]
plant_abund <- as.matrix(plant_abund)


## ----sample constraints--------------------------------------------------

nsamples <- 1

constraint_samples <- list()

for(i in 1:nrow(plant_abund)) {
  s = length(which(!is.na(plant_abund[i, ])))
  n = sum(plant_abund[i,], na.rm = T)
  this_fs <- sample_feasibleset(s = s, n = n, nsamples)
  # this_mete <- sample_METE(s = s, n= n, nsamples)

  these_constraint_samples <- list(this_fs) #, this_mete)

  constraint_samples[[i]] <- these_constraint_samples

  rm(this_fs)
  #rm(this_mete)
  rm(these_constraint_samples)

  print(i)
  save(constraint_samples, file = 'constraint_samples_nohighN.RData')
}



## ----R2------------------------------------------------------------------

fs_r2 <- list()
mete_r2 <- list()

fs_evar <- list()
fs_simp <- list()
fs_skew <- list()

mete_evar <- list()
mete_simp <- list()
mete_skew <- list()

for(i in 1:nrow(plant_abund)) {
  s = length(which(!is.na(plant_abund[i])))
  n = sum(plant_abund[i,], na.rm = T)
  fs_r2[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
                              sampled_rads = constraint_samples[[i]][[1]],
                              stat = "r2", constraint = get_fs_ct(s, n, nsamples = nsamples))
  # mete_r2[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[1]],
  #                               stat = "r2", constraint = get_mete_prediction(s, n))

  fs_evar[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
                                sampled_rads = constraint_samples[[i]][[1]],
                                stat = "evar")
  fs_simp[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
                                sampled_rads = constraint_samples[[i]][[1]],
                                stat = "simpson")
  fs_skew[[i]] <-  get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
                                sampled_rads = constraint_samples[[i]][[1]],
                                stat = "rad_skew")

  # mete_evar[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[2]],
  #                               stat = "evar")
  # mete_simp[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[2]],
  #                               stat = "simpson")
  # mete_skew[[i]] <- get_stat_list(empirical_rad = as.integer(plant_abund[i, 1:s]),
  #                               sampled_rads = constraint_samples[[i]][[2]],
  #                               stat = "rad_skew")


  rm(s)
  rm(n)
}


## ----get quantiles-------------------------------------------------------
fs_r2_quantile <- vapply(fs_r2, FUN = test_quantile, FUN.VALUE = 1)
fs_evar_quantile <- vapply(fs_evar, FUN = test_quantile, FUN.VALUE = 1)
fs_simp_quantile <- vapply(fs_simp, FUN = test_quantile, FUN.VALUE = 1)
fs_skew_quantile <- vapply(fs_skew, FUN = test_quantile, FUN.VALUE = 1)
#
#
# mete_r2_quantile <- vapply(mete_r2, FUN = test_quantile, FUN.VALUE = 1)
# mete_evar_quantile  <- vapply(mete_evar, FUN = test_quantile, FUN.VALUE = 1)
# mete_simp_quantile  <- vapply(mete_simp, FUN = test_quantile, FUN.VALUE = 1)
# mete_skew_quantile  <- vapply(mete_skew, FUN = test_quantile, FUN.VALUE = 1)


plant_abund_results <- cbind(portal_plants[[1]], plant_abund, fs_r2_quantile, fs_evar_quantile, fs_simp_quantile, fs_skew_quantile) # , mete_r2_quantile, mete_evar_quantile, mete_simp_quantile, mete_skew_quantile)

  write.csv(plant_abund_results, "plants_done_nohighN.csv")

  save.image('plants_done_nohighN.RData')


