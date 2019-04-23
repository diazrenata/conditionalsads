## ----setup---------------------------------------------------------------
library(conditionalsads)

## ----generate Portal, eval =T--------------------------------------------
setwd(here::here())
# portal_plants <- process_portal_plants(load_portal_plants(download = F))
#
# rm_list = ls()
# rm(list = rm_list[ which(rm_list != "portal_plants")])
# rm(rm_list)
#
# plant_abund <- portal_plants[[2]]
# plant_abund <- as.matrix(plant_abund)
#
# save.image('plant_data.RData')

load('plant_data.RData')


## ----sample constraints, eval = T----------------------------------------

nsamples <- 2

constraint_samples <- list()

constraint_samples <- apply(plant_abund, MARGIN = 1, FUN = sample_plant_constraints, nsamples = nsamples, fs= T)

save.image('sampling_constraints.RData')



## ----R2, eval =T---------------------------------------------------------


fs_r2 <- list()
fs_kl <- list()
fs_evar <- list()
fs_simp <- list()
fs_skew <- list()
mete_r2 <- list()
mete_kl <- list()
mete_evar <- list()
mete_simp <- list()
mete_skew <- list()
poilogs <- list()

for(i in 1:nrow(plant_abund)) {

  this_rad = as.integer(plant_abund[i, ])
  this_rad = na.omit(this_rad)
  s = length(this_rad)
  n = sum(this_rad)


  if(!is.null(constraint_samples[[i]][[1]])) {
    fs_constraint = get_fs_ct(s, n, nsamples = nsamples, newsamples = F, oldsamples = constraint_samples[[i]][[1]])

    fs_r2[[i]] <- get_stat_list(empirical_rad = this_rad,
                                sampled_rads = constraint_samples[[i]][[1]],
                                stat = "r2", constraint = fs_constraint)
    fs_kl[[i]] <- get_stat_list(empirical_rad = this_rad,
                                sampled_rads = constraint_samples[[i]][[1]],
                                stat = "kl_div", constraint = fs_constraint)
    fs_evar[[i]] <- get_stat_list(empirical_rad = this_rad,
                                  sampled_rads = constraint_samples[[i]][[1]],
                                  stat = "evar")
    fs_simp[[i]] <- get_stat_list(empirical_rad = this_rad,
                                  sampled_rads = constraint_samples[[i]][[1]],
                                  stat = "simpson")
    fs_skew[[i]] <-  get_stat_list(empirical_rad = this_rad,
                                   sampled_rads = constraint_samples[[i]][[1]],
                                   stat = "rad_skew")

  }

  if(!(constraint_samples[[i]][[2]][1] == 'METE error')) {
    possibly_get_mete <- purrr::possibly(get_mete_prediction, otherwise = NULL)
    mete_constraint  = possibly_get_mete(s, n)

    if(!is.null(mete_constraint)) {
      mete_r2[[i]] <- get_stat_list(empirical_rad = this_rad,
                                    sampled_rads = constraint_samples[[i]][[2]],
                                    stat = "r2", constraint = mete_constraint)


      mete_kl[[i]] <- get_stat_list(empirical_rad = this_rad,
                                    sampled_rads = constraint_samples[[i]][[2]],
                                    stat = "kl_div", constraint = mete_constraint)
    }


    mete_evar[[i]] <- get_stat_list(empirical_rad = this_rad,
                                    sampled_rads = constraint_samples[[i]][[2]],
                                    stat = "evar")

    mete_simp[[i]] <- get_stat_list(empirical_rad = this_rad,
                                    sampled_rads = constraint_samples[[i]][[2]],
                                    stat = "simpson")
    mete_skew[[i]] <- get_stat_list(empirical_rad = this_rad,
                                    sampled_rads = constraint_samples[[i]][[2]],
                                    stat = "rad_skew")
  }

  possibly_poilogs <- purrr::possibly(rad_poilog_cs, otherwise = NULL)

  poilogs[[i]] <- possibly_poilogs(this_rad)
  print(i)
  save.image('getting_stats.RData')

  rm(s)
  rm(n)
}



## ----get quantiles, eval = T---------------------------------------------
fs_r2_quantile <- vapply(fs_r2, FUN = test_quantile, FUN.VALUE = 1)
fs_kl_quantile <- vapply(fs_kl, FUN = test_quantile, FUN.VALUE =1)
fs_evar_quantile <- vapply(fs_evar, FUN = test_quantile, FUN.VALUE = 1)
fs_simp_quantile <- vapply(fs_simp, FUN = test_quantile, FUN.VALUE = 1)
fs_skew_quantile <- vapply(fs_skew, FUN = test_quantile, FUN.VALUE = 1)


mete_r2_quantile <- vapply(mete_r2, FUN = test_quantile, FUN.VALUE = 1)
mete_kl_quantile <- vapply(mete_kl, FUN = test_quantile, FUN.VALUE =1)
mete_evar_quantile  <- vapply(mete_evar, FUN = test_quantile, FUN.VALUE = 1)
mete_simp_quantile  <- vapply(mete_simp, FUN = test_quantile, FUN.VALUE = 1)
mete_skew_quantile  <- vapply(mete_skew, FUN = test_quantile, FUN.VALUE = 1)


# Poilog pars

pull_poilog <- function(pl_list, item) {
  if(is.null(pl_list)) {
    return(NA)
  }
  return(pl_list[[item]])
}

# possibly_pull_poilog <- purrr::possibly(pull_poilog, otherwise = 100)

poilog_expmu <- vapply(poilogs, FUN = pull_poilog, FUN.VALUE = 1, item = 1)
poilog_sig <- vapply(poilogs, FUN = pull_poilog, FUN.VALUE = 1, item = 2)

save.image('stats_done.RData')

plant_abund_results <- cbind(portal_plants[[1]], plant_abund, fs_r2_quantile,fs_kl_quantile, fs_evar_quantile, fs_simp_quantile, fs_skew_quantile, mete_r2_quantile, mete_kl_quantile, mete_evar_quantile, mete_simp_quantile, mete_skew_quantile, poilog_expmu,
                             poilog_sig)

#if(FALSE) {
write.csv(plant_abund_results, "plants_2samples_done.csv")
save.image('plants_2samples.RData')
#}


## ----fs plots, echo = F--------------------------------------------------
# load('plants_done.RData')
# library(ggplot2)
#
#
# for(i in 1:2) {
#
#   this_season <- unique(plant_abund_results$season)[i]
#
#   this_data <- plant_abund_results %>%
#     dplyr::filter(season == this_season)
#
#   r2_plot <-  ggplot(data = this_data, aes(x = trmt, y = fs_r2_quantile)) +
#     geom_jitter(aes(x = this_data$trmt, y = this_data$fs_r2_quantile)) +
#     ggtitle(paste0(this_season, " r2")) +
#     theme_bw()
#
#   kl_plot <-  ggplot(data = this_data, aes(x = trmt, y = fs_kl_quantile)) +
#     geom_jitter(aes(x = this_data$trmt, y = this_data$fs_kl_quantile)) +
#     ggtitle(paste0(this_season, " kl")) +
#     theme_bw()
#
#   evar_plot <-  ggplot(data = this_data, aes(x = trmt, y = fs_evar_quantile)) +
#     geom_jitter(aes(x = this_data$trmt, y = this_data$fs_evar_quantile)) +
#     ggtitle(paste0(this_season, " evar")) +
#     theme_bw()
#
#   simp_plot <-  ggplot(data = this_data, aes(x = trmt, y = fs_simp_quantile)) +
#     geom_jitter(aes(x = this_data$trmt, y = this_data$fs_simp_quantile)) +
#     ggtitle(paste0(this_season, " Simpson evenness")) +
#     theme_bw()
#
#   skew_plot <-  ggplot(data = this_data, aes(x = trmt, y = fs_skew_quantile)) +
#     geom_jitter(aes(x = this_data$trmt, y = this_data$fs_skew_quantile)) +
#     ggtitle(paste0(this_season, " skewness")) +
#     theme_bw()
#
#
#     poilog_mu_plot <- ggplot(data = this_data, aes(x = trmt, y = poilog_expmu)) +
#     geom_jitter(aes(x = this_data$trmt, y = this_data$poilog_expmu)) +
#     ggtitle(paste0(this_season, " poilog_expmu")) +
#     theme_bw()
#
#    poilog_sig_plot <- ggplot(data = this_data, aes(x = trmt, y = poilog_sig)) +
#     geom_jitter(aes(x = this_data$trmt, y = this_data$poilog_sig)) +
#     ggtitle(paste0(this_season, " poilog_sig")) +
#     theme_bw()
#
# print(gridExtra::grid.arrange(r2_plot, kl_plot, evar_plot, simp_plot, skew_plot,
#                               poilog_mu_plot, poilog_sig_plot,
#                           nrow = 4))
# }



