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

load('constraint_samples_justfs.RData')

fs_kl <- list()

for(i in 1:nrow(plant_abund)) {

  this_rad = as.integer(plant_abund[i, ])
  this_rad = na.omit(this_rad)
  s = length(this_rad)
  n = sum(this_rad)


  if(s == 1) {
    if(n==1) {
      fs_kl[[i]] <- NULL
      next
    }
    }
  if(!is.null(constraint_samples[[i]][[1]])) {
    fs_constraint = get_fs_ct(s, n, nsamples = nsamples, newsamples = F, oldsamples = constraint_samples[[i]][[1]])


    possiblystats = purrr::possibly(get_stat_list, otherwise = NULL)

    fs_kl[[i]] <- possiblystats(empirical_rad = this_rad,
                                sampled_rads = constraint_samples[[i]][[1]],
                                stat = "kl_div", constraint = fs_constraint)


  }
  print(i)
  save.image('getting_kl_fs.RData')

  rm(s)
  rm(n)
}

fs_kl_quantile <- vapply(fs_kl, FUN = test_quantile, FUN.VALUE =1)

save.image('getting_kl_fs.RData')
