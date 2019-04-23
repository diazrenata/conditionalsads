#' @title Draw from feasible set
#' @description Use `feasiblesads` to generate samples from the feasible set
#' @param s how many species
#' @param n how many individuals
#' @param nsamples how many samples to draw
#' @param inpar in parallel?
#' @return matrix of samples. rows are samples, columns are species
#' @export

sample_feasibleset <- function(s, n, nsamples, inpar = FALSE){
  sims <- feasiblesads::sample_fs(s, n, nsamples, storeyn = FALSE,
                                  storepath = NULL, inpar)
  return(sims)
}

#' @title Find central tendency of feasible set
#' @description Use `feasiblesads` to generate samples from the feasible set and find the central tendency
#' @param s how many species
#' @param n how many individuals
#' @param nsamples how many samples to use
#' @param newsamples get new samples or use presampled ones
#' @param oldsamples matrix of presamples
#' @param inpar in parallel?
#' @return vector central tendency of feasible set
#' @export

get_fs_ct <- function(s, n, nsamples, newsamples = TRUE, oldsamples = NULL, inpar = FALSE){
  if(newsamples == TRUE) {
    set_samples <- conditionalsads::sample_feasibleset(s = s, n = n, nsamples, inpar = inpar)
  } else {
    set_samples <- oldsamples
  }
  fs_ct <- feasiblesads::tally_sets(set_samples) %>%
    dplyr::select(-set_frequency) %>%
    feasiblesads::find_ct() %>%
    as.integer()

  return(fs_ct)
}

#' @title Draw from METE prediction
#' @description Use `meteR` to generate the SAD from METE and draw samples from it
#' @param s how many species
#' @param n how many individuals
#' @param nsamples how many samples to draw
#' @return matrix of samples. rows are samples, columns are species
#' @export
sample_METE <- function(s, n, nsamples){
  sims <- matrix(nrow = nsamples, ncol = s)

  if(s==1) {
    for(i in 1:nsamples) {
      sims[i,1] <- n
    }
  }


    if(n==s) {
      for(i in 1:nsamples) {
        for(j in 1:s) {
        sims[i,j] <- 1
      }
      }
    }
  this_esf <- meteR::meteESF(S0 = s, N0 = n)
  this_sad <- meteR::sad(this_esf)

  state.var <- n


  for(i in 1:nsamples) {
    while(is.na(sims[i, 1])) {
      new.dat <- this_sad$r(s)
      if(sum(new.dat) == state.var) {
        sims[i, ] = sort(new.dat, decreasing = F)
      }
    }
  }

  return(sims)
}

#' @title Get METE prediction as RAD
#' @description Use `meteR` to get the METE prediction for the SAD as a vector
#' @param s how many species
#' @param n how many individuals
#' @return vector METE prediction as an RAD
#' @export

get_mete_prediction <- function(s, n){
  mete_prediction = sort(meteR::meteDist2Rank(meteR::sad(meteR::meteESF(S0 = s, N0 = n))))
  return(mete_prediction)
}

#' Sample plant constraints
#'
#' @param plant_abund_row an RAD, NAs ok
#' @param nsamples how many samples
#' @param fs sample from fs?
#' @param mete sample from mete?
#'
#' @return list of samples
#' @export
#'
sample_plant_constraints <- function(plant_abund_row, nsamples, fs = TRUE, mete = TRUE) {

  s = length(which(!is.na(plant_abund_row)))
  n = sum(plant_abund_row, na.rm = T)
  this_mete <- NULL
  this_fs <- NULL

  if(fs) {
  this_fs <- sample_feasibleset(s = s, n = n, nsamples)
  }

  if(mete){

    possibly_sample_mete <- purrr::possibly(sample_METE, otherwise = 'METE error')

  this_mete <- possibly_sample_mete(s = s, n= n, nsamples)
  }

  these_constraint_samples <- list(this_fs, this_mete)

  return(these_constraint_samples)
}


