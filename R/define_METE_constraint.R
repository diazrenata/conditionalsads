#' @title Draw from METE prediction
#' @description Use `meteR` to generate the SAD from METE and draw samples from it
#' @param s how many species
#' @param n how many individuals
#' @param nsamples how many samples to draw
#' @return matrix of samples. rows are samples, columns are species
#' @export
sample_METE <- function(s, n, nsamples){

  this_esf <- meteR::meteESF(S0 = s, N0 = n)
  this_sad <- meteR::sad(this_esf)

  state.var <- n

  sims <- matrix(nrow = nsamples, ncol = s)

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
