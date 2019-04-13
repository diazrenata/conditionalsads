#' @title Draw from feasible set
#' @description Use `feasiblesads` to generate samples from the feasible set
#' @param s how many species
#' @param n how many individuals
#' @param nsamples how many samples to draw
#' @return matrix of samples. rows are samples, columns are species
#' @export

sample_feasibleset <- function(s, n, nsamples){
  sims <- feasiblesads::sample_fs(s, n, nsamples, storeyn = FALSE,
                     storepath = NULL)
  return(sims)
}
