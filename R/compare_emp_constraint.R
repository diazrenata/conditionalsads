#' @title Calculate a single r2
#'
#' @param focal_rad ascending RAD ("empirical")
#' @param compare_rad ascending RAD from samples
#'
#' @return r2
#' @export
#'
r2 <- function(focal_rad, compare_rad) {

  r2 = 1 - (
    (sum((log10(focal_rad) - log10(compare_rad))^2)) /
      sum((log10(focal_rad) - mean(log10(focal_rad)))^2)
  )

  return(r2)
}

#' @title Calculate Evar
#'
#' @param focal_rad ascending RAD
#'
#' @return focal_evar
#' @export
#'
evar <- function(focal_rad){
  # Based on Locey & White e_var(SAD) python function.
  P = log(focal_rad)
  S = length(focal_rad)

  X <- sum(((P - mean(P))^(2))/S)

  evar = 1- ((2/pi) * atan(X))

  return(evar)

}


#' @title Calculate Simpson evenness
#'
#' @param focal_rad ascending RAD
#'
#' @return Simpson evenness (1-D) for the RAD
#' @export
#'
simpson <- function(focal_rad){
  simp = vegan::diversity(focal_rad, index = "simpson")
  return(simp)
}


#' @title Calculate a single skewness
#'
#' @param focal_rad RAD
#'
#' @return focal_skewness
#' @export
#'
rad_skew <- function(focal_rad){
  skew = e1071::skewness(focal_rad)
  return(skew)
}


#' @title Get test stats for empirical and sampled RADs
#'
#' @description Wrapper for stats functions in conditionalsads.
#'
#' @param empirical_rad vector RAD for empirical community
#' @param sampled_rads matrix RAD for sampled communities
#' @param stat character "r2", "evar", "simpson", "rad_skew"
#' @param constraint if stat = r2, vector comparison RAD (central tendency of feasible set, or METE prediction)
#'
#' @return list of empirical test stat, sampled test stats
#' @export
#'
get_stat_list <- function(empirical_rad, sampled_rads, stat = "r2",
                          constraint = NULL) {
  stat_function <- match.fun(stat)

  stat_results_empirical <- NA
  stat_results_sampled <- vector(length = nrow(sampled_rads))

  if(stat == 'r2') {

    if(!is.null(constraint)) {

    stat_results_empirical <- stat_function(empirical_rad, constraint)
    stat_results_sampled <- apply(sampled_rads, MARGIN = 1, FUN = stat_function,
                                  compare_rad = constraint)
    }

  } else {
    stat_results_empirical <- stat_function(empirical_rad)
    stat_results_sampled <- apply(sampled_rads, MARGIN = 1, FUN = stat_function)
  }

  stat_results <- list(stat_results_empirical, stat_results_sampled)
  return(stat_results)
}


#' @title Calculate the quantile of a test stat for a focal RAD vs. a sample of RADs
#'
#' @param stats_list list of (1) single test stat (R2, evenness, skew) from focal distribution and (2) vector of test stats from sample distributions
#'
#' @return focal_quantile of focal vs. sample
#' @export
#'
test_quantile <- function(stats_list){
  if(is.null(stats_list)) {
    return(NA)
  }

  if(is.nan(stats_list[[1]])) {
    return(NA)
  }

  if(is.nan(stats_list[[2]])) {
    return(NA)
  }

  focal_stat = stats_list[[1]]
  sample_stats = stats_list[[2]]
  sample_ecdf <- ecdf(sample_stats)
  focal_q <- sample_ecdf(focal_stat)
  return(focal_q)
}


#' @title Poilog parameters (From Supp et al 2013)
#'
#' @param x_vec rad
#'
#' @return exp(mu) and sig
#' @export
#'
rad_poilog_cs = function(x_vec){
  #to get back parameter values mu and sigma for RADs
  par_mle = poilog::poilogMLE(x_vec, startVals = c(mu = mean(log(x_vec)),
                                           sig = sd(log(x_vec))))$par
  mu = as.numeric(par_mle)[1]
  sig = as.numeric(par_mle)[2]
  S = length(x_vec)
  N = sum(x_vec)


  parameters = list(exp(mu), sig)
  return(parameters)
}
