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

#
#' @title Calculate the quantile of a test stat for a focal RAD vs. a sample of RADs
#'
#' @param focal_stat Single test stat (R2, evenness, skew) from focal distribution
#' @param sample_stats Vector of test stats from sample distributions
#'
#' @return focal_quantile
#' @export
#'
test_quantile <- function(focal_stat, sample_stats){
  sample_ecdf <- ecdf(sample_stats)
  focal_q <- sample_ecdf(focal_stat)
  return(focal_q)
}
