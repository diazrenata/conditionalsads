#' @title Simulate toy data
#' @description Simulates toy data:
#' Toy data should have:
#' Comparisons (difference btwn treatment and control):
#'   h1: Same S and N and both are drawn from the constraint
#'   h2: S and N change but both are drawn from the constraint
#'   h3: Same S and N but treatment is different from constraint
#'   h4: S and N change and treatment is different from constraint
#' Draw from FS and see if METE gives same result
#' And vice versa
#' @param param constraint "METE" or "FS", whether to draw from METE or feasible set for constraint
#' @param nsamples how many samples for each treatment/control pair?
#' @return list of 4 long data frames with the above properties
#' @export

make_toy_data <- function(constraint = "FS", nsamples = 50, ctrl_s = 10, ctrl_n = 1000) {
  # Same S and N, both are drawn from constraint

  h1 = matrix(nrow = nsamples * 2, ncol = ctrl_s)
  if(constraint == "FS") {
    h1[1:nsamples, ] <- sample_feasibleset(ctrl_s, ctrl_n, nsamples)
    h1[(nsamples+1):nrow(h1), ] <- sample_feasibleset(ctrl_s, ctrl_n, nsamples)
  }
  if (constraint == "METE") {
    h1[1:nsamples, ] <- sample_METE(ctrl_s, ctrl_n, nsamples)
    h1[(nsamples+1):nrow(h1), ] <- sample_METE(ctrl_s, ctrl_n, nsamples)
  }

  h1 = as.data.frame(h1)
  colnames(h1) <- 1:ctrl_s
  h1 <- cbind(c(rep('control', nsamples), rep('treatment', nsamples)), h1)
  h1 <- cbind(c(rep('h1', nrow(h1))), h1)
  colnames(h1)[1:2] <- c("h", "trtmnt")

  # S and N change, both are drawn from constraint

  trt_s = floor(ctrl_s*.8)
  trt_n = floor(ctrl_n * .8)

  h2 = matrix(nrow = nsamples * 2, ncol = ctrl_s)
  if(constraint == "FS") {
    h2[1:nsamples, ] <- sample_feasibleset(ctrl_s, ctrl_n, nsamples)
    h2[(nsamples+1):nrow(h2), 1:trt_s] <- sample_feasibleset(trt_s, trt_n, nsamples)
  }
  if (constraint == "METE") {
    h2[1:nsamples, ] <- sample_METE(ctrl_s, ctrl_n, nsamples)
    h2[(nsamples+1):nrow(h2), 1:trt_s] <- sample_METE(trt_s, trt_n, nsamples)
  }

  h2 = as.data.frame(h2)
  colnames(h2) <- 1:ctrl_s
  h2 <- cbind(c(rep('control', nsamples), rep('treatment', nsamples)), h2)
  h2 <- cbind(c(rep('h2', nrow(h2))), h2)
  colnames(h2)[1:2] <- c("h", "trtmnt")

  # Same S and N, treatment is not drawn from constraint

  h3 = matrix(nrow = nsamples * 2, ncol = ctrl_s)
  if(constraint == "FS") {
    h3[1:nsamples, ] <- sample_feasibleset(ctrl_s, ctrl_n, nsamples)
   for(j in (nsamples+1):nrow(h3)) {
     nonconstraint_samples <- floor(runif(n = ctrl_s - 1, min = floor(.8 * floor(ctrl_n/ctrl_s)), max = floor(1.2 * floor(ctrl_n/ctrl_s))))
     nonconstraint_samples <- c(nonconstraint_samples, ctrl_n - sum(nonconstraint_samples))
     nonconstraint_samples <- sort(nonconstraint_samples)
    h3[j, ] <- nonconstraint_samples
   }
  }
  if (constraint == "METE") {
    h3[1:nsamples, ] <- sample_METE(ctrl_s, ctrl_n, nsamples)
    for(j in (nsamples+1):nrow(h3)) {
      nonconstraint_samples <- floor(runif(n = ctrl_s - 1, min = floor(.8 * floor(ctrl_n/ctrl_s)), max = floor(1.2 * floor(ctrl_n/ctrl_s))))
      nonconstraint_samples <- c(nonconstraint_samples, ctrl_n - sum(nonconstraint_samples))
      nonconstraint_samples <- sort(nonconstraint_samples)
      h3[j, ] <- nonconstraint_samples
    }
  }

  h3 = as.data.frame(h3)
  colnames(h3) <- 1:ctrl_s
  h3 <- cbind(c(rep('control', nsamples), rep('treatment', nsamples)), h3)
  h3 <- cbind(c(rep('h3', nrow(h3))), h3)
  colnames(h3)[1:2] <- c("h", "trtmnt")

  # S and N change and treatment is not drawn from constraint

  h4 = matrix(nrow = nsamples * 2, ncol = ctrl_s)
  if(constraint == "FS") {
    h4[1:nsamples, ] <- sample_feasibleset(ctrl_s, ctrl_n, nsamples)
    for(j in (nsamples+1):nrow(h4)) {
      nonconstraint_samples <- floor(runif(n = trt_s - 1, min = floor(.8 * floor(trt_n/trt_s)), max = floor(1.2 * floor(trt_n/trt_s))))
      nonconstraint_samples <- c(nonconstraint_samples, trt_n - sum(nonconstraint_samples))
      nonconstraint_samples <- sort(nonconstraint_samples)
      h4[j, 1:trt_s] <- nonconstraint_samples
    }
  }
  if (constraint == "METE") {
    h4[1:nsamples, ] <- sample_METE(ctrl_s, ctrl_n, nsamples)
    for(j in (nsamples+1):nrow(h4)) {
      nonconstraint_samples <- floor(runif(n = trt_s - 1, min = floor(.8 * floor(trt_n/trt_s)), max = floor(1.2 * floor(trt_n/trt_s))))
      nonconstraint_samples <- c(nonconstraint_samples, trt_n - sum(nonconstraint_samples))
      nonconstraint_samples <- sort(nonconstraint_samples)
      h4[j, 1:trt_s] <- nonconstraint_samples
    }
  }

  h4 = as.data.frame(h4)
  colnames(h4) <- 1:ctrl_s
  h4 <- cbind(c(rep('control', nsamples), rep('treatment', nsamples)), h4)
  h4 <- cbind(c(rep('h4', nrow(h4))), h4)
  colnames(h4)[1:2] <- c("h", "trtmnt")

  return(rbind(h1, h2, h3, h4))
}
