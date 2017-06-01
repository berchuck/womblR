#' womblR
#'
#' This package provides methods for estimation of multivariate average hazard ratios as defined by Kalbfleisch and Prentice.
#' The underlying survival functions of the event of interest in each group can be estimated using either the (weighted) Kaplan-Meier estimator or
#' the Aalen-Johansen estimator for the transition probabilities in Markov multi-state models. Right-censored and left-truncated data is supported.
#' Moreover, the difference in restricted mean survival can be estimated. Currently variance estimation for the average hazard ratio based on the
#' Aalen-Johansen estimator is only supported for competing risks models, i.e. for estimation of the average sub-distribution hazard ratio
#' (Average cause-specific hazard ratios can be estimated by using the Kaplan-Meier estimator with competing risks data).
#'
#' Furthermore estimation of quantiles, ratios and differences of quantiles and corresponding p-values and confidence intervals of
#' survival times based on the (weighted) Kaplan-Meier estimator and the Aalen-Johansen estimator is also supported.
#'
#' @author Samuel I. Berchuck \email{berchuck@ad.unc.edu}
#'
#' @name womblR
#' @docType package
#' @import Rcpp
#' @importFrom graphics abline axis layout par plot points title segments symbols text
#' @importFrom grDevices  colorRampPalette
#' @importFrom utils tail
#' @importFrom stats var lm
#' @importFrom mvtnorm pmvnorm
#' @useDynLib womblR
NULL
