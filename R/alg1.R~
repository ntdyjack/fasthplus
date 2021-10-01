#' Algorithim 1 for H+ Estimate
#'
#' Estimates H+ for two sets (vectors) A and B, or 
#' a dissimilarity matrix D and a label vector L.
#' Approximation is calculated using p+1 percentiles,
#' with an accuracy bound of 1/p.
#'
#' @param qA (vector, px1)
#' @param qB (vector, px1)
#' @return Estimated H+ (numeric)
alg1 <- function(qA,qB,p){
  he <- sum(sapply(qA, function(x) sum(x>qB))) / (p^2)
  return(he)
}
