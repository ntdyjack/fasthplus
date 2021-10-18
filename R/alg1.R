#' Algorithm 1 ("brute_force") for H+ Estimate
#'
#' Estimates H+ for two sets (vectors) A and B, or 
#' a dissimilarity matrix D and a label vector L.
#' Approximation is calculated using p+1 percentiles,
#' with an accuracy bound of 1/p.
#'
#' @param qA (vector, px1)
#' @param qB (vector, px1)
#' @return Estimated H+, un-standardized (numeric vector)
#' @keywords internal
alg1 <- function(qA,qB,p){
  inty <- sapply(qA, function(x) sum(x>qB))
  return(inty)
}
