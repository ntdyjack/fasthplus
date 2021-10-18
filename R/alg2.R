#' Algorithm 2 (grid_search) for H+ Estimate
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
alg2 <- function(qA,qB,p){
  i <- j <- 1
  inty <- rep(0,p)
  while(i <= p & j <= p){
    si <- qA[i] > qB[j]
    if(si){ #increase qB
      j <- j + 1
    }else{ #increase qA
      inty[i] <- (j-1)
      i <- i + 1
    }
  }
  return(inty)
}
