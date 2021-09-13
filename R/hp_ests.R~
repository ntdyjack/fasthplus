#' Estimate H+
#'
#' Estimates H+ for two sets (vectors) a and b. 
#' Utilizes order statistics to algorithmically approximate H+ by sampling
#' a percentage (p) of within- and between-cluster distances.
#'
#' @param a (vector, any length)
#' @param b (vector, any length)
#' @param p percentage of within- and between-cluster distances to sample (numeric)
#' @return Estimated H+ given a, lab, and p
#' @export
#' @examples
#' a <- sapply(1:100, function(i) rnorm(n=50,mean=1.0,sd=1))
#' b <- sapply(1:100, function(i) rnorm(n=50,mean=-1.0,sd=1))
#' h <- hp_ests(a=a,b=b)
hp_ests <- function(a,b,p=.005){
  #generate adjacency matrix and vector
#  ind <- sapply(lab, function(x) sapply(lab, function(y) x==y))
#  ind <- ind[upper.tri(ind)]
#  iw <- which(ind)
#  ib <- which(!ind)
#  dis <- as.matrix(dis)
#  dis <- dis[upper.tri(dis)]
  a <- sort(a)
  b <- sort(b)
  o <- round(p * (length(a) + length(b) ))
  oa <- round(seq(1,length(a),length.out=o))
  ob <- round(seq(1,length(b),length.out=o))

  if(a[oa[1]] > b[ob[o]]){
    spe <- length(oa)*length(ob) 
  } else if(a[oa[o]] < b[ob[1]]){
    spe <- 0
  } else {
    spe <- sum(sapply(a[oa], function(x) sum(x > b[ob])))
  }

  hpe <- spe / (as.numeric(length(oa))*as.numeric(length(ob)))
  return(hpe)
}
