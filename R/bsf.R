#' Bootstrap function (bsf) for H+ Estimate
#'
#' Estimates H+ for a random subset of the supplied data
#' to be replicated within the main hpb() function
#' Samples the observations (rows) of d and 
#' the entries of l to generate a single bootstrap estimate.
#'
#' @param d (data, nxm)
#' @param l (identities, nx1)
#' @param r (numeric, 1x1)
#' @param q (list, pre-computed by hpb) 
#' @return One iteration of H+ bootstrap estimate
#' @importFrom stats dist
#' @keywords internal

bsf <- function(d,l,t,q){
  #is this a general solution to sampling from classes in a balanced way?
  #q <- table(l)/length(l)
  #p <- as.vector(sapply(names(q), function(x) sample(which(l==x),round(s/length(q)))))
  p <- as.vector(sapply(q, function(x) sample(x,round(t/length(q)))))
  lab <- l[p]
  dis <- as.matrix(dist(d[p,]))
  dis <- dis[upper.tri(dis)]
  ind <- sapply(lab, function(x) x==lab)
  ind <- ind[upper.tri(ind)]
  iw <- which(ind)
  ib <- which(!ind)
  dw <- dis[iw]
  db <- dis[ib]
  sp <- sum(sapply(dw, function(x) sum(x>db)))
  hp <- sp / (as.numeric(length(dw))*as.numeric(length(db)))
  return(hp)

}
