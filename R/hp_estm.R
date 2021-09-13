#' Estimate H+
#'
#' Estimates H+ from a dissimilarity matrix (dis) and identity/label vector (lab). 
#' Utilizes order statistics to algorithmically approximate H+ by sampling
#' a percentage (p) of within- and between-cluster distances.
#'
#' @param dis dissimilarity (distance or matrix with dim nxn)
#' @param lab identity label (vector with length n)
#' @param p percentage of within- and between-cluster distances to sample (numeric)
#' @return Estimated H+ given dis, lab, and p
#' @export
hp_estm <- function(dis,lab,p=.005){
  #generate adjacency matrix and vector
  ind <- sapply(lab, function(x) sapply(lab, function(y) x==y))
  ind <- ind[upper.tri(ind)]
  iw <- which(ind)
  ib <- which(!ind)
  dis <- as.matrix(dis)
  dis <- dis[upper.tri(dis)]
  dw <- sort(dis[iw])
  db <- sort(dis[ib])
  o <- round(p*length(ind))
  ow <- round(seq(1,length(iw),length.out=o))
  ob <- round(seq(1,length(ib),length.out=o))

  if(dw[ow[1]] > db[ob[o]]){
    spe <- length(ow)*length(ob) 
  } else if(dw[ow[o]] < db[ob[1]]){
    spe <- 0
  } else {
    spe <- sum(sapply(dw[ow], function(x) sum(x > db[ob])))
  }

  hpe <- spe / (as.numeric(length(ow))*as.numeric(length(ob)))
  return(hpe)
}
