#' @title H+ discordance estimation with bootstrapping
#'
#' @description Estimates the H+ discordance metric 
#' given data Dat and a label vector L. 
#' Bootstrapping is performed over r iterations with s points sampled per iterate.
#'
#' @param D numeric matrix or data frame with observations in rows (nxm)
#' @param L numeric vector containing a set of length n
#' @param r numeric number of bootstrap iterations
#' @param t numeric  pre-bootstrap sample size, 1-5% of n 
#' 
#' @return list, h is the estimated H+ value.
#' @return gamma1 and gamma2 are plausible ranges for what % of A (or Dw)
#' @return are strictly greater than B (or Db)
#' @export
#' 
#' @importFrom stats median quantile
#' 
#' @examples
#' a <- sapply(1:500, function(i) rnorm(n=50, mean=0))
#' b <- sapply(1:500, function(i) rnorm(n=50, mean=0))
#' x <- t(cbind(a,b))
#' l <- c(rep(0,500), rep(1,500))
#' h <- hpb(D=x, L=l, r=30,t=50)
#' 
hpb <- function(D, L, r=30, t) {
  dlflg <- missing(D) | missing(L)
  if(dlflg){
    stop("please provide both D and L")
  }
  tyflg <- !(any(class(D) %in% c('matrix','data.frame'))) | !is.numeric(D)
  if(tyflg){
   stop("please ensure D is a numeric matrix or data.frame")
  }
  dmflg <- ! nrow(D) == length(L)
  if(dmflg){
    stop("Dimension mismatch for D and L")
  }
  trflg <- ! (class(r) %in% c('numeric','integer') & class(t) %in% c('numeric','integer'))
  if(trflg){
    stop("please ensure r and t are numeric or integer")
  }
  tnflg <- t >= nrow(D)
  if(tnflg){
    stop("t is too large, try ~1-5% of observations")
  }
  lbtab <- table(L)/length(L) #pre-compute this table / list
  lblis <- lapply(names(lbtab), function(x) which(L==x))
  hpb <- mean(replicate(r,bsf(d=D,l=L,t=t,q=lblis),T))
  return(hpb)
}
