#' @title H+ discordance metric
#'
#' @description Estimates the H+ discordance metric 
#' for either (1) two sets (vectors) A and B, or 
#' (2) a dissimilarity matrix D and a label vector L.
#' Approximation is calculated using p+1 percentiles,
#' with an accuracy bound of 1/p.
#'
#' @param A numeric vector containing a set of length n
#' @param B numeric vector containing a set of length n
#' @param D distance matrix of dimension nxn
#' @param L numeric or character vector of length n
#' @param p integer representing the number of percentiles
#' @param alg integer (1 or 2) representing the choice of algorithm used to estimate H+ (Algorithm 1 or 2)
#' 
#' @return A numeric returning the estimated value of H+. 
#' @export
#' 
#' @examples
#' a <- rnorm(n=500,mean=0)
#' b <- rnorm(n=500,mean=1)
#' h <- hpe(A=a,B=b,p=101,alg=1)
#'
#' a <- sapply(1:500, function(i) rnorm(n=50,mean=0))
#' b <- sapply(1:500, function(i) rnorm(n=50,mean=0))
#' x <- cbind(a,b)
#' d <- dist(t(x))
#' l <- c(rep(0,500),rep(1,500))
#' h <- hpe(D=d,L=l,p=101,alg=1)
#' 
hpe <- function(A, B, D, L, p = 101, alg = 1) {
  abflg <- missing(A) & missing(B)
  dlflg <- missing(D) & missing(L)
  if (abflg & dlflg) {
    stop("please provide either A B or D L")
  } else if (!abflg) {
    nmflg <- (!is.numeric(A) & !is.numeric(A))
    if (nmflg) {
      stop("please ensure A B are numeric")
    } else {
      print("Estimating H+ using A B formulation...")
    }
  } else if (!dlflg) {
    #  tyflg <- !(class(D)=='dist' | (is.matrix(D) & isSymmetric(D)  & is.numeric(D)) )
    tyflg <- !(class(D) == 'dist')
    if (tyflg) {
      stop("please ensure D is a dist objcect")
    }
    tyflg <- !(is.numeric(L) | is.character(L))
    if (tyflg) {
      stop("please ensure L is numeric or character vector")
    }
    D <- as.matrix(D)
    dmflg <- !(ncol(D) == nrow(D) & ncol(D) == length(L))
    if (dmflg) {
      stop("Dimension mismatch for D and L")
    } else {
      print("Estimating H+ using D L formulation...")
    }
    ind <- sapply(L, function(x)
      sapply(L, function(y)
        x == y))
    ind <- ind[upper.tri(ind)]
    iw <- which(ind)
    ib <- which(!ind)
    D <- D[upper.tri(D)]
    A <- D[iw]
    B <- D[ib]
  }
  
  
  ps <- seq(0, 1, length.out = p)
  qA <- quantile(A, probs = ps)
  qB <- quantile(B, probs = ps)
  
  #call hp alg1 or 2
  if (alg == 1) {
    he <- alg1(qA, qB, p)
  } else if (alg == 2) {
    he <- alg2(qA, qB, p)
  } else{
    stop("please specify a vaid algorithm (alg=1 or 2)")
  }
  
  #return estimate
  return(he)
}
