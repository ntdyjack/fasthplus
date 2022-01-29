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
#' @param alg character string ("brute_force" or "grid_search") representing the choice of algorithm used to estimate H+
#' @param alpha logical indicator to return alpha values that parameterize balance of within/between cluster distances
#' @param gammas logical indicator to return estimate for gamma values that parameterize what %Dw is greater than a second %Db
#' 
#' @return h is the estimated H+ value.
#' @return (optional) aw and ab (alphaW and alphaB) are (respectively) the portion of within- and between-cluster distances (or portional sizes of A and B)
#' @return (optional) gw and gb (gammaW and gammaB) are plausible ranges for gw*100% of Dw (or A) are strictly greater than gw*100% Db (or B)
#' @export
#' 
#' @importFrom stats median quantile
#' 
#' @examples
#' a <- rnorm(n=500, mean=0)
#' b <- rnorm(n=500 ,mean=1)
#' h <- hpe(A=a, B=b, p=101, alg="brute_force")
#'
#' a <- sapply(1:500, function(i) rnorm(n=50, mean=0))
#' b <- sapply(1:500, function(i) rnorm(n=50, mean=0))
#' x <- cbind(a,b)
#' d <- dist(t(x))
#' l <- c(rep(0,500), rep(1,500))
#' h <- hpe(D=d, L=l, p=101, alg="brute_force")
#' 
hpe <- function(A, B, D, L, p = 101, alg = "brute_force",alpha=F,gammas=F) {
  abflg <- missing(A) & missing(B)
  dlflg <- missing(D) & missing(L)

  if (abflg & dlflg) {
    stop("please provide either (A and B) or (D and L)")
  }

  if (!abflg) {
    nmflg <- (!is.numeric(A) & !is.numeric(B))
    if (nmflg) {
      stop("please ensure A B are numeric")
    } #else {
      #print("Estimating H+ using A B formulation...")
    #}
  }

  if (!dlflg) {
    #  tyflg <- !(class(D)=='dist' | (is.matrix(D) & isSymmetric(D)  & is.numeric(D)) )
    tyflg <- !(class(D) == 'dist')
    if (tyflg) {
      stop("please ensure D is a dist object")
    }
    tyflg <- !(is.numeric(L) | is.character(L))
    if (tyflg) {
      stop("please ensure L is numeric or character vector")
    }
    D <- as.matrix(D)
    dmflg <- !(ncol(D) == nrow(D) & ncol(D) == length(L))
    if (dmflg) {
      stop("Dimension mismatch for D and L")
    }# else {
#      print("Estimating H+ using D L formulation...")
#    }
    #ind <- sapply(L, function(x)
    #  sapply(L, function(y)
    #    x == y))
    ind <- sapply(L, function(x) x==L)
    ind <- ind[upper.tri(ind)]
    iw <- which(ind)
    ib <- which(!ind)
    D <- D[upper.tri(D)]
    A <- D[iw]
    B <- D[ib]
  }
  
  plflg <- ! (class(p) %in% c('numeric','integer') | p >= min(length(A),length(B)))
  if(plflg){
    stop("please ensure p a small numeric or integer , <1% of observations")
  } 
 
  ps <- seq(0, 1, length.out = p)
  qA <- quantile(A, probs = ps)
  qB <- quantile(B, probs = ps)

  #call hp alg ("brute_force" or "grid_search")
  if (alg == "brute_force") {
    hev <- alg1(qA, qB, p)
  } else if (alg == "grid_search") {
    hev <- alg2(qA, qB, p)
  } else{
    stop("please specify a valid algorithm ('brute_force' or 'grid_search')")
  }
  he <- sum(hev)/p^2 #He

  #estimation of gamma using He with 1/p-1 bound 
  cmp <- sapply(ps, function(x) x*ps)
  pth <- sapply(1:p, function(i) {
    tmp <- rep(F,p)
    if(hev[i]>0){tmp[hev[i]] <- T}
    return(tmp)
  })
  gi <- abs(cmp - he) <= 1/(p-1) & pth
  if(any(gi)){
    gAv <- apply(gi,2, function(x) which(x))
    gA <- ps[round(median(unlist(gAv)))]
    gAr <- c(gA-1/(p-1),gA+1/(p-1))
    #c(ps[min(unlist(gAi))],ps[max(unlist(gAi))])
    gBv <- apply(gi,1, function(x) which(x))
    gB <- ps[round(median(unlist(gBv)))]
    gBr <- c(gB-1/(p-1),gB+1/(p-1))

  } else {
    gAr <- NA
    gBr <- NA
    warning('No suitable gammas found for given p, try increasing this parameter')
  }

  if(!alpha & !gammas){
    #fin <- list(h=he, gamma1 = gAr, gamma2 = gBr)
    fin <- he
  } else {
    an <- as.numeric(length(A))
    bn <- as.numeric(length(B))
    aw <- an / (an+bn)
    #ab <- bn / (an+bn)
  } 

  if (alpha & gammas){
    fin <- list(h=he, gammas=c(gw =gAr, gb = gB), alpha=aw)
  } else if (alpha & !gammas) {
    fin <- list(h=he, alpha=aw)
  } else if (!alpha & gammas) {
    fin <- list(h=he, gammas=c(gw =gAr, gb = gB))
  } 

  #return estimate
  return(fin)
}
