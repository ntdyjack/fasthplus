# fasthplus

fasthplus provides fast approximations for the disconcordance of two sets.
We present h+, a slight improvement modification of g+, a generalized disconcordance measure proposed by [Rohlf,1975] (https://www.annualreviews.org/doi/abs/10.1146/annurev.es.05.110174.000533).
We present discorcodance in the sense of label fitness (clustering) for a generalized dissimilarity matrix or more generally for two arbitrary sets.


# installation

At present, our package is available only via github installtion using the devtools package.

```R {cmd=false}
library(devtools)
install_github(repo="ntdyjack/fasthplus", ref = "main")
library(fasthplus)
```

# functions

##estimate h+ for a dissimilarity matrix and set of labels
```R {cmd=false}
a <- sapply(1:100, function(i) rnorm(n=50,mean=0.0,sd=1))
b <- sapply(1:100, function(i) rnorm(n=50,mean=0.0,sd=1))
c <- cbind(a,b)
l <- c(rep(0,100),rep(1,100))
d <- dist(t(c))
h <- hp_estm(dis=d,lab=l,p=0.005)
```

##estimate h+ for two sets
```R {cmd=false}
h <- hp_estm(a=a,b=b)
```
	
