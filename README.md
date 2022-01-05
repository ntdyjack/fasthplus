# fasthplus

### Introduction 

The fasthplus R package provides fast approximations for metrics of 
discordance or dissimilarity. 

The metric G+ was
[introduced by W. T. Williams in 1971](https://onlinelibrary.wiley.com/doi/abs/10.2307/1218253) 
as a way to measure the discordance or dissimilarity between two different classifications (where the classification consists of distance matrix and a set of predicted labels for each observation). 

Here, we introduce the H+, which is a modified discordance metric of G+. 
This metric can be used (1) to evaluate the discordance between two arbitrary sets or (2) to evaluate label fitness (clustering) for a generalized dissimilarity matrix. 


### Installing fasthplus

At present, our package is available only via github installation using the `devtools` package.

```
library(devtools)
install_github(repo="ntdyjack/fasthplus", ref = "main")
```

After installation, the package can be loaded into R

```
library(fasthplus)
```

### Using fasthplus

The main function in the **fasthplus** package is `hpe()`. 
The `hpe()` function accepts either (1) two sets (`A` and `B`) or 
(2) a distance matrix (`D`) and set of labels (`L`). 

To provide a fast way to calculate H+, we provide an algorithm that approximates H+ using the number of percentiles (`p`), which is also an argument in the `hpe()` function. 

To run the `hpe()` function with two sets (`A` and `B`) and the number of `p` + 1 percentiles: 

```
a <- rnorm(n=500,mean=0)
b <- rnorm(n=500,mean=1)
h <- hpe(A=a,B=b,p=101,alg=1)
```

To run the `hpe()` function with a dissimilarity matrix (`D`) and set of labels (`L`): 

``` 
# Two sets
a <- sapply(1:100, function(i) rnorm(n=50,mean=0.0,sd=1))
b <- sapply(1:100, function(i) rnorm(n=50,mean=0.0,sd=1))
c <- cbind(a,b)

# Create a set of labels
l <- c(rep(0,100),rep(1,100))

# Calculate a distance matrix
d <- dist(t(c))
h <- hpe(D=d,L=l,p=0.005)
```

	
### Issues and bug reports

Please use https://github.com/ntdyjack/fasthplus/issues to submit issues, bug reports, and comments.

### Contributors

* [Nathan Dyjack](https://github.com/ntdyjack)
* [Stephanie Hicks](https://github.com/stephaniehicks)



