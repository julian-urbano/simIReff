[![Travis-CI Build Status](https://travis-ci.org/julian-urbano/simIReff.svg?branch=develop)](https://travis-ci.org/julian-urbano/simIReff)
[![License](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![CRAN version](http://www.r-pkg.org/badges/version/simIReff?color=blue)](https://cran.r-project.org/package=simIReff) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/simIReff?color=blue)](https://cran.r-project.org/package=simIReff) 

# simIReff <img src="logo/logo-140x122.png" align="right" />

Provides tools for the stochastic simulation of effectiveness scores to mitigate data-related limitations of Information Retrieval evaluation research. These tools include:

- Fitting of continuous and discrete distributions to model system effectiveness.
- Plotting of effectiveness distributions.
- Selection of distributions best fitting to given data.
- Transformation of distributions towards a prespecified expected value.
- Proxy to fitting of copula models based on these distributions.
- Simulation of new evaluation data from these distributions and copula models.

For reference please refer to *Julián Urbano and Thomas Nagler, "[Stochastic Simulation of Test Collections: Evaluation Scores](http://julian-urbano.info/files/publications/065-stochastic-simulation-test-collections-evaluation-scores.pdf)", ACM SIGIR, 2018*.

## Installation

You may install the stable release from CRAN

```r
install.packages("simIReff")
```

or the latest development version from GitHub

```r
devtools::install_github("julian-urbano/simIReff", ref = "develop")
```

## Usage

Fit a marginal AP distribution and simulate new data
```r
x <- web2010ap[,10] # sample AP scores of a system
e <- effContFitAndSelect(x, method = "BIC") # fit and select based on BIC
plot(e) # plot pdf, cdf and quantile function
e$mean # expected value
y <- reff(50, e) # simulation of 50 new topics
```

and transform the distribution to have a pre-specified expected value.
```r
e2 <- effTransform(e, mean = .14) # transform for expected value of .14
plot(e2)
e2$mean # check the result
```
----------

Build a copula model of two systems
```r
d <- web2010ap[,2:3] # sample AP scores
e1 <- effCont_norm(d[,1]) # force the first margin to follow a truncated gaussian
e2 <- effCont_bks(d[,2]) # force the second margin to follow a beta kernel-smoothed
cop <- effcopFit(d, list(e1, e2)) # copula
y <- reffcop(1000, cop) # simulation of 1000 new topics
c(e1$mean, e2$mean) # expected means
colMeans(y) # observed means
```

and modify the model so both systems have the same distribution
```r
cop2 <- cop # copy the model
cop2$margins[[2]] <- e1 # modify 2nd margin
y <- reffcop(1000, cop2) # simulation of 1000 new topics
colMeans(y) # observed means
```
----------

Automatically build a gaussian copula to many systems,
```r
d <- web2010p20[,1:20] # sample P@20 data from 20 systems
effs <- effDiscFitAndSelect(d, support("p20")) # fit and select margins
cop <- effcopFit(d, effs, family_set = "gaussian") # fit copula
y <- reffcop(1000, cop) # simulate new 1000 topics
```

compare observed vs. expected mean,
```r
E <- sapply(effs, function(e) e$mean)
E.hat <- colMeans(y)
plot(E, E.hat)
abline(0:1)
```

compare observed vs. expected variance,
```r
Var <- sapply(effs, function(e) e$var)
Var.hat <- apply(y, 2, var)
plot(Var, Var.hat)
abline(0:1)
```

and compare original vs. simulated distributions.
```r
o <- order(colMeans(d))
boxplot(d[,o])
points(colMeans(d)[o], col = "red", pch = 4) # plot means
boxplot(y[,o])
points(colMeans(y)[o], col = "red", pch = 4) # plot means
```

## License

`simIReff` is released under the terms of the [MIT License](https://opensource.org/licenses/MIT).

When using this archive, please [cite](CITE.bib) the above paper:

    @inproceedings{urbano2018simulation,
      author = {Urbano, Juli\'{a}n and Nagler, Thomas},
      booktitle = {International ACM SIGIR Conference on Research and Development in Information Retrieval},
      title = {{Stochastic Simulation of Test Collections: Evaluation Scores}},
      pages = {695--704},
      year = {2018}
    }
