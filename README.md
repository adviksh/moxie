# moxie: a toolbox for causal inference in multi-outcome experiments

This package implements methods described in [Machine-Learning Tests for Effects on Multiple Outcomes](https://arxiv.org/pdf/1707.01473.pdf) by Ludwig, Mullainathan, and Spiess (w.p.).

## Install

`moxie` currently depends on the helper package `learners`, which is available on [GitHub](https://github.com/adviksh/learners). You'll need to install `learners` before `moxie` will work.

### Option 1: R
Make sure you have an up-to-date version of the `devtools` package. Then, open an R session and run the following commands:
```
devtools::install_github("adviksh/learners")
devtools::install_github("adviksh/moxie")
```

### Option 2: Command Line
From a terminal window, navigate to the location where you want to download this repo to. Then run the following commands to download the helper package (`learners`) and this package (`moxie`):
```
git clone git@github.com:adviks/learners
Rscript -e "devtools::install(pkg = 'learners', build_vignettes = TRUE)"
git clone git@github.com:adviks/moxie
Rscript -e "devtools::install(pkg = 'moxie', build_vignettes = TRUE)"
```


## Usage
See `vignette("quick_start", package = "moxie")` for a more detailed walkthrough. The script below gives a bare-bones introduction to key functions.

```r
library(moxie)

# Generate data
n   <- 500
k_x <- 3

x   <- matrix(rnorm(n * k_x), n, k_x)

tmt <- rep(0:1, length.out = n)

# Include outcomes with no treatment effect and linear treatment effect
y   <- cbind(rnorm(n),
             rnorm(n, mean = 0.5 * tmt))

tmt <- factor(tmt)

# Check for balance
# (if x on its own predicts tmt better than expected by chance)
balance_test <- analyze_features(x = x,
                                 tmt = tmt,
                                 learner = moxie::binary_logistic,
                                 y = y)

# Check for signal in outcomes
# (if (x,y) predict tmt better than (x) predicts treatment)
signal_test <- analyze_features(x = list(x_alone = x,
                                         x_and_y = cbind(x, y)),
                                tmt = tmt,
                                learner = moxie::binary_logistic,
                                y = y)
```

## Notes
`moxie` is designed to implement the prediction-as-inference workflow for any predictive model (ex: regression, random forests, rule ensembles). To do so, it requires models in the form of S3 objects with the `learner` class. See `help("learner_class")` for more information.

## References

1. Ludwig, J., Mullainathan, S., & Spiess, J. (2017). Machine Learning Tests for Effects on Multiple Outcomes. arXiv preprint arXiv:1707.01473.

2. Lattner, B. (2018). hete: Heterogeneous Treatment Effect Modeling. R package version 0.1.0.

## TODO

1. Write a technical / extending vignette. Should include things like writing custom learners, using pieces of the package to assemble other workflows, and how package handles factor-type tmt.
