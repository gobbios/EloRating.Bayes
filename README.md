# EloRating.Bayes

## Installation

You need [`cmdstanr`](https://mc-stan.org/cmdstanr/) in order to install and run `EloRating.Bayes`. 
This in turn requires a working C++ toolchain first. 
Check out the [getting started-guide](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) from `cmdstanr` to see how to get everything set up properly. 
Also [this document](https://mc-stan.org/docs/cmdstan-guide/cmdstan-installation.html#cpp-toolchain) might be helpful.

Then install `cmdstanr`.

```
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

And you also need the `remotes` packages, which is easy to install from CRAN:

```
install.packages("remotes")
```

Then check whether things are set up correctly:

```
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE)
```

If this gives positive feedback, install `EloRating.Bayes`:

```
library(remotes)
remotes::install_github("gobbios/EloRating.Bayes", dependencies = TRUE, build_vignettes = FALSE)
```


If you want to install (recompile) the intro vignette, use:

```
library(remotes)
remotes::install_github("gobbios/EloRating.Bayes", dependencies = TRUE, build_vignettes = TRUE)
```


