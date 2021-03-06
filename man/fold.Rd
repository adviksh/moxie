% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/folds.R
\name{fold}
\alias{fold}
\alias{fold_grouped}
\alias{fold_stratified}
\alias{fold_grouped_stratified}
\title{CV Folds}
\usage{
fold(n_folds, fold_weights = rep(1, n_folds), n_obs)

fold_grouped(n_folds, fold_weights = rep(1, n_folds), groups)

fold_stratified(n_folds, fold_weights = rep(1, n_folds), strata)

fold_grouped_stratified(n_folds, fold_weights = rep(1, n_folds), groups,
  strata, .f, ...)
}
\arguments{
\item{n_folds}{Number of Folds}

\item{fold_weights}{Nonnegative vector giving the relative size of each fold}

\item{n_obs}{Number of Observations}

\item{groups}{vector identifying the group each observation belongs to}

\item{strata}{vector identifying the stratum each observation belongs to}

\item{.f}{A function to identify the stratum for each group. See details.}

\item{...}{Additional arguments to .f}
}
\value{
An integer vector giving the fold for each observation
}
\description{
CV Folds
}
\section{Functions}{
\itemize{
\item \code{fold_grouped}: Grouped CV Folds

\item \code{fold_stratified}: Stratified CV Folds

\item \code{fold_grouped_stratified}: Grouped and Stratified CV Folds
}}

\examples{
# Basic use
fold(n_folds = 5, n_obs = 20)

# The first fold will have twice as many elements as the second
fold(n_folds = 2, fold_weights = c(2, 1), n_obs = 3)

}
