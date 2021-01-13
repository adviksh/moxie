#' @title Analyze a Causal Experiment with a Predictive Model.
#' @name analyze
#' 
#' @description Evaluate whether variables in a feature set(s) contain
#' information about treatment level assignment.
#' 
#' @param features matrix | (named) list of matrices \cr
#' Features to use when predicting treatment. Can be a matrix or a (named) list
#' of matrices.
#' @param tmt vector \cr
#' A vector of treatment assignments.
#' @param learner object of class "learner" or "constructor" \cr
#' A predictive model.
#' @param learners list of object of class "learner" or "constructor"
#' @param task_type "binary" | "regression" | "multiclass" \cr
#' The kinds of models and predictions to make for treatment status.
#' "binary" for binary treatment, "regression" for continuous
#' treatment, and "multiclass" for discrete multilevel treatment.
#' @param weight numeric vector, optional \cr
#' Observation weights.
#' @param y matrix, optional
#' Outcomes of the experiment. If provided, these will be used to estimate
#' treatment effects. If you want to use the outcomes to predict treatment
#' effect, they need to be included in \code{features}. NULL skips treatment
#' effect estimation.
#' @param block vector, optional \cr
#' A vector of block IDs, if treatment was randomly assigned within blocks.
#' @param cluster vector, optional
#' A vector of cluster IDs, if treatment was randomly assigned to clusters
#' of observations. All observations in the same cluster must have been 
#' assigned the same treatment.
#' @param loss function or (named) list of functions \cr
#' A function to measure how good predictions are. Lower values indicate
#' better predictions. If provided as a list of functions, each function will
#' be used to evaluate predictions separately.
#' @param test_method "cv" | "holdout" \cr
#' Whether to use cross-validation ("cv") or a holdout set ("holdout") for
#' hypothesis tests. Using a holdout usually reduces the statistical power of
#' the procedure, but will also usually be faster.
#' @param test_split proportion | integer | integer vector \cr
#' Only used when test_method = "holdout". Either the proportion of observations
#' to place in the holdout set, or an integer vector that assigns observations
#' to the holdout set. If given a vector, entries with NAs will be excluded
#' from both the training and holdout sets..
#' @param tune_split integer | integer vector \cr
#' Either the number of folds for tuning, or an integer vector that assigns
#' observations to tuning folds. Entries with NAs will not be used for tuning.
#' @param permutations integer | list of permuted tmt \cr
#' Either the number of permutations to perform or a list of permuted treatment
#' assignments in the holdout set. NULL skips the permutation test.
#' @param parallel character vector \cr
#' Steps of analysis to parallelize. Can be any combination of "tune",
#' "train", "predict", "perm_test", and "tmt_hat_struct". Only effective when
#' comparing a list of x or a list of learners.
#' @param random_seed integer \cr
#' Random seed to set before starting analysis. Uses kind = "L'Ecuyer-CMRG"
#' to 
#' @param verbose boolean \cr
#' Whether to print progress via message().
#' 
#' @return Alist with elements:
#' \describe{
#'   \item{run_time}{time elapsed}
#'   \item{random_seed}{random seed used in analysis}
#'   \item{system_info}{system and user information, as output by Sys.info()}
#'   \item{session_info}{information about R, the OS, and attached and loaded packages, as output by sessionInfo()}
#'   \item{learners}{a list of tuned and trained learners, one per feature set provided }
#'   \item{folds}{a list of tuning and training folds used}
#'   \item{predictions}{predicted treatment assignment for all observations}
#'   \item{perm_test}{a data.frame containing the results of the permutation test}
#'   \item{tmt_hat_struct}{a data.frame summarizing the relationship between predicted treatment assignment and the outcomes}
#'   \item{comparison}{a data.frame comparing features (for \code{analyze_features})}
#'   }
#'
#' @details 
#' 
#' analyze_features() and analyse_features() are synonyms.
#' 
#' \code{analyze_features} accepts either a single feature set (x as a matrix),
#' or several feature sets (x as a list of matrices). With a single feature set,
#' it measures how well the provided learner can recover information about 
#' treatment assignment. With several feature sets, it measures how well the 
#' provided learner recovers information about treatment assignment from each 
#' feature set, and then compares performance across these feature sets.
#' 
#' When comparing a list of feature sets, we recommend naming each list entry.
#' Otherwise, \code{moxie} will use default names
#' ("feature_set_1", "feature_set_2", ...) in output.
#' 
#' If blocks or clusters are present, they affect how the data get 
#' split into training/holdout, how the training set gets split into folds,
#' and how treatment labels are permuted in the holdout set. 
#' 
#' When blocks are present, sample-splitting happens at the block level. 
#' For example, suppose block_var contains 10 blocks, holdout_split = 0.5,
#' and tune_folds = 5. Then, 5 blocks will be placed in the holdout set, and 
#' the remaining 5 blocks will be divided into training folds of 1 block each. 
#' When blocks are not present but clusters are, sample-splitting happens at 
#' the cluster level.
#'
#' When blocks and clusters are present, treatment label permutation happens 
#' at the cluster level, within blocks. In each permutation, the treatment 
#' assignment for an entire cluster gets picked from the pool of treatments
#' assigned to all clusters in the same block. When blocks are present but
#' clusters are not, this happens at the observation level within blocks.
#' When clusters are present but blocks are are not, this happens at the 
#' cluster level in the entire holdout set.
NULL
