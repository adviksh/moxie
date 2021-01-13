# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(future)
library(furrr)
library(moxie)
plan(multiprocess, workers = 20)

# OLS ---------------------------------------------------------------------
ols <- moxie::binary_ols


# GBT ---------------------------------------------------------------------
gbt_tune <- function(features, tgt, wt = rep(1, nrow(features)), folds) {
  
  if (is.factor(tgt)) tgt <- as.integer(tgt) - 1L
  
  gbt_design <- expand.grid(max_depth = 7)
  
  gbt_tuning <- purrr::pmap(gbt_design,
                            xgboost::xgb.cv,
                            data = xgboost::xgb.DMatrix(data = features,
                                                        label = tgt,
                                                        weight = wt),
                            folds   = split(seq_along(folds), folds),
                            params  = list(objective = "binary:logistic",
                                           eval.metric = "rmse",
                                           nthread = 1L,
                                           eta = 0.2),
                            nrounds = 500L,
                            verbose = FALSE,
                            early_stopping_rounds = 10L)
  
  tidy_xgb_cv <- function(result){
    tibble::tribble(~params, ~nrounds, ~rmse,
                    result$params,
                    result$best_ntreelimit,
                    result$evaluation_log$test_rmse_mean[result$best_iteration])
  }
  
  gbt_tuning_df <- purrr::map_dfr(gbt_tuning, tidy_xgb_cv)
  
  best_iter   <- which.min(gbt_tuning_df$rmse)
  
  gbt_train <- function(features, tgt, wt, params, nrounds) {
    
    if (is.factor(tgt)) tgt <- as.integer(tgt) - 1L
    
    xgboost::xgboost(data = features, label = tgt, weight = wt,
                     params = gbt_tuning_df$params[[best_iter]],
                     nrounds = gbt_tuning_df$nrounds[[best_iter]],
                     verbose = FALSE)
  }
  
  
  purrr::partial(gbt_train,
                 params  = gbt_tuning_df$params[[best_iter]],
                 nrounds = gbt_tuning_df$nrounds[[best_iter]])
}


gbt_predict <- function(model, features) {
  as.numeric(stats::predict(model, features))
}

gbt <- moxie::make_learner(name       = "gbt",
                           tune_fn    = gbt_tune,
                           predict_fn = gbt_predict)

rm(gbt_tune, gbt_predict)



# OLSBoost ----------------------------------------------------------------

olsboost_tune <- function(features, tgt, wt = NULL, folds) {
  
  if (is.factor(tgt)) tgt <- as.integer(tgt) - 1L
  if (is.null(wt)) wt <- rep(1, length(tgt))
  
  p_ols_oos <- moxie::tune_predict_oos(moxie::binary_ols,
                                       features, tgt, wt, folds)
  
  # GBT grid search
  gbt_design <- expand.grid(max_depth = 7)
  
  gbt_tuning <- purrr::pmap(gbt_design,
                            xgboost::xgb.cv,
                            data = xgboost::xgb.DMatrix(data = features,
                                                        label = tgt,
                                                        base_margin = p_ols_oos,
                                                        weight = wt),
                            folds = split(seq_along(folds), folds),
                            params = list(nthread = 1L,
                                          eta = 0.2,
                                          objective = "reg:linear",
                                          eval.metric = "rmse",
                                          min_child_weight = 1,
                                          colsample_bytree = 1),
                            nrounds = 50L,
                            early_stopping_rounds = 5L,
                            metrics = list("rmse"),
                            verbose = FALSE)
  
  tidy_xgb_cv <- function(result) {
    
    best_iter <- which.min(result$evaluation_log$test_rmse_mean)
    
    cbind(tibble::tribble(~params, result$params),
          result$evaluation_log[best_iter, ])
    
  }
  
  gbt_tuning_df <- purrr::map_dfr(gbt_tuning, tidy_xgb_cv)
  
  best_iter   <- which.min(gbt_tuning_df$test_rmse_mean)
  
  best_params <- gbt_tuning_df$params[[best_iter]]
  
  ols_rmse     <- Metrics::rmse(tgt, p_ols_oos)
  gbt_rmse     <- gbt_tuning_df$test_rmse_mean[[best_iter]]
  best_nrounds <- ifelse(ols_rmse <= gbt_rmse,
                         0, gbt_tuning_df$iter[[best_iter]])
  
  olsboost_train <- function(features, tgt, wt, eta, nrounds, max_depth,
                             min_child_weight, colsample_bytree) {
    
    if (is.factor(tgt)) tgt <- as.integer(tgt) - 1L
    if (is.null(wt)) wt <- rep(1, length(tgt))
    
    ols_fit <- train(moxie::binary_ols, features, tgt, wt)
    p_ols <- predict(ols_fit, features)
    
    if (nrounds == 0) {
      gbt_fit <- NULL
    } else {
      gbt_fit <- xgboost::xgboost(
        data = xgboost::xgb.DMatrix(data = features,
                                    label = tgt,
                                    base_margin = p_ols),
        params = list(nthread = 1L,
                      eta = eta,
                      max_depth = max_depth,
                      min_child_weight = min_child_weight,
                      colsample_bytree = colsample_bytree,
                      objective = "reg:linear",
                      eval.metric = "rmse"),
        weight = wt,
        nrounds = nrounds,
        metrics = list("rmse"),
        verbose = FALSE)
    }
    
    list(ols = ols_fit,
         gbt = gbt_fit)
  }
  
  train_fn <- purrr::partial(olsboost_train,
                             nrounds = best_nrounds,
                             eta = best_params$eta,
                             max_depth = best_params$max_depth,
                             min_child_weight = best_params$min_child_weight,
                             colsample_bytree = best_params$colsample_bytree)
  
  return(train_fn)
}

olsboost_pred <- function(object, features) {
  
  p_ols <- predict(object$ols, features)
  
  if (rlang::is_null(object$gbt)) {
    pred <- p_ols
  } else {
    pred <- predict(object$gbt,
                    newdata = xgboost::xgb.DMatrix(data = features,
                                                   base_margin = p_ols))
  }
  
  pmin(pmax(pred, 0), 1)
}


# Make Learner ------------------------------------------------------------
olsboost <- moxie::make_learner(name = "olsboost",
                                tune_fn = olsboost_tune,
                                predict_fn = olsboost_pred)

rm(olsboost_tune, olsboost_pred)



# Config ------------------------------------------------------------------
feature_design <- crossing(n_features = 2:4,
                           n_obs = c(100, 500, 1000)) %>% 
  mutate(features = map2(n_obs, n_features,
                         ~matrix(rnorm(.x * .y), nrow = .x)))

design <- crossing(learner = list(ols, olsboost),
                   test_method = c("cv", "holdout"),
                   features = list(matrix(rnorm(1000 * 5), nrow = 1000)),
                   iter = 1:500) %>% 
  mutate(tmt = map(features, ~rep_len(0:1, nrow(.x))),
         tmt = map(tmt, sample))

result <- transmute(design,
                    iter = iter,
                    n_obs = map_int(features, nrow),
                    n_features = map_int(features, ncol),
                    learner_name = map_chr(learner, "name"),
                    test_method = test_method,
                    p_val = future_pmap(design[, c("learner", "test_method", "features", "tmt")],
                                        analyze_features,
                                        .progress = TRUE),
                    p_val = map_dbl(p_val, pluck, "perm_test", "p_val"))

write_rds(result, here("p_values_20190313.rds"))