# Predict the Mean --------------------------------------------------------
mean_learner <- learners::make_learner(
  train_fun = function(features, tgt, wt) {
    
    if (is.null(wt)) wt <- rep(1L, length(tgt))
    if (is.factor(tgt)) tgt <- as.integer(tgt) - 1L
    
    weighted.mean(tgt, wt)
    
  },
  predict_fun = function(model, features) {
    
    rep(model, nrow(features))
    
  })

