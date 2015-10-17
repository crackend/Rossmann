# Evaluation metric
rmspe <- function(preds, obs) {
  nonZeroInd <- obs > 0
  preds <- preds[nonZeroInd]
  obs <- obs[nonZeroInd]
  err <- sqrt(sum(((preds - obs) / obs) ^ 2, na.rm = TRUE) / length(obs))
  return(RMSPE = err)
}