rmse <- function(obs, pred) {
  return(sqrt(mean((obs-pred)^2)))
}
