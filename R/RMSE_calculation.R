RMSE_calculation <- function (x,y){
  # x = VCF layer | y = Predicted VCF layer
  delta <- x-y
  square <- (delta)^2
  mean <- cellStats(square, 'mean')
  RMSE <- sqrt(mean)
  return(RMSE)
}