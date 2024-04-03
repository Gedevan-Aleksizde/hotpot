#' S3 method for class 'hotpot'
#' @export predict.hotpot
#' @exportS3Method stats::predict
predict.hotpot <- function(model, newdata = NULL, ...){
  if(!class(model) %in% "hotpot"){
    stop(sprintf("model should be a hotpot object, but received %s", class(model)))
  }
  if(is.null(newdata)){
    newdata <- model$X
  }
  else{
    if("data.frame" %in% class(newdata)){
      newdata <- as.matrix(newdata)
    }
    if(NCOL(newdata) != 2){
      warning(sprintf("Argument newdata should have just 2 columns, but it has %d columns. use only the first two columns used", NCOL(newdata)))
      newdata <- newdata[, 1:2]
    }
  }
  if(model$params_normalize$scale){
    newdata <- center(newdata, model$params_normalize$center)
    newdata <- scale_unit(newdata, model$params_normalize$minimum, model$params_normalize$maximum)
  }
  X_r <- rotate_X(newdata, -model$parameters$theta)
  return(model$boundary(X_r[, 1]) >= X_r[, 2])
}
