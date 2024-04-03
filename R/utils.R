#' @export
generate_decision_function <- function(boundary_function){
  return(function(x){
    return(boundary_function(x[1]) > x[2])
  })
}
