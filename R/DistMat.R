#' Coomputes distance matrix
#' @param locations -- [ID, X, Y] Node id and X, Y co-ordinates/ Long, Lat
#' @param method -- Metric to calculate distnace between nodes. Feasible methods for X-Y co-ordinates c("euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"); Feasible methods for Long-Lat c(distCosine, distHaversine). Default: "euclidean". Note: Please make sure that, method should be a character only for X-Y co-ordinates not for Long-Lat
#' @examples
#' data(An32k5locations)
#' DistMat(An32k5locations)
#' @export

DistMat <- function(locations, method = "euclidean"){
  # locations -- [ID, X, Y] Node id and X, Y co-ordinates
  # method = "euclidean"
  if(method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")){
    DMat <- dist(locations[, -1], method = method, diag = TRUE, upper = TRUE)
  }else if(method %in% c("distCosine", "distHaversine")){
    DMat <- dist(locations[, -1], method = method, diag = TRUE, upper = TRUE)
  }
  DMat <- as.matrix(DMat)
  row.names(DMat) <- locations[, 1]
  colnames(DMat) <- locations[, 1]
  return(DMat)
}