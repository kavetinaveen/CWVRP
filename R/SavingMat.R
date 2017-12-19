#' To compute savings matrix
#' @param DMat -- Distance matrix, assuming first row and column represents distance from depot
#' depot = 1 by default (fixed)
#' @examples
#' data(An32k5locations)
#' DMat <- DistMat(An32k5locations)
#' row.names(DMat) <- An32k5locations[, 1]
#' colnames(DMat) <- An32k5locations[, 1]
#' SavingMat(DMat)
#' @export

SavingMat <- function(DMat, depot = 1){
  SMat <- matrix(0, nrow = nrow(DMat) - 1, ncol = nrow(DMat) - 1)
  # row.names(SMat) <- setdiff(row.names(DMat), as.character(depot))
  # colnames(SMat) <- setdiff(row.names(DMat), as.character(depot))
  row.names(SMat) <- row.names(DMat)[-1]
  colnames(SMat) <- row.names(DMat)[-1]
  # for(i in row.names(SMat)){
  #   for(j in colnames(SMat)){
  #     if(as.numeric(i) < as.numeric(j)){
  #       SMat[i, j] = DMat[depot, i] + DMat[depot, j] - DMat[i, j]
  #     }
  #   }
  # }
  for(i in seq_len(nrow(SMat))){
    for(j in seq_len(ncol(SMat))){
      if(i < j){
        SMat[i, j] = DMat[depot, i+1] + DMat[depot, j+1] - DMat[i+1, j+1]
      }
    }
  }
  
  return(SMat)
}