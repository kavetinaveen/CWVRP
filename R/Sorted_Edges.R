#' To sort edges in a decreasing order of their savings
#' @param DMat -- Distance matrix
#' @examples
#' @examples
#' data(An32k5locations)
#' DMat <- DistMat(An32k5locations)
#' row.names(DMat) <- An32k5locations[, 1]
#' colnames(DMat) <- An32k5locations[, 1]
#' Sorted_Edges(DMat)
#' @export

Sorted_Edges <- function(DMat){
  SMat <- SavingMat(DMat, depot = 1)
  result <- as.data.frame.table(SMat)
  colnames(result) <- c("i", "j", "Saving")
  result$i <- as.character(result$i)
  result$j <- as.character(result$j)
  result <- result[result$Saving != 0, ]
  result <- result[order(result$Saving, decreasing = TRUE), ]
  return(result)
}