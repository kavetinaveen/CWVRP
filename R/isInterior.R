#' To find whether a node is interior of the route or not
#' @param route -- Sequence of nodes (Ex: 5 4 3 8)
#' @param node -- One of the existing node of the route (Ex: 5)
#' @examples
#' route <- c(5, 4, 3 ,8)
#' node <- 5
#' isInterior(c(5, 4, 3, 8), 5) # FALSE
#' isInterior(c(5, 4, 3, 8), 4) # TRUE
#' @export

isInterior <- function(route, node){
  # route -- Sequence of nodes (Ex: 5 4 3 8)
  # node -- One of the existing node of the route (Ex: 5)
  # isInterior(c(5, 4, 3, 8), 5) -- FALSE
  # isInterior(c(5, 4, 3, 8), 4) -- TRUE
  if(route[1] == node | route[length(route)] == node)
    return(FALSE)
  else 
    return(TRUE)
}