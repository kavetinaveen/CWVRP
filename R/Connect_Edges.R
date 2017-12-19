#' Connect new edges with the existing route
#' @param route -- Sequence of nodes (Ex: 5 4 3 8)
#' @param common_node -- One of the existing node of the route (Ex: 5)
#' @param new_node -- New node to be included to the route (Ex: 9)
#' @examples
#' route <- c(5, 4, 3, 8)
#' common_node <- 5
#' new_node <- 9
#' Connect_Edges(route, common_node, new_node)
#' @export

Connect_Edges <- function(route, common_node, new_node){
  if(route[1] == common_node){
    new_route <- c(new_node, route)
  }else if(route[length(route)] == common_node){
    new_route <- c(route, new_node)
  }else{
    warning("new node is an interior node of the route, cannot connect this edge")
    return(route)
  }
  return(new_route)
}