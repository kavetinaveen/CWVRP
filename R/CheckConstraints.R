#' Checks whether the constraints satisfied or not
#' @param route -- Sequence of nodes (Ex: 5 4 3 8)
#' @param common_node -- One of the existing node of the route and it shouldn't be an interior of the route (Interior of route: nodes, which have both left and right edge, ex: 3, 4 in above route) (Ex: 5)
#' @param new_node -- New node to be included to the route (Ex: 9)
#' @param Constraints -- List of constraints to check. Currently implemented only vehicle capacity constraint
#' @examples
#' route <- c(5, 4, 3, 8)
#' common_node <- 5
#' new_node <- 9
#' Vehicle_Capacity <- 100
#' data(An32k5demand)
#' demand <- An32k5demand
#' const <- c("Capacity")
#' CheckConstraints(route, common_node, new_node, demand, Vehicle_Capacity, Constraints = const)
#' @export

CheckConstraints <- function(route, common_node, new_node, demand, Vehicle_Capacity, Constraints = c("Capacity")){
  new_route <- Connect_Edges(route, common_node, new_node)
  Filled <- FALSE
  if("Capacity" %in% Constraints){
    if(!exists("Vehicle_Capacity") | is.null(Vehicle_Capacity)){
      stop("Please specify vehicle capacity")
    }else{
      route_demand <- sum(demand[demand[, 1] %in% new_route, 2])
      Filled <- ifelse(route_demand > min(demand[, 2]), TRUE, FALSE)
      if(route_demand < Vehicle_Capacity)
        flag_capacity <- TRUE
      else
        return(list(Satisfied = FALSE, Filled = Filled))
    }
  }
  return(list(Satisfied = TRUE, Filled = Filled))
}
