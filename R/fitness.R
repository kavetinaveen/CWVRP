#' Computes fitness of a solution. It handles vehicle capacity.
#' @param sol Solution vector
#' @param vc Vehicle capacity
#' @param route Logical variable. If TRUE, 
#' @examples 
#' locations <- An32k5locations
#' demand <- An32k5demand
#' sol <- sample(nrow(locations)-1, nrow(locations)-1)
#' vc <- 100
#' DMat <- as.matrix(dist(locations[, -1], upper = TRUE, diag = TRUE))
#' row.names(DMat) <- c(0, 1:(nrow(demand)-1))
#' colnames(DMat) <- c(0, 1:(nrow(demand)-1))
#' fitness(sol, vc, demand, DMat, route = TRUE)
#' @export

fitness <- function(sol, vc, demand, DMat, route = FALSE){
  n <- length(sol)
  fit <- 0
  total_load <- 0
  route_pop <- c()
  edge_dist <- c()
  first <- 1
  j <- 1
  while(j <= n){
    total_load <- total_load + demand[demand[,1] == sol[j], 2]
    if(total_load <= vc){
      if(j == first){
        route_pop <- c(route_pop, 0, sol[j])
        fit <- fit + DMat["0", as.character(sol[j])]
        edge_dist <- c(edge_dist, DMat["0", as.character(sol[j])])
        j <- j + 1
      }else{
        if(total_load/vc > 0.8){
          if(DMat[as.character(sol[j-1]), as.character(sol[j])] > DMat["0", as.character(sol[j])]){
            fit <- fit + DMat[as.character(sol[j-1]), "0"]
            edge_dist <- c(edge_dist, DMat[as.character(sol[j-1]), "0"])
            first <- j
            total_load <- 0
          }else{
            route_pop <- c(route_pop, sol[j])
            fit <- fit + DMat[as.character(sol[j-1]), as.character(sol[j])]
            edge_dist <- c(edge_dist, DMat[as.character(sol[j-1]), as.character(sol[j])])
            j <- j + 1
          }
        }else{
          route_pop <- c(route_pop, sol[j])
          fit <- fit + DMat[as.character(sol[j-1]), as.character(sol[j])]
          edge_dist <- c(edge_dist, DMat[as.character(sol[j-1]), as.character(sol[j])])
          j <- j + 1
        }
      }
    }else{
      fit <- fit + DMat[as.character(sol[j-1]), "0"]
      edge_dist <- c(edge_dist, DMat[as.character(sol[j-1]), "0"])
      first <- j
      total_load <- 0
    }
  }
  fit <- fit + DMat[as.character(sol[length(sol)]), "0"]
  edge_dist <- c(edge_dist, DMat[as.character(sol[length(sol)]), "0"])
  route_pop <- c(route_pop, 0)
  if(route){
    return(list(route = route_pop, edge_dist = edge_dist, fit = fit))
  }else{
    return(fit)
  }
}
