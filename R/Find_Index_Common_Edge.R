#' To find an index of the sorted edges, in which a common node between routes and existing routes exists. (Only for Sequential approach, internal purpose)
#' @param Sort_Edge -- Edges sorted in a decreasing order of their savings
#' @param Served_nodes -- List of nodes already serviced 

Find_Ind_Common_Edge <- function(Sort_Edge, Served_nodes){
  for(i in 1:nrow(Sort_Edge)){
    if(length(intersect(c(Sort_Edge[i, 1], Sort_Edge[i, 2]), Served_nodes)) > 0) return(i)
  }
  return(0)
}