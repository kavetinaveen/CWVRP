#' Implements Clarke-Wright Savings algorithm to find greedy routes
#' @param locations -- [ID, X, Y] Node id and X, Y co-ordinates/long, lat
#' @param demand -- Demand at each node [ID, Demand]
#' @param DMat -- Provide distance matrix, if you already have computed. Make sure that first row and column of the distance matrix represents distances from depot to all the nodes
#' @param Vehicle_Capacity -- Vehicle capacity
#' @param method -- Metric to calculate distnace between nodes. Feasible methods for X-Y co-ordinates c("euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"); Feasible methods for Long-Lat c(distCosine, distHaversine). Default: "euclidean". Note: Please make sure that, method should be a character only for X-Y co-ordinates not for Long-Lat
#' @param Constraints -- List of constraints to check. Currently implemented only vehicle capacity constraint
#' @param type -- Type of savings algorithm. Possible values ("Parallel", "Sequential"). If you want to build more than one route then we strong recommend you to use "Parallel", "Sequential" algorithm may end up with infeasible nodes for more than one route. For building one route (assumes infinite vehilce capacity) go for "Sequential"
#' @param Plot -- If you want to plot the final greedy routes. Logical (TRUE or FALSE). Default: TRUE
#' @param logfile -- If you want to save all routes in each iteration. Logical (TRUE or FALSE). Check "Results.txt" or "Results_Seq.txt" in your working directory. Default: TRUE
#' @examples
#' data(An32k5locations)
#' locations <- An32k5locations
#' DMat <- DistMat(locations[, -1])
#' row.names(DMat) <- locations[, 1]
#' colnames(DMat) <- locations[, 1]
#' data(An32k5demand)
#' demand <- An32k5demand
#' Vehicle_Capacity <- 100
#' CW_VRP(demand = demand, locations = locations, Vehicle_Capacity = Vehicle_Capacity)
#' @references
#' [1] Classical Heuristics for Vehicle Routing Problem by Gilbert Laporte and Frederic Semet, October, 1998 (Revised: August, 1999), Les Cahiers du Gerad
#' @importFrom graphics plot
#' @import ggplot2
#' @importFrom ggplot2 ggplot aes geom_path labs annotate
#' @importFrom stats dist
#' @importFrom utils capture.output
#' @export

CW_VRP <- function(demand = NULL, locations = NULL, DMat = NULL, Vehicle_Capacity = NULL, method = "euclidean", Constraints = c("Capacity"), type = "Parallel", Plot = TRUE, logfile = TRUE){
  options(expressions = 10000)
  strt <- Sys.time()
  flag_idmap <- 0
  
  if(is.null(locations) & is.null(DMat))
    stop("Please provide either of distance matrix or locations")
  
  if(is.null(locations)) Plot <- FALSE
  
  if(is.null(demand) & "Capacity" %in% Constraints){
    warning("Demand values are not available so neglecting vehicle capacity constraint")
    Constraints <- setdiff(Constraints, "Capacity")
  }
  
  if(!is.null(demand) & is.null(Vehicle_Capacity))
    stop("Please provide vehicle capacity")
  
  if(is.null(DMat)){
    map_ids <- !sum(locations[,1] %in% c(1:nrow(locations)))
    if(map_ids){
      flag_idmap <- 1
      idmap <- data.frame(OID = locations[,1], NID = c(1:nrow(locations)))
      idmap$OID <- as.character(idmap$OID)
      locations[, 1] <- c(1:nrow(locations))
    }
    DMat <- DistMat(locations)
    nnodes <- nrow(locations) - 1
  }else{
    nnodes <- nrow(DMat) - 1
  }
  SMat <- SavingMat(DMat, 1)
  Sort_Edge <- Sorted_Edges(DMat)
  Sort_Edge$i <- as.integer(as.character(Sort_Edge$i))
  Sort_Edge$j <- as.integer(as.character(Sort_Edge$j))
  Sort_Edge$Saving <- as.numeric(Sort_Edge$Saving)
  if(type == "Parallel"){
    Greedy_Routes <- CW_Parallel_VRP(Sort_Edge, nnodes = nnodes, demand, Vehicle_Capacity, logfile = logfile, Constraints = Constraints)
  }else if(type == "Sequential"){
    Greedy_Routes <- CW_Sequential_VRP(Sort_Edge, nnodes = nnodes, demand, Vehicle_Capacity, logfile = logfile, Constraints = Constraints)
  }
  
  for(i in 1:length(Greedy_Routes)){
      Greedy_Routes[[i]] <- c(1, Greedy_Routes[[i]], 1)
  }
  
  cat("Total cost: ", Total_Cost(Greedy_Routes, DMat), "\n")
  
  if(Plot == TRUE){
    g <- ggplot(locations[unlist(Greedy_Routes), ], aes_string(x = names(locations)[2], y = names(locations)[3])) + geom_path(lineend = "round", linetype = 2, show.legend = TRUE) + labs(title = "Plot of Greedy Routes") 
    
    if(exists("idmap")){
      g <- g + annotate("text", x = locations[, 2], y = locations[, 3], label = idmap$OID)
    }else{
      g <- g + annotate("text", x = locations[, 2], y = locations[, 3], label = locations[, 1])
    }
    # ifelse(exists("idmap"), idmap$OID,
    plot(g)
  }
  
  if(is.null(DMat)){
    if(map_ids){
      for(i in 1:length(Greedy_Routes)){
        Greedy_Routes[[i]] <- idmap[Greedy_Routes[[i]], "OID"]
      }
    }
  }
  
  cat("Total time taken: ", Sys.time() - strt,"\n")
  return(Greedy_Routes)
}
