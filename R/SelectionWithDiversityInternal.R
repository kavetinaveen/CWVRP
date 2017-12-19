#' Rank space selection operator with diversity, internal purpose (look at selection for more details)
#' @param prob Selection probability values, it is a function of fitness values
#' @param edge_dist Distance between two connected edges
#' @param sel Index of selected units of population
#' @param pc Crossover probability
#' @param p Weight for diversification
#' @param i Counter, internal purpose
#' @export

selection_diversity <- function(prob, edge_dist, diversity, sel = NULL, pc, p, i = 1){
  if(i == 1){
    # cat(i, "\n")
    sel <- sample(1:length(prob), size = 1, prob = pmin(pmax(0, prob), 1, na.rm = TRUE))
    # temp <- stringdist(popStrings[-sel], popStrings[sel[length(sel)]], method = "lv")
    temp <- sqrt(rowSums((edge_dist[-sel, ] - edge_dist[sel[length(sel)], ])^2))
    diversity[-sel] <- diversity[-sel] + temp
    selection_diversity(prob, edge_dist, diversity, sel, pc, p, i+1)
  }else{
    # cat(i, "\n")
    rank_div <- (length(prob) + 1) - rank(diversity, ties.method = "random")
    prob_div <- ((1 - pc)^(rank_div-1)) * pc
    prob_new <- (p * (prob_div)) + ((1 - p) * prob)
    sel <- c(sel, sample(1:length(prob), size = 1, prob = pmin(pmax(0, prob_new), 1, na.rm = TRUE), replace = TRUE))
    # temp <- stringdist(popStrings[-sel], popStrings[sel[length(sel)]], method = "lv")
    temp <- sqrt(rowSums((edge_dist[-sel, ] - edge_dist[sel[length(sel)], ])^2))
    diversity[-sel] <- diversity[-sel] + temp
    # rank_div <- (length(prob) + 1) - rank(diversity, ties.method = "random")
    # prob_div <- ((1 - pc)^(rank_div-1)) * pc
    # prob <- (p * (prob_div)) + ((1 - p) * prob)
    if(i < length(prob)){
      selection_diversity(prob, edge_dist, diversity, sel, pc, p, i+1)
    }else{
      return(sel)
    }
  }
}
