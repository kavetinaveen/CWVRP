## ----echo = FALSE, include = FALSE---------------------------------------
library(HeuristicsVRP)
options(expressions = 10000)

## ------------------------------------------------------------------------
knitr::kable(head(An32k5demand))
knitr::kable(head(An32k5locations))

## ----fig.align = 'center', fig.width = 6, fig.height = 5, fig.cap = " X-Y coordinates of locations"----
g <- ggplot(An32k5locations, aes(x = X, y = Y))

g + annotate("text", x = An32k5locations[, 2], y = An32k5locations[, 3], label = An32k5locations[, 1])

## ------------------------------------------------------------------------
DMat <- DistMat(An32k5locations)
row.names(DMat) <- NULL
knitr::kable(DMat[1:5, 1:5])

## ------------------------------------------------------------------------
DMat <- DistMat(An32k5locations)
SMat <- SavingMat(DMat)
knitr::kable(SMat[1:5, 1:5])

## ------------------------------------------------------------------------
DMat <- DistMat(An32k5locations)
Sort_Edges <- Sorted_Edges(DMat)
row.names(Sort_Edges) <- NULL
knitr::kable(head(Sort_Edges))

## ----fig.align = 'center', fig.width = 6, fig.height = 5, comment = NA----
Greedy_routes <- CW_VRP(demand = An32k5demand, locations = An32k5locations, Vehicle_Capacity = 100)

## ----comment = NA--------------------------------------------------------
Greedy_routes

## ----fig.align = 'center', fig.width = 6, fig.height = 5, comment = NA----
Greedy_routes_Seq <- CW_VRP(demand = An32k5demand, locations = An32k5locations, Vehicle_Capacity = 100, type = "Sequential")

## ----comment = NA--------------------------------------------------------
Greedy_routes_Seq

## ----fig.align = 'center', fig.width = 6, fig.height = 5, comment = NA----
Greedy_routes_Seq <- CW_VRP(demand = An32k5demand, locations = An32k5locations, Vehicle_Capacity = 500, type = "Sequential")

## ----comment = NA--------------------------------------------------------
Greedy_routes_Seq

