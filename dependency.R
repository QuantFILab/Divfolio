library(miniCRAN)
library(igraph)
library(plot.matrix)
library(Matrix)
library("xtable")

tagsp <- c("BatchGetSymbols",
           "tidyverse",
           "rvest",
           "shinydashboard",
           "shiny",
           "plotly",
           "shinycustomloader",
           "shinydashboardPlus",
           "waiter",
           "shinyWidgets",
           "dashboardthemes",
           "quantmod",
           "PerformanceAnalytics",
           "reshape2",
           "viridis",
           "shinyjqui",
           "DT",
           "lubridate",
           "robustbase",
           "shinyBS", 
           "cluster",
           "huge",
           "qgraph",
           "magic",
           "shinyFeedback",
           "shinypop")
a.tagsp <- sort(tagsp)
ver.package <- sapply(a.tagsp, function(x) as.character(packageVersion(x)))
desp.package <- sapply(a.tagsp, function(x) packageDescription(x)$Description)

tab.package <- data.frame(a.tagsp,ver.package,desp.package) %>% `colnames<-`(c("Package","Version","Description"))
print(xtable(tab.package, auto = TRUE), type="html", file="example.html", include.rownames=FALSE)


##########################
dg <- makeDepGraph(tagsp, suggests = FALSE)
set.seed(111)
mdg <- as.matrix(as.matrix(dg))


#dev.new(width=50, height=50)
#
igraph.options(plot.layout= layout_with_dh(dg, weight.edge.lengths = 0, weight.node.dist = 100, weight.border = 100), vertex.size=8,
               cex = 0.7, edge.width = 0.01, edge.arrow.width= 0.5, edge.arrow.size= 0.5, legendPosition = c(-1, -1))
plot(dg,main = "Packages Dependency of the Software")

sapply(tagsp,citation)


