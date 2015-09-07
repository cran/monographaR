## ---- eval=FALSE---------------------------------------------------------
#  install.packages("monographaR", dependencies=T)

## ---- eval=FALSE---------------------------------------------------------
#  setwd("C:/My_wording_directory")

## ---- message=FALSE------------------------------------------------------
library(monographaR)

## ---- eval=FALSE---------------------------------------------------------
#  data("monographaR_examples")
#  head(monographaR_examples$colletorList)
#  head(monographaR_examples$examinedSpecimens)
#  head(monographaR_examples$phenoHist)
#  head(monographaR_examples$tableToDescription)
#  head(monographaR_examples$map_data)

## ---- eval=FALSE, tidy=TRUE----------------------------------------------
#  data("monographaR_examples")
#  write.csv(monographaR_examples$colletorList, file="collector_list_model.csv", row.names=F)
#  write.csv(monographaR_examples$examinedSpecimens, file="examined_specimens_model.csv", row.names=F)
#  write.csv(monographaR_examples$phenoHist, file="phenology_model.csv", row.names=F)
#  write.csv(monographaR_examples$tableToDescription, file="table_to_description_model.csv", row.names=F)
#  write.csv(monographaR_examples$map_data, file="map_functions_model.csv", row.names=F)

## ---- tidy=TRUE, results='asis'------------------------------------------

data(monographaR_examples)
monographaR_examples$collectorList -> data


## ---- echo=FALSE, results='asis'-----------------------------------------
data(monographaR_examples)
monographaR_examples$collectorList -> data
knitr::kable(head(data, 5), align="l")

## ---- tidy=TRUE, results='hide'------------------------------------------

collectorList(data, filename = "", paragraphs = FALSE)


## ---- echo=FALSE, results='asis'-----------------------------------------

collectorList(data, filename = "", paragraphs = FALSE)


## ---- tidy=TRUE, results='asis'------------------------------------------

data(monographaR_examples)
monographaR_examples$examinedSpecimens -> data


## ---- echo=FALSE, results='asis'-----------------------------------------
data(monographaR_examples)
monographaR_examples$examinedSpecimens -> data
colnames(data)[2:3] <- c("Collector", "Number")
knitr::kable(head(data, 5), align="l")

## ---- tidy=TRUE, results='hide'------------------------------------------

examinedSpecimens(data, filename = "")


## ---- echo=FALSE, results='asis'-----------------------------------------

examinedSpecimens(data, filename = "")


## ---- tidy=TRUE, results='asis'------------------------------------------

data(monographaR_examples)
monographaR_examples$tableToDescription -> data


## ---- echo=FALSE, results='asis'-----------------------------------------
data(monographaR_examples)
monographaR_examples$tableToDescription -> data
knitr::kable(head(data[,1:5], 5), align="l")

## ---- eval=FALSE---------------------------------------------------------
#  
#  data[,-1] -> data  ## removing first column
#  tableToDescription(data, filename = "")
#  

## ---- echo=FALSE, results='asis'-----------------------------------------

data[,-c(1,8)] -> data
tableToDescription(data, filename = "")


## ---- tidy=TRUE, results='asis'------------------------------------------

data(monographaR_examples)
monographaR_examples$phenoHist -> data


## ---- echo=FALSE, results='asis'-----------------------------------------
data(monographaR_examples)
monographaR_examples$phenoHist -> data
knitr::kable(head(data, 5), align="l")

## ---- tidy=TRUE, eval=FALSE----------------------------------------------
#  
#  phenoHist(data, shrink=1.1, axis.cex=0.8, title.cex=1, pdf=FALSE)
#  
#  

## ---- echo=FALSE, fig.width=3.45-----------------------------------------

par(mar=c(2,2,2,2)) ## this is just to adjust the margins of the figures

phenoHist(data, shrink=1.1, axis.cex=0.8, title.cex=1, pdf=FALSE)



## ---- tidy=TRUE, results='asis'------------------------------------------

data(monographaR_examples)
monographaR_examples$map_data -> data


## ---- echo=FALSE, results='asis'-----------------------------------------
data(monographaR_examples)
monographaR_examples$map_data -> data
knitr::kable(head(data, 5), align="l")

## ---- tidy=TRUE, eval=FALSE----------------------------------------------
#  
#  mapBatch(data , type="simple", zoom=T, margin=0.2, points.col="black",  points.border="white", shape.col="gray90", points.cex=1.5, shape.border = "gray90", export="pdf")
#  

## ---- echo=FALSE, message=FALSE, fig.width=3.45--------------------------
library(rgeos)
library(maptools)
library(raster)
data(monographaR_examples)
monographaR_examples$map_data -> data
data("wrld_simpl")
colnames(data) <- c("sp", "x", "y")
geo <- data
coordinates(geo) <- ~x + y
spp <- as.character(unique(data[, 1]))
spp <- sort(spp)
margin = 0.1; shape.border = "black"; shape.col = "white"; points.col = "black"; points.border = "gray50"; points.cex = 1

par(mar=c(0,0,1,0))
par(cex.main=0.7)

for (i in 1:4) {
  sp <- spp[i]
  spRows <- which(data$sp == sp)
  spData <- data[spRows, ]
  xy <- spData
  coordinates(xy) <- ~x + y
  ext <- extent(xy) * (margin + +1)
  xlim <- c(ext[1], ext[2])
  ylim <- c(ext[3], ext[4])
  plot(wrld_simpl, xlim = xlim, ylim = ylim, axes = T, 
         col = shape.col, border = shape.border, add = F, 
         asp = 1)
  plot(xy, pch = 21, col = points.border, bg = points.col, 
       cex = points.cex, add = T)
  box()
  title(sp)
}


## ---- tidy=TRUE, results='asis'------------------------------------------

data(monographaR_examples)
monographaR_examples$map_data -> data


## ---- echo=FALSE, results='asis'-----------------------------------------
data(monographaR_examples)
monographaR_examples$map_data -> data
knitr::kable(head(data, 5), align="l")

## ---- tidy=TRUE, eval=FALSE----------------------------------------------
#  
#  mapDiversity(data , resolution=1, plot=TRUE, plot.with.grid=TRUE, legend = T, export = F)
#  

## ---- fig.align='center', echo=328, fig.width=7, fig.height=3.8----------

par(mar=c(0,0,0,0)) ## this is just to adjust the margins of the figures

mapDiversity(data , resolution=1, plot=TRUE, plot.with.grid=TRUE, legend = T)



## ---- tidy=TRUE, results='asis'------------------------------------------

data(monographaR_examples)
monographaR_examples$map_data -> data


## ---- echo=FALSE, results='asis'-----------------------------------------
data(monographaR_examples)
monographaR_examples$map_data -> data
knitr::kable(head(data, 5), align="l")

## ---- tidy=TRUE, eval=FALSE----------------------------------------------
#  
#  map.table <- mapTable(data, type="grid", resolution=3, write.output=FALSE)
#  
#  map.table$table
#  

## ---- fig.align='center', echo=380, fig.width=7, fig.height=3.8----------

par(mar=c(0,0,0,0)) ## this is just to adjust the margins of the figures

map.table <- mapTable(data, type="grid", resolution=3, write.output=FALSE)
knitr::kable(head(map.table$table[,1:26], 5), align="l")


