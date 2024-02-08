## ----message=FALSE, warning=FALSE---------------------------------------------
library(monographaR)

## ----eval=FALSE---------------------------------------------------------------
#  setwd("C:/My_working_directory")

## ----eval=FALSE---------------------------------------------------------------
#  data("monographaR_examples")
#  head(monographaR_examples$dichoKey)
#  head(monographaR_examples$interactiveKey)

## ----eval=FALSE, tidy=FALSE---------------------------------------------------
#  data("monographaR_examples")
#  write.csv(monographaR_examples$dichoKey, file="dichoKey_model.csv", row.names=F)
#  write.csv(monographaR_examples$interactiveKey, file="interactiveKey_model.csv", row.names=F)
#  

## ----eval=FALSE, tidy=TRUE, results='asis'------------------------------------
#  
#  read.csv("dichoKey_model.csv") -> dat
#  

## ----echo=FALSE---------------------------------------------------------------
data("monographaR_examples")
monographaR_examples$dichoKey -> dat
as.matrix(dat) -> dat
dat[which(dat == "")] <- NA
data.frame(dat) -> dat

## ----echo=FALSE, results='asis'-----------------------------------------------
data(monographaR_examples)
monographaR_examples$dichoKey -> data
knitr::kable(head(data[,1:5], 5), align="l")

## ----tidy=TRUE, results='asis'------------------------------------------------
### First let's include the column name of the first column (where the species name is stored) 
colnames(dat)[1] <- "species"

### Now just run the function 

dataKey(dat, poly.sep = "/") -> dat.k
dat.k$dat -> dat.p

## ----tidy=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  
#  dichoKey(dat.p) -> key
#  

## ----echo=FALSE, message=FALSE------------------------------------------------
dichoKey(dat.p) -> key
cat(key$key, sep="\n")


## ----tidy=TRUE, message=FALSE, results='asis'---------------------------------

### It is possible to check if there was any unresolved species 

### which?

key$unresolved

### how many?

length(key$unresolved)


## ----eval=FALSE---------------------------------------------------------------
#  cat(key$key, file="Pleiochiton_key.txt")

## ----tidy=TRUE, message=FALSE, results='asis'---------------------------------

## The percent of polymorphic spp. for each character was generated before (dataKey) and stored in theobject "dat.k"

dat.k$summary
((dat.k$summary)+1) -> dat.c
colnames(dat.p)
dat.c[2] <- max(dat.c)
dat.c[11] <- max(dat.c)

dichoKey(dat.p, dat.c, cp=0) -> key.c
length(key.c$unresolved)

# Export

cat(key.c$key, file="Pleiochiton_key_costs.txt")


## ----eval=FALSE---------------------------------------------------------------
#  dichoKey.app()

## ----eval=FALSE,  tidy=TRUE, results='asis'-----------------------------------
#  
#  read.csv("interactiveKey_model.csv") -> dat
#  

## ----echo=FALSE---------------------------------------------------------------
data("monographaR_examples")
monographaR_examples$interactiveKey -> dat
as.matrix(dat) -> dat
dat[which(dat == "")] <- NA
data.frame(dat) -> dat

## ----echo=FALSE, results='asis'-----------------------------------------------
data(monographaR_examples)
monographaR_examples$interactiveKey -> data
knitr::kable(head(data[,1:5], 5), align="l")

## ----tidy=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  
#  interactiveKeyLabels(taxon = "species", language = "english") -> spp.eng.labs
#  
#  ### It would be possible to export the object "spp.eng.labs" (e.g., with write.csv), change the labels accordingly and import it again to use in the next step
#  
#  ### Let's change the "title" (default is "My taxon")
#  
#  head(spp.eng.labs)
#  spp.eng.labs[3] <- "Pleiochiton"
#  head(spp.eng.labs)
#  

## ----tidy=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  
#  interactiveKey(dat = dat, txt.labels = spp.eng.labs, poly.sep = "/", taxa.in.italics = TRUE, theme = "lumen")
#  

