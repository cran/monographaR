\name{mapBatch}
\alias{mapBatch}
\title{
Generates map in batch mode
}
\description{
This wrapper function will export maps for all species in data. 
}
\usage{
mapBatch(data, type = "simple", zoom = T, margin = 0.1, shape, 
raster = NULL, points.col = "black", points.border = "gray50",
points.cex = 1, shape.col = "white", shape.border = "black", 
raster.col = rev(gray.colors(65, start = 0, end = 1)), 
raster.legend = F, export = "pdf", tiff.width = 100, 
tiff.height = 100, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{type}{
"simple" or "user"
}
  \item{zoom}{
logical
}
  \item{margin}{
numeric, 0 to 1
}
  \item{shape}{
a single or a list of spatial shape objects, when type = "user"
}
  \item{raster}{
a raster object
}
  \item{points.col}{
character
}
  \item{points.border}{
character
}
  \item{points.cex}{
numeric
}
  \item{shape.col}{
character
}
  \item{shape.border}{
character
}
  \item{raster.col}{
vector of colors, when a raster is provided
}
  \item{raster.legend}{
logical
}
  \item{export}{
"pdf" or "tiff"
}
  \item{tiff.width}{
numeric, in mm
}
  \item{tiff.height}{
numeric, in mm
}
  \item{\dots}{
additional arguments for plotting the extra shapes, when type = "user"
}
}
\details{
The function has two output options: a single pdf with all maps (export = 
"pdf") or individual tiff files for each species (export = "tiff"). 
It requires a data.frame with three columns, ordered as: species, longitude 
and latitude. If zoom = TRUE, the function will set the limits of the plot 
using the distribution of each species plus the margin (relative value). If 
zoom = FALSE, the function will use the distribution of the whole data to set 
the limits (all maps will have the same limits). Colors can be changed with 
the arguments points.col, shape.col, shape.border, while the size of the 
points can be changed with points.cex. A raster layer can be provided 
(elevation for instance), and the colors of the raster are controlled by 
raster.col. Optionally, the user can provide a single or a list of shape files 
(type = "user").
}
\value{
Exports a pdf or tiff files.
}
\author{
Marcelo Reginato
}
\seealso{
\code{\link[maptools:readShapePoly]{maptools}}
\code{\link[raster:raster-package]{raster}}
}
\examples{

## loading the example data

data(monographaR_examples)
monographaR_examples$map_data -> data
head(data)

## running the function

# mapBatch(data , type="simple", zoom=T, margin=0.2, points.col="black",
# points.border="white", shape.col="gray90", points.cex=1.5, shape.border 
# = "gray90", export="pdf")

}

