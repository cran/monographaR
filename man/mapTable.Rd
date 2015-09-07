\name{mapTable}
\alias{mapTable}
\title{
Generates a presence/absence matrix of species on grids or countries
}
\description{
This function will generate a presence/absence matrix based on a grid (if type="grid") or
on countries (if type="countries").
}
\usage{
mapTable(data, type = "grid", resolution = 1, write.output = 
FALSE, layer = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{type}{
"grid", "countries" or "user"
}
  \item{resolution}{
numeric (degrees)
}
  \item{write.output}{
logical
}
  \item{layer}{
Spatial DataFrame object, see \code{\link[maptools:readShapeSpatial]{readShapeSpatial}}
}
}
\details{
It requires a data.frame with three columns, ordered as:
species, longitude and latitude. The resolution of the grid can be changed by the argument
"resolution" (in degrees). If type = "user", a layer to intersect the points and create
the matrix should be supplied (a Spatial DataFrame object). It uses functions of the
 package raster and maptools.

}
\value{
list, with a matrix and grid (if type="grid), or a matrix (if type="countries").
}
\author{
Marcelo Reginato
}

\seealso{
\code{\link[raster:raster-package]{raster}}
}
\examples{

## loading the example data

data(monographaR_examples)
monographaR_examples$map_data -> data
head(data)

## running the function with grid

map.table <- mapTable(data, type="grid", resolution=3, 
write.output=FALSE)
  
map.table$table
t(map.table$table)

map.table$grid -> grid

data(wrld_simpl)
plot(grid, border="white")
plot(wrld_simpl, add=TRUE)
plot(grid, add=TRUE)
raster::text(grid, grid@data$layer, cex=1)

## running the function with countries

map.table.country <- mapTable(data, type="country")
map.table.country

}

