\name{mapDiversity}
\alias{mapDiversity}
\title{
Diversity heatmap
}
\description{
This function will generate a diversity heatmap using presence/absence of 
species on grid cells.
}
\usage{
mapDiversity(data, resolution = 1, plot = T, plot.with.grid = T, 
export = F, legend = T, filename = "diversity_map")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{resolution}{
numeric, size of the grid cells (degrees)
}
  \item{plot}{
logical
}
  \item{plot.with.grid}{
logical, whether or not to add a grid to the plot
}
  \item{export}{
logical
}
  \item{legend}{
logical
}
  \item{filename}{
character
}
}
\details{
It requires a data.frame with three columns, ordered as: species, longitude 
and latitude. The function will plot and return a raster object. The 
resolution of the grid can be changed by the argument "resolution" (in 
degrees). It uses functions of the package raster.
}
\value{
A raster object.
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

## running the function

mapDiversity(data , resolution=1, plot=TRUE, plot.with.grid=TRUE)

}

