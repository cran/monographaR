mapBatch <- function (data, type = "simple", zoom = T, margin = 0.1, shape, 
          raster = NULL, points.col = "black", points.border = "gray50", 
          points.cex = 1, shape.col = "white", shape.border = "black", 
          raster.col = rev(gray.colors(65, start = 0, end = 1)), raster.legend=F, 
	  export = "pdf", tiff.width = 100, tiff.height = 100, ...) 
{
  if (class(data) != "data.frame") {
    stop("data must be a data.frame")
  }
  if (ncol(data) != 3) {
    stop("data must have 3 columns, see help(\"mapBatch\")")
  }
  wrld_simpl = NULL	
  cat("Assuming the columns are ordered as:")
  cat("\n species, longitude and latitude", fill=T)
  cat("\n", fill=T)
  colnames(data) <- c("sp", "x", "y")
  geo <- data
  coordinates(geo) <- ~x + y
  spp <- as.character(unique(data[, 1]))
  spp <- sort(spp)
  data(wrld_simpl, envir = environment())
  if (export == "pdf") {
    pdf("mapBatch.pdf")
  }
  for (i in 1:length(spp)) {
    sp <- spp[i]
    if (export == "tiff") {
      tiff(paste("Figure ", i, " - ", sp, ".tif", sep = ""), 
           width = tiff.width, height = tiff.height, units = "mm", 
           res = 600, compression = "lzw+p")
    }
    spRows <- which(data$sp == sp)
    spData <- data[spRows, ]
    xy <- spData
    coordinates(xy) <- ~x + y
    if (zoom == T) {
      ext <- extent(xy) * (margin + +1)
      xlim <- c(ext[1], ext[2])
      ylim <- c(ext[3], ext[4])
    }
    else {
      ext <- extent(geo) * (margin + +1)
      xlim <- c(ext[1], ext[2])
      ylim <- c(ext[3], ext[4])
    }
    if (type == "simple") {
      plot(wrld_simpl, xlim = xlim, ylim = ylim, axes = T, 
           col = shape.col, border = shape.border, add = F, 
           asp = 1)
      if (is.null(raster) == F) {
        plot(raster, col = raster.col, add = T, legend=raster.legend)
        plot(wrld_simpl, xlim = xlim, ylim = ylim, axes = T, 
             col = shape.col, border = shape.border, add = T, 
             asp = 1)
      }
    }
    else {
      if (class(shape) == "list") {
        plot(shape[[1]], xlim = xlim, ylim = ylim, axes = T, 
             col = shape.col, border = shape.border, add = F, 
             asp = 1, ...)
        if (is.null(raster) == F) {
          plot(raster, col = raster.col, add = T, legend=raster.legend)
          plot(shape[[1]], xlim = xlim, ylim = ylim, axes = T, 
               col = shape.col, border = shape.border, add = T, 
               asp = 1, ...)
        }
        for (k in 2:length(shape)) {
          plot(shape[[k]], xlim = xlim, ylim = ylim, 
               axes = T, col = shape.col, border = shape.border, 
               add = T, asp = 1)
        }
      }
      else {
        plot(shape, xlim = xlim, ylim = ylim, axes = T, 
             col = shape.col, border = shape.border, add = F, 
             asp = 1)
        if (is.null(raster) == F) {
          plot(raster, col = raster.col, add = T, legend=raster.legend)
          plot(shape, xlim = xlim, ylim = ylim, axes = T, 
               col = shape.col, border = shape.border, add = T, 
               asp = 1)
        }
      }
    }
    plot(xy, pch = 21, col = points.border, bg = points.col, 
         cex = points.cex, add = T)
    box()
    title(sp)
    if (export == "tiff") {
      dev.off()
    }
  }
  if (export == "pdf") {
    dev.off()
  }
  cat("Maps were saved in:")
  cat("\n", getwd())
}