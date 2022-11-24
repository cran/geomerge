generateGrid<-function(extent, size, local.CRS, makeWGS84 = TRUE, silent=FALSE){
  if (silent){
    cat <-function(...){}
  }
  missing_arguments <- c()
  terminate <- FALSE
  if (!is(local.CRS,'CRS')){
    missing_arguments <- append(missing_arguments,paste0('\n local.CRS must be class "CRS"'))
    terminate <-TRUE
  }
  if (!is.projected(extent) | local.CRS@projargs != proj4string(extent)){
    cat(paste0('\n NOTE: extent not provided in a projected CRS or the one matching local.CRS. Changing projection but this may take a while...'))
    if (is(extent,'RasterLayer')){
      extent <- projectRaster(extent, crs=local.CRS)
    }else{
      suppressWarnings(extent <- spTransform(extent,local.CRS))
    }
    cat('Done.\n')
  }
  bb <- bbox(extent)
  cs <- c(3.28084, 3.28084)*size  # cell size is m x m
  cc <- bb[, 1] + (cs/2)  # cell offset
  cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd <- SpatialGridDataFrame(grd,data=data.frame(id=1:prod(cd)),proj4string=CRS(proj4string(extent)))
  sp_poly<- as(sp_grd, "SpatialPolygonsDataFrame")
  sp_poly$ID <- 1:dim(sp_poly)[1]
  sp_poly$z<-NULL
  if (makeWGS84){
    cat(paste0('\n Changing projection to WGS84...'))
    standard.CRS <- CRS("EPSG:4326")
    suppressWarnings(sp_poly <- spTransform(sp_poly,standard.CRS))
    cat('Done.')
  }
  return(sp_poly)
}