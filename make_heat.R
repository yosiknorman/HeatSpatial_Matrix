library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("maptools")
library("KernSmooth")
library("raster")
library("ncdf4")
library("viridis")
fil1 = "www/data/nc/H08_B13_Indonesia_201811051610.nc"
fil1 = "~/H08_B13_Indonesia_201810172120.nc"
fhat = raster(fil1)

# fhat@ncols
# fhat@nrows
# plot(fhat)
# x11()
# x = fhat
make_list_raster = function(x){
  mfh = as.matrix(x)
  mfh = t(apply(mfh, c(2), FUN = rev))
  lng = seq(extent(x)[1], extent(x)[2], length = x@ncols)
  lat = seq(extent(x)[3], extent(x)[4], length = x@nrows)
  res = list(lng = lng, lat = lat, fhat = mfh)
  return(res)
}

kde = make_list_raster(fhat)
rentang = c(180, 200,  210, 230)
# kde$fhat[kde$fhat > 270] = NA
CL <- contourLines(kde$lng , kde$lat , kde$fhat,  levels = rentang)


## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

areaX  = c()
for(i in 1:length(spgons)){
  areaX[i]= spgons@polygons[[i]]@area  
}

isort = order(areaX)
spgons = spgons[rev(isort),]

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
LEVS = LEVS[rev(isort)]
NLEV <- length(levels(LEVS))
ilevs = as.integer(as.matrix(LEVS))
ilevs[ilevs == rentang[2]] = 1
ilevs[ilevs == rentang[3]] = 2
ilevs[ilevs == rentang[4]] = 3


hh = colorRampPalette(c("red","yellow", "green"))
# gin

leaflet() %>% 
  addTiles(
    urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    attribution = 'Created by <a href="http://www.mapbox.com/">Yosik Norman</a>') %>%
    addPolygons(data = spgons, color = hh(NLEV)[ilevs], weight = 0.1 , fillOpacity =  0.4, 
                popup = as.character(LEVS),
                highlightOptions = highlightOptions(stroke = 1, 
                                                    weight = 2, 
                                                    fillOpacity = 1,bringToFront = F, color = "black"
                                                      ))

