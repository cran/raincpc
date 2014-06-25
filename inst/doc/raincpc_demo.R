## ----, echo = FALSE, message = FALSE-------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE)

## ----, message = FALSE---------------------------------------------------
library(raincpc)
library(SDMTools)
library(raster)
library(ggplot2)

## ------------------------------------------------------------------------
cpc_get_rawdata(2014, 6, 15, 2014, 6, 19) 

## ------------------------------------------------------------------------
rain1 <- cpc_read_rawdata(2014, 6, 15)
print(rain1)

## ------------------------------------------------------------------------
rain2 <- cpc_read_rawdata(2014, 6, 16)
rain3 <- cpc_read_rawdata(2014, 6, 17)
rain4 <- cpc_read_rawdata(2014, 6, 18)
rain5 <- cpc_read_rawdata(2014, 6, 19)

rain_tot <- rain1 + rain2 + rain3 + rain4 + rain5
print(rain_tot)

## ------------------------------------------------------------------------
png("gfx_rain1.png", width = 800, height = 600)
plot(rain_tot, 
     breaks = c(0, 80, 160, 240, 320),
     col = c("grey", "red", "green", "blue"), 
     main = "Rainfall (mm) June 15 - June 19 2014")
garbage <- dev.off()

## ------------------------------------------------------------------------
raster_ggplot <- function(rastx) {
  require(SDMTools)

  stopifnot(class(rastx) == "RasterLayer")
  
  gfx_data <- getXYcoords(rastx)
  # lats need to be flipped
  gfx_data <- expand.grid(lons = gfx_data$x, lats = rev(gfx_data$y), 
                            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  gfx_data$rain <- rastx@data@values
  
  return (gfx_data)
}

## ------------------------------------------------------------------------
rain_gg <- raster_ggplot(rain_tot)

rain_gg$rain_chunks <- cut(rain_gg$rain, breaks = c(0, 80, 160, 240, 320), 
                         include.lowest = TRUE)

gfx_gg <- ggplot(data = rain_gg)
gfx_gg <- gfx_gg + geom_raster(aes(lons, lats, fill = rain_chunks))
gfx_gg <- gfx_gg + scale_fill_manual(values = c("grey", "red", "green", "blue"))
gfx_gg <- gfx_gg + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_gg <- gfx_gg + labs(x = NULL, y = NULL, fill = "Rain (mm)")
gfx_gg <- gfx_gg + ggtitle("Rainfall June 15 - June 19 2014")
  
png("gfx_rain2.png", width = 800, height = 600)
plot(gfx_gg)
garbage <- dev.off()

## ------------------------------------------------------------------------
lon_vals <- seq(100.75, 130.75, 0.5)
lat_vals <- seq(-10.75, 20.75, 0.5)
reg_box <- expand.grid(lons = lon_vals, lats = lat_vals, 
                            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
reg_box$rain <- extract.data(reg_box, rain_tot)
reg_box$rain_chunks <- cut(reg_box$rain, breaks = c(0, 80, 160, 240, 320), 
                         include.lowest = TRUE)

## ------------------------------------------------------------------------
gfx_gg <- ggplot(data = reg_box)
gfx_gg <- gfx_gg + geom_raster(aes(lons, lats, fill = rain_chunks))
gfx_gg <- gfx_gg + scale_fill_manual(values = c("grey", "red", "green", "blue"))
gfx_gg <- gfx_gg + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_gg <- gfx_gg + labs(x = NULL, y = NULL, fill = "Rain (mm)")
gfx_gg <- gfx_gg + ggtitle("Rainfall over Southeast Asia June 15 - June 19 2014")
  
png("gfx_rain3.png")
plot(gfx_gg)
garbage <- dev.off()

