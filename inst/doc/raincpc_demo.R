## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE)

## ---- message = FALSE---------------------------------------------------------
library(raincpc)
library(raster)
library(ggplot2)

## -----------------------------------------------------------------------------
cpc_get_rawdata(2014, 7, 1, 2014, 7, 7) 

## -----------------------------------------------------------------------------
rain1 <- cpc_read_rawdata(2014, 7, 1)
print(rain1)

## -----------------------------------------------------------------------------
rain_vals <- mapply(FUN = cpc_read_rawdata, yr = 2014, mo = 7, day = 1:7, SIMPLIFY = FALSE, USE.NAMES = FALSE)

## -----------------------------------------------------------------------------
rain_tot <- Reduce("+", rain_vals)
print(rain_tot)

## -----------------------------------------------------------------------------
plot(rain_tot, 
     breaks = c(0, 1, 90, 180, 270, 360),
     col = c("red", "orange", "yellow", "green", "blue"), 
     main = "Rainfall (mm) July 1 - July 7, 2014")

## -----------------------------------------------------------------------------
prep_for_ggplot <- function(rastx) {

  gfx_data <- sdm_getXYcoords(rastx)
  # lats need to be flipped
  gfx_data <- expand.grid(lons = gfx_data$x, lats = rev(gfx_data$y), 
                            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  gfx_data$rain <- rastx@data@values
  
  return (gfx_data)
}

## -----------------------------------------------------------------------------
rain_gg <- prep_for_ggplot(rain_tot)

rain_gg$rain_chunks <- cut(rain_gg$rain, breaks = c(0, 1, 90, 180, 270, 360), 
                         include.lowest = TRUE)

gfx_gg <- ggplot(data = rain_gg)
gfx_gg <- gfx_gg + geom_tile(aes(lons, lats, fill = rain_chunks))
gfx_gg <- gfx_gg + scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue"))
gfx_gg <- gfx_gg + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_gg <- gfx_gg + labs(x = NULL, y = NULL, fill = "Rain (mm)")
gfx_gg <- gfx_gg + ggtitle("Global Rainfall July 1 - July 7, 2014")
  
plot(gfx_gg)


## -----------------------------------------------------------------------------
lon_vals <- seq(100.75, 130.75, 0.5)
lat_vals <- seq(-10.75, 20.75, 0.5)
reg_box <- expand.grid(lons = lon_vals, lats = lat_vals, 
                            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
reg_box$rain <- sdm_extract_data(reg_box, rain_tot)
reg_box$rain_chunks <- cut(reg_box$rain, breaks = c(0, 1, 90, 180, 270, 360), 
                         include.lowest = TRUE)

## -----------------------------------------------------------------------------
gfx_gg <- ggplot(data = reg_box)
gfx_gg <- gfx_gg + geom_tile(aes(lons, lats, fill = rain_chunks))
gfx_gg <- gfx_gg + scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue"))
gfx_gg <- gfx_gg + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_gg <- gfx_gg + labs(x = NULL, y = NULL, fill = "Rain (mm)")
gfx_gg <- gfx_gg + ggtitle("Rainfall over Southeast Asia July 1 - July 7, 2014")
  
plot(gfx_gg)

## -----------------------------------------------------------------------------
cpc_get_rawdata(2014, 7, 1, 2014, 7, 7, usa = TRUE) 

## -----------------------------------------------------------------------------
rain_vals <- mapply(FUN = cpc_read_rawdata, yr = 2014, mo = 7, day = 1:7, usa = TRUE, SIMPLIFY = FALSE, USE.NAMES = FALSE)
rain_tot <- Reduce("+", rain_vals)
print(rain_tot)

## -----------------------------------------------------------------------------
rain_gg <- prep_for_ggplot(rain_tot)

rain_gg$rain_chunks <- cut(rain_gg$rain, breaks = c(0, 1, 45, 90, 135, 180), 
                         include.lowest = TRUE)

gfx_gg <- ggplot(data = rain_gg)
gfx_gg <- gfx_gg + geom_tile(aes(lons, lats, fill = rain_chunks))
gfx_gg <- gfx_gg + scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue"))
gfx_gg <- gfx_gg + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_gg <- gfx_gg + labs(x = NULL, y = NULL, fill = "Rain (mm)")
gfx_gg <- gfx_gg + ggtitle("USA Rainfall July 1 - July 7, 2014")
  
plot(gfx_gg)


## -----------------------------------------------------------------------------
rain1 <- cpc_read_rawdata(2014, 7, 1, usa = TRUE, write_output = TRUE)

