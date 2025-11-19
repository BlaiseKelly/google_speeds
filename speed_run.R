library(gmapsdistance)
library(lubridate)
library(tidyverse)
library(googleway)
library(reshape2)
library(openair)
library(lwgeom)
library(sf)
library(dplyr)
library(mapview)
library(stplanr)
library(osmactive)
library(osmdata)
library(tmap)
library(tmap.networks)

source("R/get.R")

select <- dplyr::select

## enter google console distance api key
api_key <- ""

## This link in Stockholm has a count point with open access data to compare
# stockholm <- readRDS("data/google_speed_stockholm.RDS") |> 
#   select(ID = osm_id, geometry)

# create a geo referenced point
area <- st_point(c(106.895,47.916)) |> 
  st_sfc(crs = 4326) |> 
  st_buffer(100)

area_bb <- st_bbox(area)

##download landuse from osm
x <- opq(bbox = area_bb) %>%
  add_osm_feature(key = c('highway')) %>%
  osmdata_sf()

##extract rd data and trim to the main roads
osm_drive = osmactive::get_driving_network(x$osm_lines) 
  select(ID = osm_id, highway) |> 
  filter(highway %in% c("primary", "trunk", "trunk_link", "tertiary"))

# how much will it cost?
costs <- get_costs(links = osm_drive)

# get speed data using Google API
speed_dat <- get_future_week_speeds(osm_drive)

#saveRDS(speed_dat, "data/ulaanbaater.RDS")

# alternatively read in from data folder
speed_dat <- readRDS("data/ulaanbaater.RDS")

# swap UTC date with date for local area
speed_dat$date_new <- with_tz(speed_dat$date, "Asia/Ulaanbaatar")

#plot for each link
dir.create("plots")
for (eL in unique(speed_dat$ID)){
  tryCatch({
    df2 <- speed_dat |> 
      filter(ID == eL) |> 
      select(date = date_new, speed)
    p2 <- timeVariation(df2, pollutant = "speed", main = paste0(eL, " Weekly Speeds (kph)"))
    filename <- paste0("plots/", eL, "_", Sys.Date(), ".png")
    png(filename, width=2000, height=500, units="px", res=160)
    print(p2$plot$day.hour)
    dev.off()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# plot mean for all links
p2 <- timeVariation(speed_dat, pollutant = "speed", main = paste0("speed profile of area central Ulaanbaatar, Mongolia (kph)"))

# save plot
filename <- paste0("plots/avg_profile.png")
png(filename, width=2000, height=1500, units="px", res=180)
print(p2)
dev.off()


# join spatial data to speeds and adjust date format for tmap
speed_plot <- speed_dat |> 
  transmute(date = as.factor(date_new),day, ID, speed) |> 
  left_join(osm_drive, by = "ID") |> 
  st_set_geometry("geometry")

# get background map
bg <- basemaps::basemap_raster(speed_plot, map_service = "carto", map_type = "light")
#bg_m <- mask(bg,st_transform(area,3857))


# generate a date string for the plot
d8s2plot <- speed_dat |> 
  filter(ID == speed_plot$ID[1]) |> 
  mutate(day = wday(date_new,label = TRUE,abbr = FALSE)) |> 
  transmute(date_day = paste0(format(date_new), "\n", day))

# set to override limit of 64 frames
tmap_options(facet.max = 200)

#assign(paste0(c,"_map"), cycle_net_c)
tmap_mode("plot")
tm1 <- tm_shape(bg)+
  tm_rgb(col_alpha = 1)+
  tm_shape(speed_plot) +
  tm_edges(
    col = "speed",col.scale = tm_scale_intervals(values = "gmt.seis"),
    lwd = 4
  )+
  tm_legend(show = TRUE, position = c(0.77,0.20))+
  tm_title(text = d8s2plot$date_day, position = c(0.05,0.96))+
  tm_layout(frame = FALSE, panel.show = FALSE)+
  tm_animate(by = "date")
#tm_components(c("tm_legend", "tm_credits"), position = c("left", "top"), bg.color = "grey95")

tmap_animation(tm1, filename = paste0("plots/speeds.gif"), width =1700, height = 3000, dpi = 300, delay = 40)

