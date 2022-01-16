library(gmapsdistance)
library(rgdal)
library(lubridate)
library(tidyverse)
library(googleway)
library(reshape2)
library(openair)
library(lwgeom)
library(sf)
library(dplyr)
library(mapview)

select <- dplyr::select

## enter google console distance api key
api_key <- ""

##load links
stockholm <- readRDS("data/google_speed_stockholm.RDS")
bristol <- readRDS("data/bristol_routes.RDS")

stockholm <- select(stockholm, ID = osm_id, geometry)
bristol <- select(bristol, ID = link_id, geometry)

## to see the routes use mapview
mapview(bristol)+stockholm

all_links <- stockholm

##Work out approximate cost of running the API with all the links (?)
Cost_Estimate <- NROW(unique(all_links$ID))*168*2*0.5/100

spare_links <- floor((200-Cost_Estimate)/(168*2*0.5/100))
bristol <- bristol[1:spare_links,]
all_links <- rbind(all_links, bristol)

##Work out approximate cost of running the API with all the links (?)
Cost_Estimate <- NROW(unique(all_links$ID))*168*2*0.5/100

nextMonday <- function(date) {
  date <- ymd(date)
  .day <- wday(date)
  shift <- ifelse(as.numeric(.day) < 2, 0, 7)
  date + days(2 - as.numeric(.day) + shift)
}

monday <- nextMonday(Sys.Date())
sunday <- monday+6

#set the date Google will search for, this should be a FUTURE DATE that represents Monday to Sunday
kr8d8 <- data.frame(date = seq(
  from=as.POSIXct(paste(monday, "0:00"), tz="UTC"),
  to=as.POSIXct(paste(sunday, "23:00"), tz="UTC"),
  by="hour"
)  )


kr8d8$DoW <- wday(kr8d8$date)
kr8d8$hr_week <- seq(1:NROW(kr8d8))
kr8d8$dayt <- hour(kr8d8$date)
kr8d8$dayt <- as.numeric(kr8d8$dayt)
kr8d8$Hour <- kr8d8$dayt+1
kr8d8$Day_Name <- weekdays(kr8d8$date)

kr8d8_hr_min <- colsplit(kr8d8$date," ", names = c("Date", "hh_mm_ss"))
kr8d8$hh_mm_ss <- kr8d8_hr_min$hh_mm_ss
kr8d8$Date <- kr8d8_hr_min$Date

Big_speedz_list <- list()
Big_distance_list <- list()
Big_time_list <- list()

Links <- unique(all_links$ID)
L <- Links[1]
for(L in Links) {
  
  Section <- filter(all_links, ID == L)
  Section <- data.frame(st_coordinates(Section))
  
  Start <- paste0(Section$Y[1], "+", Section$X[1])
  Finish <- paste0(Section$Y[NROW(Section)], "+", Section$X[NROW(Section)])
  ForWeek <- kr8d8 %>% 
    mutate(Start = Start,
           Finish = Finish)
  datez <- unique(ForWeek$hr_week)
  
  speed_list <- list()
  distance_list <- list()
  time_list <- list()
  D <- datez[1]
  for (D in datez){
    tryCatch({
      df <- filter(ForWeek, hr_week == D)
      dep_d8 <- as.character(df$Date)
      dep_thyme <- as.character(df[,7])
      
      ## make request - THIS STEP COSTS MONEY
    calcs_SF <- gmapsdistance(origin = Start, destination = Finish,
                                mode = "driving", key = api_key,
                                dep_date = dep_d8, dep_time = dep_thyme, traffic_model = "best_guess")
      
      
      Outs_SF <- do.call(rbind, calcs_SF)
      time_SF <- as.numeric(Outs_SF[1,1])
      distance_SF <- as.numeric(Outs_SF[2,1])
      
      
      speed_SF <- (distance_SF/time_SF)*3.6
      
      Speedz <- select(df, date, day = Day_Name)
      Speedz$ID <- L
      Speedz$SF <- speed_SF
      
      distancez <- select(df, date, day = Day_Name)
      distancez$ID <- L
      distancez$SF <- distance_SF
      
      timez <- select(df, date, day = Day_Name)
      timez$ID <- L
      timez$SF <- time_SF
      
      speed_list[[D]] <- Speedz
      distance_list[[D]] <- distancez
      time_list[[D]] <- timez
      whereupto <- paste0(L,"_", D)
      print(whereupto)
      # update GUI console
      flush.console()
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  SL <- do.call(rbind, speed_list)
  DL <- do.call(rbind, distance_list)
  TL <- do.call(rbind, time_list)
  
  Big_speedz_list[[L]] <- SL
  Big_distance_list[[L]] <- DL
  Big_time_list[[L]] <- TL
  
}

save.image(file = paste0("outputs/Google_run_", Sys.Date(), ".RData"))

#load("outputs/Google_run_2021-08-31.RData")

S_ALL <- do.call(rbind, Big_speedz_list)
BDL_SF <- do.call(rbind, Big_distance_list)
BTL_SF <- do.call(rbind, Big_time_list)

S_ALL <- select(S_ALL, date, day, ID, speed = SF)

speeds_ALL <- transmute(S_ALL, date, day, ID, speed, speed_source = "Google API")

write.csv(speeds_ALL, paste0("outputs/all_speedz_", Sys.Date(), ".csv"), row.names = FALSE)

##average speeds for the roads
speeds_AVG <- timeAverage(speeds_ALL, "month", type = c("ID", "speed_source"))
speeds_AVG <- select(speeds_AVG, -date)
speeds_AVG <- left_join(speeds_AVG, all_links, by = "ID")
write.csv(speeds_AVG, paste0("outputs/avg_speeds_", Sys.Date(), ".csv"))
st_write(speeds_AVG, paste0("outputs/speeds_avg_", Sys.Date(), ".geojson"))

S_ALL_Col <- colsplit(S_ALL$ID, "_", names = c("Main", "Sub"))
S_ALL_Ps <- bind_cols(S_ALL, S_ALL_Col)

for (eL in Links){
  tryCatch({
    df2 <- filter(S_ALL, ID == eL)
    p2 <- timeVariation(df2, pollutant = "speed", main = paste0(eL, " Weekly Speeds (kph)"))
    filename <- paste0("plots/", eL, "_", Sys.Date(), ".png")
    png(filename, width=2000, height=1000, units="px", res=160)
    print(p2)
    dev.off()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

