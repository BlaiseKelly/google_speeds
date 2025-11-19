


get_costs <- function(links, cost_per_request = 0.5){

  ##Work out approximate cost of running the API with all the links (?)
  Cost_Estimate <- NROW(unique(links$ID))*168*2*cost_per_request/100
  
  return(Cost_Estimate)
  
}


nextMonday <- function(date) {
  date <- ymd(date)
  .day <- wday(date)
  shift <- ifelse(as.numeric(.day) < 2, 0, 7)
  date + days(2 - as.numeric(.day) + shift)
}


get_future_week_speeds <- function(links){
  
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
  
  kr8d8_hr_min <- colsplit(format(kr8d8$date)," ", names = c("Date", "hh_mm_ss"))
  kr8d8$hh_mm_ss <- kr8d8_hr_min$hh_mm_ss
  kr8d8$Date <- kr8d8_hr_min$Date
  
  Big_speedz_list <- list()
  Big_distance_list <- list()
  Big_time_list <- list()
  
  Links <- unique(links$ID)
  L <- Links[1]
  for(L in Links) {
    
    Section <- filter(links, ID == L)
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
    for (D in datez){
      tryCatch({
        df <- filter(ForWeek, hr_week == D)
        dep_d8 <- as.character(format(df$Date))
        dep_thyme <- paste0(format(df[,7]), " UTC")
        
        ## make request - THIS STEP COSTS MONEY
        calcs_SF <- gmapsdistance(origin = Start, destination = Finish,
                                  mode = "driving", key = api_key,
                                  dep_date = dep_d8, dep_time = dep_thyme, traffic_model = "best_guess")
        
        
        Outs_SF <- do.call(rbind, calcs_SF)
        distance_SF <- as.numeric(Outs_SF[1,1])
        time_SF <- as.numeric(Outs_SF[2,1])
        
        
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
  
  
  
 # save.image(file = paste0("outputs/Google_run_", Sys.Date(), ".RData"))
  
  #load("outputs/Google_run_2021-08-31.RData")
  
  
  BDL_SF <- do.call(rbind, Big_distance_list)
  BTL_SF <- do.call(rbind, Big_time_list)
  S_ALL <- do.call(rbind, Big_speedz_list) |> 
    select(date, day, ID, speed = SF) |> 
    mutate(link_dist_m = BDL_SF$SF,
           time_taken_s = BTL_SF$SF)
  
  return(S_ALL)
  
}
