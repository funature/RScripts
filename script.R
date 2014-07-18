instant_pkgs <- function(pkgs) { 
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages were already loaded!\n")
  }
}

libraries <- c("plyr", "ggplot2", "grid", "gtable", "sp", "Cairo", "gridExtra", "reshape2", "graphics")
instant_pkgs(libraries)


# Some values needed by the program
month_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
winddir_group <- c("North", "NE", "East", "SE", "South", "SW", "West", "NW")

# get location information
getlocation <- function(epwfile) {
  location <- read.table(file = epwfile, sep = ",", nrows = 1)
  names(location) <- c("location", "city", "prov", "country", "source", "wmo", "lat", "long", "elev", "time")
  location
}

doy <- function(month, day){
  output <- sum(month_days[1:month]) - month_days[month] + day
  output
}

#cauculate absolute humidity(ah) from relative humidity(rh), dry bulb temperature(dbt)
# rh2ah <- function(rh, dbt) {
#   ah <- (6.112*exp((17.67*dbt)/(dbt+243.5))*2.164*rh)/(273.15+dbt)
#   ah
# }
rh2ah <- function(rh, dbt) {
  
  #calculate wetbulb temperature assuming sea level pressure 1013.25 mbar
  #formula from NOAA http://www.srh.noaa.gov/epz/?n=wxcalc_rh
  
  
  #some of the following equations are from
  #Professor Wei Fang, Ph.D., Dept. of Bio-Industrial Mechatronics
  #Engineering, National Taiwan University, Email: weifang@ccms.ntu.edu.tw
  
  patm=101.325 # in kPa
  
  #saturation vapour pressure, pws, in kPa
  if (dbt <= 0) {
    A1 <- -5.6745359*10^3
    A2 <- 6.3925247
    A3 <- -9.677843*10^-3
    A4 <- 0.6221570*10^-6
    A5 <- 2.0747825*10^-9
    A6 <- -0.9484024*10^-12
    A7 <- 4.16735019
  }   # for -100 to 0 deg.C
  else   {
    A1 <- -5.8002206*10^3
    A2 <- 1.3914993
    A3 <- -48.640239*10^-3
    A4 <- 41.764768*10^-6
    A5 <- -14.452093*10^-9
    A6 <- 0.0
    A7 <- 6.5459673
  }     # for 0 to 200 deg.C
  dbtk <- dbt+273.15  # change to Kelvin
  SatP <- A1/dbtk+A2+A3*dbtk+A4*dbtk*dbtk+A5*dbtk*dbtk*dbtk+A6*dbtk*dbtk*dbtk*dbtk+A7*log(dbtk)
  pws <- exp(SatP)/1000
  
  #actual water vapour pressure, kPa
  pw <- pws*rh/100
  
  #moisture content or absolute humidity g of water vapour / kg dry air
  ah <- 0.62198*pw/(patm-pw)
  #units in mg of water vapour / kg dry air, use for plotting in
  #psychrometric chart
  ahm<- 0.62198*pw/(patm-pw)*1000
  
  #absolute humidity @saturation g of vapour / kg dry air
  ahs <- 0.62198*pws/(patm-pws)*1000
  ahm
}

winddir_simple <- function(winddir) {
  
  if ((31*360/32 < winddir & winddir <= 360) | winddir <= 1*360/32) {
    winddir_simple = 1 #"Nord" # Nord
  }
  else if (1*360/32 < winddir & winddir <= 3*360/32) {
    winddir_simple = 2 #"NNE"
  }
  else if (3*360/32 < winddir & winddir <= 5*360/32) {
    winddir_simple = 3 #"NE"
  }
  else if (5*360/32 < winddir & winddir <= 7*360/32) {
    winddir_simple = 4 #"NEE"
  }
  else if (7*360/32 < winddir & winddir <= 9*360/32) {
    winddir_simple = 5 #"East" #East
  }
  else if (9*360/32 < winddir & winddir <= 11*360/32) {
    winddir_simple = 6 #"ESE"
  }
  else if (11*360/32 < winddir & winddir <= 13*360/32) {
    winddir_simple = 7 #"SE" #SouthEast
  }
  else if (13*360/32 < winddir & winddir <= 15*360/32) {
    winddir_simple = 8 #"SES"
  }
  else if (15*360/32 < winddir & winddir <= 17*360/32) {
    winddir_simple = 9 #"South"
  }
  else if (17*360/32 < winddir & winddir <= 19*360/32) {
    winddir_simple = 10 #"SSW"
  }
  else if (19*360/32 < winddir & winddir <= 21*360/32) {
    winddir_simple = 11 #"SW"
  }
  else if (21*360/32 < winddir & winddir <= 23*360/32) {
    winddir_simple = 12 #"SWW"
  }
  else if (23*360/32 < winddir & winddir <= 25*360/32) {
    winddir_simple = 13 #"West"
  }
  else if (25*360/32 < winddir & winddir <= 27*360/32) {
    winddir_simple = 14 #"WNW"
  }
  else if (27*360/32 < winddir & winddir <= 29*360/32) {
    winddir_simple = 15 #"NW"
  }
  else if (29*360/32 < winddir & winddir <= 31*360/32) {
    winddir_simple = 16 #"NWN"
  }
  winddir_simple
}

## get hourly data
# !important make sure the header of the file contains 8 rows
#
getdata <- function(epwfile) {
  data <- read.table(file=epwfile, sep = ",", skip = 8)
  # name the columns, more details see http://apps1.eere.energy.gov/buildings/energyplus/pdfs/AuxiliaryPrograms.pdf
  # because the data from different sources have different numbers of comlumn, so I cut the first 32 columns
  # if all columns are needed, change the code a little
  data <- subset(data, j = c(1:32))
  # names(data) <- c("year", "month", "day", "hour", "minute", "datasource", "drybulb", "dewpoint", "relhum", "atmos_pressure", "exthorrad", "extdirrad", "horirsky", "glohorrad", "dirnorrad", "difhorrad", "glohorillum", "dirnorillum", "difhorillum", "zenlum", "winddir", "windspd", "totskycvr", "opaqskycvr", "visibility", "ceiling_hgt", "presweathobs", "presweathcodes", "precip_wtr", "aerosol_opt_depth", "snowdepth", "days_last_snow", "Albedo", "liq_precip_depth", "liq_precip_rate")
  names(data) <- c("year", "month", "day", "hour", "minute", "datasource", "drybulb", "dewpoint", "relhum", "atmos_pressure", "exthorrad", "extdirrad", "horirsky", "glohorrad", "dirnorrad", "difhorrad", "glohorillum", "dirnorillum", "difhorillum", "zenlum", "winddir", "windspd", "totskycvr", "opaqskycvr", "visibility", "ceiling_hgt", "presweathobs", "presweathcodes", "precip_wtr", "aerosol_opt_depth", "snowdepth", "days_last_snow")
  
  for (i in 1:8760) {
    data$winddir_simple[[i]] <- winddir_simple(data$winddir[i])
  }
  
  abshum <- vector()
  for (i in 1:8760){
    abshum[i] <- rh2ah(data$relhum[i], data$drybulb[i])
  }
  data$abshum <- abshum
  
  
  day_of_year <- c(1:8760)
  for (i in 1:8760) {
    data$day_of_year[[i]] <- doy(data[i, "month"], data[i, "day"])
  }
  
  data$TIME <- c(1:8760)
  
  data$enthalpy <- 1.006*data$drybulb + (1.84*data$drybulb + 2501)*data$abshum/1000
  
  data
}

small_fontsize <- unit(5, "mm")
default_fontsize <- unit(8, "mm")
title_fontsize <- unit(10, "mm")

# library(rgdal)
# library(maptools) 
# library(ggmap)
# library(maps)
# library(mapdata)
# library(rworldmap)
# library(ggplot2)
# library(grid)
# library(gtable)
# library(sp)
# library(Cairo)
# library(gridExtra)
# library(reshape2)

# TODO define the regions you want focus, get directly in C#
# maybe better get regions from the locations
getRegions <- function () {
  
}

read.table.solar <- function (file) {
  
}

get.locations <- function () {
  locations <- locationTable$source
}

climate.basic <- function(data){
  t_max <- max(data$drybulb)
  t_min <- min(data$drybulb)
  t_ave <- mean(data$drybulb)
  hum_max <- max(data$abshum)
  hum_min <- min(data$abshum)
  hum_ave <- mean(data$abshum)
  rhum_max <- max(data$relhum)
  rhum_min <- min(data$relhum)
  rhum_ave <- mean(data$relhum)
  output <- c(t_max, t_min, t_ave, hum_max, hum_min, hum_ave, rhum_max, rhum_min, rhum_ave)
}

# read whole data from a file
data.read <- function (path, unit=TRUE) {
  # read data from file
  data <- read.table(file = path, sep = ",", strip.white=T, header = TRUE, stringsAsFactors=FALSE)
  if (unit) {
    # delete the row of units
    data <- data[-1,]
  }
#   as.data.frame.matrix(data)
  return (data)
}

# subset the data with chosen location and period
data.subset <- function (data, locations, periods="Annual") {
  subdata <- subset(data, location %in% locations & period == periods )
}

# subset the data with chosen location and period
locations.subset <- function (data, locations) {
  subdata <- subset(data, source %in% locations)
}

# get the range of scala from the given data 
get.range <- function (data, type) {
    
    if (type == "solar") range <- c(0, max(data[,c(7)]))
    
    if (type == "heat.heating" | type == "heat.cooling") {
      data[, 15:19] <- sapply(data[, 15:19], as.numeric)
      maxvalue <- max(abs(min(data[, 15:19])), abs(max(data[, 15:19])))
      range <- c(-maxvalue, maxvalue)
    }
    
    if (type == "heat.heating.new" | type == "heat.cooling.new") {
      data[, 2:5] <- sapply(data[, 2:5], as.numeric)
      maxvalue <- max(abs(min(data[, 2:5])), abs(max(data[, 2:5])))
      range <- c(-maxvalue, maxvalue)
    }
    
    if (type == "moisture.heating" | type == "moisture.cooling") {
      data[, 25:27] <- sapply(data[, 25:27], as.numeric)
      maxvalue <- max(abs(min(data[,25:27])), abs(max(data[,25:27])))
      range <- c(-maxvalue, maxvalue)
    }
    
    if (type == "moisture.heating.new" | type == "moisture.cooling.new") {
      data[, 6:7] <- sapply(data[, 6:7], as.numeric)
      maxvalue <- max(abs(min(data[,6:7])), abs(max(data[,6:7])))
      range <- c(-maxvalue, maxvalue)
    }
    
    if (type == "balance.heating" | type == "balance.cooling") {
      data[, c("MWINF",  "MWVENT",  "MWIGAIN")] <- sapply(data[, c("MWINF",  "MWVENT",  "MWIGAIN")], as.numeric)
#       data[ , c("MWINF",  "MWVENT",  "MWIGAIN")] <- data[ , c("MWINF",  "MWVENT",  "MWIGAIN")]*100
      tmp <- data[, c("FILENAME", "QTRANS",  "QINF",  "QVENT",  "QGINT",	"QSOL", "MWINF",  "MWVENT",  "MWIGAIN")]
      tmp <- melt(tmp, id="FILENAME")
      tmp$value <- sapply(tmp$value, as.numeric)
      maxvalue <- max(abs(min(tmp$value)), abs(max(tmp$value)))
      range <- c(-maxvalue, maxvalue)
    }

    if (type == "balance.heating.new" | type == "balance.cooling.new") {
    #   data[, c("MWINF",  "MWVENT",  "MWIGAIN")] <- sapply(data[, c("MWINF",  "MWVENT",  "MWIGAIN")], as.numeric)
      #       data[ , c("MWINF",  "MWVENT",  "MWIGAIN")] <- data[ , c("MWINF",  "MWVENT",  "MWIGAIN")]*100
      tmp <- data[, c("SITUATION", "QTRANS",  "QVENTS",  "QSOLAR",  "QINTS",  "QVENTL", "QINTL")]
      tmp <- melt(tmp, id="SITUATION")
      tmp$value <- sapply(tmp$value, as.numeric)
      maxvalue <- max(abs(min(tmp$value)), abs(max(tmp$value)))
      range <- c(-maxvalue, maxvalue)
    }
  
  range
}

# data.single[, c(13:17, 23:25)] <- sapply(data.single[, c(12:16, 22:24)], as.numeric)
# min(data.single[,c(12:16, 22:24)])
# maxvalue <- max(abs(min(data.single[,c(12:16, 22:24)])), abs(max(data.single[,c(12:16, 22:24)])))

# prepare the data to plot 
data.single.solar <- function (data, location.single, str.period) {
  subdata <- subset(data, location == location.single & period == str.period)
  data.plot <- readRowToDataframe(subdata[1, ])
}

# prepare the data to plot 
data.single.heat.heating <- function (data) {
  mdata <- melt(data, id=c("DCK",  "FILENAME",  "TYPE",  "INSU_THICKNESS",  "SLOPE",  "AZIMUTH",  "ID",  "PERIOD",  "SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("heating", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QINF",  "QVENT",	"QGINT",	"QSOL"))
}

data.single.heat.cooling <- function (data) {
  mdata <- melt(data, id=c("DCK",  "FILENAME",  "TYPE",  "INSU_THICKNESS",  "SLOPE",  "AZIMUTH",  "ID",  "PERIOD",  "SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("cooling", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QINF",  "QVENT",  "QGINT",	"QSOL"))
}

data.single.heat.heating.new <- function (data) {
  mdata <- melt(data, id=c("SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("heating", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QVENTS",  "QSOLAR",  "QINTS"))
}

data.single.heat.cooling.new <- function (data) {
  mdata <- melt(data, id=c("SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("cooling", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QVENTS",  "QSOLAR",  "QINTS"))
}

data.single.moisture.heating <- function (data) {
  mdata <- melt(data, id=c("DCK",  "FILENAME",  "TYPE",  "INSU_THICKNESS",  "SLOPE",  "AZIMUTH",  "ID",  "PERIOD",  "SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("heating", mdata$SITUATION) & mdata$variable %in% c("MWINF",  "MWVENT",  "MWIGAIN"))
}

data.single.moisture.cooling <- function (data) {
  mdata <- melt(data, id=c("DCK",  "FILENAME",  "TYPE",  "INSU_THICKNESS",  "SLOPE",  "AZIMUTH",  "ID",  "PERIOD",  "SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("cooling", mdata$SITUATION) & mdata$variable %in% c("MWINF",  "MWVENT",  "MWIGAIN"))
}

data.single.moisture.heating.new <- function (data) {
  mdata <- melt(data, id=c("SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("heating", mdata$SITUATION) & mdata$variable %in% c("QVENTL",  "QINTL"))
}

data.single.moisture.cooling.new <- function (data) {
  mdata <- melt(data, id=c("SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("cooling", mdata$SITUATION) & mdata$variable %in% c("QVENTL",  "QINTL"))
}

data.single.balance.heating <- function (data) {
  data[, c("MWINF",  "MWVENT",  "MWIGAIN")] <- sapply(data[, c("MWINF",  "MWVENT",  "MWIGAIN")], as.numeric)
#   data[ , c("MWINF",  "MWVENT",  "MWIGAIN")] <- data[ , c("MWINF",  "MWVENT",  "MWIGAIN")]*100
  mdata <- melt(data, id=c("DCK",  "FILENAME",  "TYPE",  "INSU_THICKNESS",  "SLOPE",  "AZIMUTH",  "ID",  "PERIOD",  "SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("heating", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QINF",  "QVENT",	"QGINT",	"QSOL", "MWINF",  "MWVENT",  "MWIGAIN"))
}

data.single.balance.cooling <- function (data) {
  data[, c("MWINF",  "MWVENT",  "MWIGAIN")] <- sapply(data[, c("MWINF",  "MWVENT",  "MWIGAIN")], as.numeric)
#   data[ , c("MWINF",  "MWVENT",  "MWIGAIN")] <- data[ , c("MWINF",  "MWVENT",  "MWIGAIN")]*100
  mdata <- melt(data, id=c("DCK",  "FILENAME",  "TYPE",  "INSU_THICKNESS",  "SLOPE",  "AZIMUTH",  "ID",  "PERIOD",  "SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("cooling", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QINF",  "QVENT",  "QGINT",	"QSOL", "MWINF",  "MWVENT",  "MWIGAIN"))
}

data.single.balance.heating.new <- function (data) {
#   data[, c("MWINF",  "MWVENT",  "MWIGAIN")] <- sapply(data[, c("MWINF",  "MWVENT",  "MWIGAIN")], as.numeric)
  #   data[ , c("MWINF",  "MWVENT",  "MWIGAIN")] <- data[ , c("MWINF",  "MWVENT",  "MWIGAIN")]*100
  mdata <- melt(data, id=c("SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("heating", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QVENTS",  "QSOLAR",  "QINTS", "QVENTL",  "QINTL"))
}

data.single.balance.cooling.new <- function (data) {
#   data[, c("MWINF",  "MWVENT",  "MWIGAIN")] <- sapply(data[, c("MWINF",  "MWVENT",  "MWIGAIN")], as.numeric)
  #   data[ , c("MWINF",  "MWVENT",  "MWIGAIN")] <- data[ , c("MWINF",  "MWVENT",  "MWIGAIN")]*100
  mdata <- melt(data, id=c("SITUATION"), na.rm = TRUE)
  data.melt <- subset(mdata, grepl("cooling", mdata$SITUATION) & mdata$variable %in% c("QTRANS",  "QVENTS",  "QSOLAR",  "QINTS", "QVENTL",  "QINTL"))
}

data.plot <- function (data, type) {
  if (type=="solar") data.melt <- data.single.solar(data)
  if (type=="heat.heating") data.melt <- data.single.heat.heating(data)
  if (type=="heat.cooling") data.melt <- data.single.heat.cooling(data)
  if (type=="heat.heating.new") data.melt <- data.single.heat.heating.new(data)
  if (type=="heat.cooling.new") data.melt <- data.single.heat.cooling.new(data)
  if (type=="moisture.heating") data.melt <- data.single.moisture.heating(data)
  if (type=="moisture.cooling") data.melt <- data.single.moisture.cooling(data)
  if (type=="moisture.heating.new") data.melt <- data.single.moisture.heating.new(data)
  if (type=="moisture.cooling.new") data.melt <- data.single.moisture.cooling.new(data)
  if (type=="balance.heating") data.melt <- data.single.balance.heating(data)
  if (type=="balance.cooling") data.melt <- data.single.balance.cooling(data)
  if (type=="balance.heating.new") data.melt <- data.single.balance.heating.new(data)
  if (type=="balance.cooling.new") data.melt <- data.single.balance.cooling.new(data)
  
  data.melt
}

# select $location
# select $period 
# select $type "heating or humidification" or "cooling or dehumidification"

# bilanz.subdata <- function (data, str.location, str.period = "Annual") {
#   subdata <- subset(data, location == str.location & period == str.period)
#   mdata <- melt(subdata, id=c("location",  "longitude",  "latitude",	"elevation",	"analysis",	"period",	"situation"))
#   mdata.e <- subset(mdata, grepl("heating or humidification", mdata$situation) & variable %in% c("QTRANS",  "QINF",	"QVENT",	"QGINT",	"QSOL"))
#   mdata.e.load <- subset(mdata.e, grepl("load", mdata.e$situation))
#   mdata.e.relief <- subset(mdata.e, grepl("relief", mdata.e$situation))
#   mdata.e
# }

readRowToDataframe <- function(rowData) {
  orientation <- rep(NA, length(orientation.list)*length(slope.list))
  slope <- rep(NA, length(orientation.list)*length(slope.list))
  value <- rep(NA, length(orientation.list)*length(slope.list))
  
  for (i in c(1:length(orientation.list))) {
    for (j in c(1:length(slope.list))) {
      col <- paste("A", orientation.list[i], "_S", slope.list[j], sep="")
      if (slope.list[j] == "000")  {
        col <- "A000_S000"
      }
      index <- grep(col, colnames(rowData))
      orientation[length(slope.list)*(i-1)+j] <- i
      slope[length(slope.list)*(i-1)+j] <- j
      value[length(slope.list)*(i-1)+j] <- rowData[1, index]
      #     bubba[4*(i-1)+j,] <- c(i, j, row[1, index])
    }
  }
  
  location <- rowData[1, "location"]
  period <- rowData[1, "period"]
  long <- rowData[1, "longitude"]
  lat <- rowData[1, "latitude"]
  
  table <- data.frame(orientation = orientation, 
                      slope = slope, 
                      value = value)
  output <- list(location=location, period=period, long=long, lat=lat, table=table)
}

getRowData.solar <- function (data, locations, periods="Annual") {
  subdata <- subset(data, location %in% locations & period == periods)
  row <- subdata[1, ]
  table <- readRowToDataframe(row)
}

theme_full <- function () {
  theme <- theme( 
    legend.position = "none",
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin = unit(0,"null"),
    plot.margin = rep(unit(0,"null"),4),
    axis.ticks.length = unit(0,"null"),
    axis.ticks.margin = unit(0,"null")
  )
}

theme.balance <- function () {
  theme <- theme( 
    legend.position = "none",
#     text = element_text(size=default_fontsize, colour="black"),
    axis.title = element_blank(),
    axis.text = element_text(size=default_fontsize, colour="black"),
    axis.text.x = element_text(angle=45, hjust=0.5, vjust=0.5, color="grey20"),
    axis.text.y = element_blank(),
    panel.background = element_rect(colour="black", fill=NA),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
#     panel.grid.major.y = element_line(colour="grey"),
    panel.grid = element_blank(),
    rect = element_blank()
  )
}

theme_no_axis <- function () {
  theme <- theme( 
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
}

draw.map <- function (locations, regions) {
  # get world dataset
  world <- map_data("world")
  
  # create a subset of interested regions
  world_regions <- subset(world, region %in% regions )
  
  lat.min <- min(world_regions$lat)
  lat.max <- max(world_regions$lat)
  long.min <- min(world_regions$long)
  long.max <- max(world_regions$long)
  ylim.min <- (lat.max + lat.min)/2 - 1.1*(lat.max - lat.min)/2
  ylim.max <- (lat.max + lat.min)/2 + 1.1*(lat.max - lat.min)/2
  xlim.min <- (long.max + long.min)/2 - 1.1*(long.max - long.min)/2
  xlim.max <- (long.max + long.min)/2 + 1.1*(long.max - long.min)/2
  
  p <- ggplot() + 
       geom_polygon(data=world, aes(x=long, y=lat, group = group), fill="white", colour = "#DDDDDD") + 
       geom_polygon(data=world_regions, aes(x=long, y=lat, group = group), fill="#636363", colour = "#636363" ) + 
       coord_map(project="mercator", lat0=0) + 
       coord_fixed(ratio = 1, xlim = c(xlim.min, xlim.max), ylim = c(ylim.min, ylim.max)) 
  
  # mark the locations
  p <- p + geom_point(data = locations, aes(x = long, y = lat), size = 2) + 
       geom_text(data = locations, aes(x = long, y = lat, label = location), vjust = -1)
  
  
  # make some theme change
  p <- p + labs(y = NULL) + labs(x = NULL) + theme_full() + theme_no_axis() +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    theme(panel.background = element_rect(fill = "lightblue", colour = NA))
  
  map <- list(plot=p, xmin=xlim.min, xmax=xlim.max, ymin=ylim.min, ymax=ylim.max)
}

map.proportion <- function (map){
  x <- map$xmax - map$xmin
  y <- map$ymax - map$ymin
  proportion <- c(x,y)
}

draw.map.high <- function (regions) {
  
  # create a table to link the locations and shape files
  
  countries = readOGR(dsn=".", layer="CHN_adm1")
  
  countries_df <- fortify(countries)
  
  plot <- ggplot(countries_df) + 
          geom_polygon(aes(long,lat, group=group)) + 
          labs(title="World map (longlat)") + 
          coord_equal()
  
}

# the plot methods to display charts for different purpose
subplot.heat <- function (data, legend.range=NULL) {
  p <- ggplot() +
       geom_bar(data = data, aes(x=variable, y=as.numeric(value), fill=variable), stat="identity",position="identity") +
       geom_hline(yintercept=0) + 
       labs(x = NULL, y = NULL) + 
        theme( 
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
        ) + 
       scale_y_continuous(limits=legend.range)
#   if (max(legend.range) < 500) {p <- p + coord_fixed(ratio = 1/100)}
#   else {p <- p + coord_fixed(ratio = 1/500)}
  p <- p + theme_full()
}

singleplot.heat <- function (data, type, range) {
  
  data.melt <- data.plot(data, type)
  
  data.q.load <- subset(data.melt, variable %in% c("QTRANS",  "QINF",  "QVENT",  "QGINT",  "QSOL") & grepl("load", SITUATION))
  data.q.relief <- subset(data.melt, variable %in% c("QTRANS",  "QINF",  "QVENT",  "QGINT",  "QSOL") & grepl("relief", SITUATION))
  p <- ggplot() +
    geom_bar(data = data.q.load, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#FC98A2")) +
    geom_bar(data = data.q.relief, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#9499DB")) +
    geom_hline(aes(yintercept=0), size=1, color="grey20") +
    geom_text(data=data.q.load, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=-1) + 
    geom_text(data=data.q.relief, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=2) + 
    labs(title = "energy gain and loss [ kWh ]", x = NULL, y = "energy gain and loss [ kWh ]") + 
    guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
    scale_x_discrete(labels=c("transmission \nenergy gain",  "infiltration \nenergy gain",  "ventilation \nenergy gain",  "internal \nenergy gain",  "solar \nenergy gain")) +
    scale_y_continuous(limits=range)+
    scale_fill_identity()
  #   if (max(legend.range) < 500) {p <- p + coord_fixed(ratio = 1/100)}
  #   else {p <- p + coord_fixed(ratio = 1/500)}
  #   print(p)
  p <- p + theme.balance()
  situation <- data.melt$SITUATION[1]
  period <- data.melt$PERIOD[1]
  filename <- data.melt$FILENAME[1]
  if (grepl("heating or humidification", situation) == TRUE) 
    {period <- paste("heating or humidification", period, sep=" - ")}
  if (grepl("cooling or dehumidification", situation) == TRUE) 
    {period <- paste("cooling or dehumidification", period, sep=" - ")}
  title <- grid.text(label=paste(filename, period, sep="\n"), x=0.1, y=0.9, just="left", vjust = 1, gp=gpar(fontsize=default_fontsize, col="black"))
  
  grid.newpage()
  pushViewport(
    viewport(
      layout=grid.layout(2, 2,
                         widths=unit(c(8/16, 8/16),
                                     c("npc", "npc")),
                         heights=unit(c(2, 10),
                                      c("line", "null")))))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.draw(title)
  popViewport()
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
  exampleplot.balance()
  popViewport()
  pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
  print(p, newpage=FALSE)
  popViewport()
}

singleplot.heat.new <- function (data, type, range) {
  
  
  data.melt <- data.plot(data, type)
  
  data.q.load <- subset(data.melt, variable %in% c("QTRANS",  "QVENTS",  "QSOLAR",  "QINTS") & grepl("load", SITUATION))
  data.q.relief <- subset(data.melt, variable %in% c("QTRANS",  "QVENTS",  "QSOLAR",  "QINTS") & grepl("relief", SITUATION))
  p <- ggplot() +
    geom_bar(data = data.q.load, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#9499DB")) +
    geom_bar(data = data.q.relief, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#FC98A2")) +
    geom_hline(aes(yintercept=0), size=1, color="grey20") +
    geom_text(data=data.q.load, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=-1) + 
    geom_text(data=data.q.relief, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=2) + 
    labs(title = "energy gain and loss [ kWh ]", x = NULL, y = "energy gain and loss [ kWh ]") + 
    guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
    scale_x_discrete(labels=c("transmission \nenergy gain",  "ventilation \nenergy gain",  "solar \nenergy gain",  "internal \nenergy gain")) +
    scale_y_continuous(limits=range)+
    scale_fill_identity()
  #   if (max(legend.range) < 500) {p <- p + coord_fixed(ratio = 1/100)}
  #   else {p <- p + coord_fixed(ratio = 1/500)}
  #   print(p)
  p <- p + theme.balance()
  situation <- data.melt$SITUATION[1]
#   period <- data.melt$PERIOD[1]
#   filename <- data.melt$FILENAME[1]
  if (grepl("heating or humidification", situation) == TRUE) 
  {situation <- "heating or humidification"}
  if (grepl("cooling or dehumidification", situation) == TRUE) 
  {situation <- "cooling or dehumidification"}
  title <- grid.text(label=situation, x=0.1, y=0.9, just="left", vjust = 1, gp=gpar(fontsize=default_fontsize, col="black"))
  
  grid.newpage()
  pushViewport(
    viewport(
      layout=grid.layout(2, 2,
                         widths=unit(c(8/16, 8/16),
                                     c("npc", "npc")),
                         heights=unit(c(2, 10),
                                      c("line", "null")))))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.draw(title)
  popViewport()
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
  exampleplot.balance()
  popViewport()
  pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
  print(p, newpage=FALSE)
  popViewport()
}

subplot.moisture <- function (data, legend.range) {
  p <- ggplot(data, aes(x=variable, y=as.numeric(value), fill=factor(variable))) +
    geom_bar(stat="identity",position="identity") +
    geom_hline(yintercept=0) + 
    labs(x = NULL, y = NULL) + 
    theme( 
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) + 
    scale_y_continuous(limits=legend.range)
  p <- p + theme_full()
}

singleplot.moisture <- function (data, type, legend.range) {
  data.melt <- data.plot(data, type)
  
  data.m.load <- subset(data.melt, variable %in% c("MWINF",  "MWVENT",  "MWIGAIN") & grepl("load", SITUATION))
  data.m.relief <- subset(data.melt, variable %in% c("MWINF",  "MWVENT",  "MWIGAIN") & grepl("relief", SITUATION))
  
  p <- ggplot() +
    geom_bar(data = data.melt, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill=paste(SITUATION,variable))) +
    geom_hline(yintercept=0) +
    geom_text(data=data.m.load, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=-0.5) + 
    geom_text(data=data.m.relief, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=1.5) + 
    labs(x = NULL, y = "moisture gain and loss [ kg ]") + 
    theme(legend.position = "none") +
    scale_x_discrete(labels=c("infiltration \nwater gain",  "ventilation \nwater gain",  "internal \nwater gain")) +
    guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
    scale_y_continuous(limits=legend.range)
#   print(p)
  p <- p + theme.balance()
  situation <- data.melt$SITUATION[1]
  period <- data.melt$PERIOD[1]
  filename <- data.melt$FILENAME[1]
  if (grepl("heating or humidification", situation) == TRUE) 
  {period <- paste("heating or humidification", period, sep=" - ")}
  if (grepl("cooling or dehumidification", situation) == TRUE) 
  {period <- paste("cooling or dehumidification", period, sep=" - ")}
  title <- grid.text(label=paste(filename, period, sep="\n"), x=0.1, y=0.9, just="left", vjust = 1, gp=gpar(fontsize=default_fontsize, col="black"))

  grid.newpage()
  pushViewport(
    viewport(
      layout=grid.layout(2, 2,
                         widths=unit(c(8/16, 8/16),
                                     c("npc", "npc")),
                         heights=unit(c(2, 10),
                                      c("line", "null")))))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.draw(title)
  popViewport()
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
  #   print(guide, newpage=FALSE)
  exampleplot.balance()
  popViewport()
  pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
  print(p, newpage=FALSE)
  popViewport()
}

singleplot.moisture.new <- function (data, type, legend.range) {
  data.melt <- data.plot(data, type)
  
  data.m.load <- subset(data.melt, variable %in% c("QVENTL",  "QINTL") & grepl("load", SITUATION))
  data.m.relief <- subset(data.melt, variable %in% c("QVENTL",  "QINTL") & grepl("relief", SITUATION))
  
  p <- ggplot() +
    geom_bar(data = data.m.load, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#9499DB")) +
    geom_bar(data = data.m.relief, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#FC98A2")) +
    geom_hline(yintercept=0) +
    geom_text(data=data.m.load, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=-0.5) + 
    geom_text(data=data.m.relief, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=1.5) + 
    labs(x = NULL, y = "moisture gain and loss [ kg ]") + 
    theme(legend.position = "none") +
    scale_x_discrete(labels=c("ventilation \nlatent gain",  "internal \nlatent gain")) +
    guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
    scale_y_continuous(limits=legend.range)+
    scale_fill_identity()
  #   print(p)
  p <- p + theme.balance()
  situation <- data.melt$SITUATION[1]
#   period <- data.melt$PERIOD[1]
#   filename <- data.melt$FILENAME[1]
  if (grepl("heating or humidification", situation) == TRUE) 
  {situation <- "heating or humidification"}
  if (grepl("cooling or dehumidification", situation) == TRUE) 
  {situation <- "cooling or dehumidification"}
  title <- grid.text(label=situation, x=0.1, y=0.9, just="left", vjust = 1, gp=gpar(fontsize=default_fontsize, col="black"))
  
  grid.newpage()
  pushViewport(
    viewport(
      layout=grid.layout(2, 2,
                         widths=unit(c(8/16, 8/16),
                                     c("npc", "npc")),
                         heights=unit(c(2, 10),
                                      c("line", "null")))))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.draw(title)
  popViewport()
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
  #   print(guide, newpage=FALSE)
  exampleplot.balance()
  popViewport()
  pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
  print(p, newpage=FALSE)
  popViewport()
}

singleplot.balance <- function (data, type, legend.range) {
  data.melt <- data.plot(data, type)
  
  data.q.load <- subset(data.melt, variable %in% c("QTRANS",  "QINF",  "QVENT",  "QGINT",  "QSOL") & grepl("load", SITUATION))
  data.q.relief <- subset(data.melt, variable %in% c("QTRANS",  "QINF",  "QVENT",  "QGINT",  "QSOL") & grepl("relief", SITUATION))
  data.m.load <- subset(data.melt, variable %in% c("MWINF",  "MWVENT",  "MWIGAIN") & grepl("load", SITUATION))
  data.m.relief <- subset(data.melt, variable %in% c("MWINF",  "MWVENT",  "MWIGAIN") & grepl("relief", SITUATION))
  
#   p1 <- ggplot(data, aes(x=variable, y=as.numeric(value), fill=paste(situation,variable))) +
#     geom_bar(stat="identity",position="identity") +
#     geom_hline(yintercept=0) + 
#     labs(x = NULL, y = "energy gain and loss [ kWh ]") + 
#     scale_y_continuous(limits=legend.range, breaks=500*c(-7:7)) +
#     guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
#     scale_x_discrete(labels=c("transmission \nenergy gain",  "infiltration \nenergy gain",  "ventilation \nenergy gain",  "internal \nenergy gain",  "solar \nenergy gain", "infiltration \nwater gain",  "ventilation \nwater gain",  "internal \nwater gain")) 
# #     scale_fill_discrete(name="",
# #                         labels=c("transmission energy gain",  "infiltration energy gain",  "ventilation energy gain",  "internal energy gain",  "solar energy gain", "infiltration water gain",  "ventilation water gain",  "internal water gain"))
# 
# #     coord_fixed(ratio = 1/20)
#   p1 <- p1 + theme.balance()

  plot <- ggplot() +
    geom_bar(data = data.melt, stat="identity",position="identity", aes(x= variable, y=as.numeric(value), fill=paste(SITUATION,variable))) +
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=5.5) + 
    geom_text(data=data.q.load, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=-0.5) + 
    geom_text(data=data.q.relief, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2, vjust=1.5) + 
    geom_text(data=data.m.load, aes(label=round(as.numeric(value), digits = 2), x=variable, y=as.numeric(value)), size=2, vjust=-0.5) + 
    geom_text(data=data.m.relief, aes(label=round(as.numeric(value), digits = 2), x=variable, y=as.numeric(value)), size=2, vjust=1.5) + 
    labs(x = NULL) +     
    scale_x_discrete(labels=c("transmission \nenergy gain",  "infiltration \nenergy gain",  "ventilation \nenergy gain",  "internal \nenergy gain",  "solar \nenergy gain", "infiltration \nwater gain",  "ventilation \nwater gain",  "internal \nwater gain")) +
    scale_y_continuous(limits=legend.range,           
                       breaks=500*c(-7:7),
                       labels=5*c(-7:7)) +
    theme.balance()
  p1 <- plot + labs(y = "energy gain and loss [ kWh ]")
  p2 <- plot + labs(y = "moisture gain and loss [ kg ]")
  
  #extract gtable
  g1<-ggplot_gtable(ggplot_build(p1))
  g2<-ggplot_gtable(ggplot_build(p2))
  
  #overlap the panel of the 2nd plot on that of the 1st plot
  
  pp<-c(subset(g1$layout, name=="panel", se=t:r))
  g<-gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, 
                     pp$l)
  
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

  iylab <- which(g2$layout$name == "ylab")
  gylab <- g2$grobs[[iylab]]
  g <- gtable_add_cols(g, g2$widths[g2$layout[iylab, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, gylab, pp$t, length(g$widths) - 1, pp$b)
  

  situation <- data.melt$SITUATION[1]
  period <- data.melt$PERIOD[1]
  filename <- data.melt$FILENAME[1]
  if (grepl("heating or humidification", situation) == TRUE) 
  {period <- paste("heating or humidification", period, sep=" - ")}
  if (grepl("cooling or dehumidification", situation) == TRUE) 
  {period <- paste("cooling or dehumidification", period, sep=" - ")}
  title <- grid.text(label=paste(filename, period, sep="\n"), x=0.1, y=0.9, just="left", vjust = 1, gp=gpar(fontsize=default_fontsize, col="black"))

  grid.newpage()
  pushViewport(
    viewport(
      layout=grid.layout(2, 2,
                         widths=unit(c(8/16, 8/16),
                                     c("npc", "npc")),
                         heights=unit(c(2, 10),
                                      c("line", "null")))))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.draw(title)
  popViewport()
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
#   print(guide, newpage=FALSE)
  exampleplot.balance()
  popViewport()
  pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
  grid.draw(g)
  popViewport()

#   grid.arrange(arrangeGrob(title,
#                            guide,
#                            ncol=2, nrow=1, widths=c(8, 8)),
#                          arrangeGrob(g), 
#                          nrow=2, heights=c(1, 11))
  
}


singleplot.balance.new <- function (data, type, legend.range) {
  data.melt <- data.plot(data, type)
  
  data.q.load <- subset(data.melt, grepl("load", SITUATION))
  data.q.relief <- subset(data.melt, grepl("relief", SITUATION))
#   data.m.load <- subset(data.melt, variable %in% c("QVENTL",  "QINTL") & grepl("load", SITUATION))
#   data.m.relief <- subset(data.melt, variable %in% c("QVENTL",  "QINTL") & grepl("relief", SITUATION))
  
  #   p1 <- ggplot(data, aes(x=variable, y=as.numeric(value), fill=paste(situation,variable))) +
  #     geom_bar(stat="identity",position="identity") +
  #     geom_hline(yintercept=0) + 
  #     labs(x = NULL, y = "energy gain and loss [ kWh ]") + 
  #     scale_y_continuous(limits=legend.range, breaks=500*c(-7:7)) +
  #     guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
  #     scale_x_discrete(labels=c("transmission \nenergy gain",  "infiltration \nenergy gain",  "ventilation \nenergy gain",  "internal \nenergy gain",  "solar \nenergy gain", "infiltration \nwater gain",  "ventilation \nwater gain",  "internal \nwater gain")) 
  # #     scale_fill_discrete(name="",
  # #                         labels=c("transmission energy gain",  "infiltration energy gain",  "ventilation energy gain",  "internal energy gain",  "solar energy gain", "infiltration water gain",  "ventilation water gain",  "internal water gain"))
  # 
  # #     coord_fixed(ratio = 1/20)
  #   p1 <- p1 + theme.balance()
  
  plot <- ggplot() +
    geom_bar(data = data.q.load, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#FC98A2")) +
    geom_bar(data = data.q.relief, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#00ADEF")) +
#     geom_bar(data = data.m.load, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#9499DB")) +
#     geom_bar(data = data.m.relief, stat="identity",position="identity", width=.5, aes(x= variable, y=as.numeric(value), fill="#FC98A2")) +
    geom_hline(aes(yintercept=0), size=1, color="grey20") + 
    geom_vline(aes(xintercept=4.5), size=0.5, color="grey30", linetype="dotted") + 
    geom_text(data=data.q.load, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2.5, vjust=-0.5, color="grey20") + 
    geom_text(data=data.q.relief, aes(label=round(as.numeric(value), digits = 0), x=variable, y=as.numeric(value)), size=2.5, vjust=1.5, color="grey20") + 
#     geom_text(data=data.m.load, aes(label=round(as.numeric(value), digits = 2), x=variable, y=as.numeric(value)), size=2, vjust=-0.5, color="grey20") + 
#     geom_text(data=data.m.relief, aes(label=round(as.numeric(value), digits = 2), x=variable, y=as.numeric(value)), size=2, vjust=1.5, color="grey20") + 
    labs(x = NULL) +     
    scale_x_discrete(labels=c("transmission",  "ventilation \nsensible",  "solar",  "internal \nsensible", "ventilation \nlatent",  "internal \nlatent")) +
    scale_y_continuous(limits=legend.range,           
                       breaks=500*c(-7:7),
                       labels=5*c(-7:7)) +
    theme.balance()+
    scale_fill_identity()
  p1 <- plot + labs(x = "energy gain and loss [ kWh ]")
  p2 <- plot + labs(x = "moisture gain and loss [ kg ]")
  
  #extract gtable
  g1<-ggplot_gtable(ggplot_build(p1))
  g2<-ggplot_gtable(ggplot_build(p2))
  
  #overlap the panel of the 2nd plot on that of the 1st plot
  
  pp<-c(subset(g1$layout, name=="panel", se=t:r))
  g<-gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, 
                     pp$l)
  
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
#   ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  iylab <- which(g2$layout$name == "ylab")
  gylab <- g2$grobs[[iylab]]
  g <- gtable_add_cols(g, g2$widths[g2$layout[iylab, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, gylab, pp$t, length(g$widths) - 1, pp$b)
  
  
  situation <- data.melt$SITUATION[1]
#   period <- data.melt$PERIOD[1]
#   filename <- data.melt$FILENAME[1]
  if (grepl("heating or humidification", situation) == TRUE) 
  {situation <- "\nheating or humidification"}
  if (grepl("cooling or dehumidification", situation) == TRUE) 
  {situation <- "\ncooling or dehumidification"}
  title <- grid.text(label=situation, x=0.1, y=0.9, just="left", vjust = 1, gp=gpar(fontsize=title_fontsize, col="black"))
  
  grid.newpage()
  pushViewport(
    viewport(
      layout=grid.layout(2, 2,
                         widths=unit(c(8/16, 8/16),
                                     c("npc", "npc")),
                         heights=unit(c(2, 10),
                                      c("line", "null")))))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.draw(title)
  popViewport()
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
  #   print(guide, newpage=FALSE)
  exampleplot.balance()
  popViewport()
  pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=2))
  grid.draw(g)
  popViewport()
  
  #   grid.arrange(arrangeGrob(title,
  #                            guide,
  #                            ncol=2, nrow=1, widths=c(8, 8)),
  #                          arrangeGrob(g), 
  #                          nrow=2, heights=c(1, 11))
  
}

subplot.solar <- function (data, legend.range) {
  p <- ggplot(data$table, aes(x=orientation,
                        y=slope,
                        fill = value)) + 
    geom_rect(aes(xmin=0.5, xmax=12.5, ymin=0.5, ymax=4.5), fill="white", colour="white") +
    geom_tile() + 
#     geom_text(aes(x=orientation,
#                   y=slope,
#                   label = value)) +
#     geom_hline(aes(yintercept=slope+0.5), size=1, colour="white") +
#     geom_segment(aes(x=orientation+0.5, xend=orientation+0.5, y = 1.5, yend = 4.5), colour="white", size=1) +
    labs(x = NULL, y = NULL) + 
    scale_x_continuous(breaks=3*c(1:4), labels=c("W", "N", "E", "S")) +
    theme( 
      axis.ticks = element_line(colour="black"),
      axis.ticks.y = element_blank(),
      axis.text = element_text(colour="black"),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    scale_fill_gradient(limits=legend.range, low = "yellow", high = "red", space = "Lab") +
    coord_polar(theta="x", start=pi*13/12)
  
    p <- p + theme_full() 
}

# singleplot.solar <- function (data, legend.range) {
#   p <- ggplot(data=data, aes(x=AZIMUTH/30, y=SLOPE/15, fill = QE/1000)) + 
#     geom_rect(aes(xmin=-0.5, xmax=11.5, ymin=-0.5, ymax=6.5), fill="white", colour="white") +
#     geom_tile() + 
# #     geom_text(aes(label=paste(orientation.list[orientation], orientation.list[slope]))) +
#     labs(x = NULL, y = NULL) + 
#     scale_x_continuous(breaks=3*c(1:4)-3, labels=c("South", "West", "North", "East")) +
#     theme(
#           text = element_text(size=12),
#           panel.background = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.ticks = element_line(colour="black"),
#           axis.ticks.y = element_blank(),
#           axis.text = element_text(colour="black"),
#           axis.text.y = element_blank(),
#           axis.title.y = element_blank()
#     ) +
#     scale_fill_gradient(limits=legend.range/1000, low = "yellow", high = "red", space = "Lab", guide = guide_colourbar(title = "annual solar energy \n[ kWp ]")) +
#     coord_polar(theta="x", start=-pi*13/12)
#     
#     sample <- exampleplot.solar()
#     legend <- g_legend(p)
# 
#     grid.newpage()
#     pushViewport(
#       viewport(
#         layout=grid.layout(3, 2,
#                            widths=unit(c(10/16, 6/16),
#                                         c("npc", "npc")),
#                            heights=unit(c(2, 5, 5),
#                                          c("line", "null", "null")))))
#     pushViewport(viewport(layout.pos.col=1:2, layout.pos.row=1))
#     grid.text(paste(data$FILENAME, "\nenergy output from photovoltaic systems \nwith different orientations", sep=" "), x=unit(5, "mm"), just=c("left", "top"), gp=gpar(fontsize=title_fontsize, col="black"))
#     popViewport()
#     pushViewport(viewport(layout.pos.col=1, layout.pos.row=2:3))
#     print(p+theme(legend.position = "none"), newpage=FALSE)
#     popViewport()
#     pushViewport(viewport(layout.pos.col=2, layout.pos.row=3))
#     grid.draw(legend)
#     popViewport()
#     pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
#     print(sample, newpage=FALSE)
#     popViewport()
# #     plot <- grid.arrange(arrangeGrob(p + theme(legend.position="none")), 
# #                          arrangeGrob(sample,
# #                                      legend,
# #                                      ncol=1, nrow=2),
# #                          ncol=2, widths=c(10, 5))
# }
multiline_text <- function(label){
  grid.table(as.matrix(strsplit(label, "\\n")[[1]]), parse=TRUE,
             theme=theme.list(gpar.corefill = gpar(fill = NA, col = NA),
                              core.just = "center"))
}

singleplot.solar <- function (data, legend.range) {
  row.max <- which.max(data$QE)
  slope <- data$SLOPE[row.max]
  azimuth <- data$AZIMUTH[row.max]
  QE <- data$QE[row.max]/1000
  p <- ggplot(data=data) + 
    geom_rect(aes(xmin=-0.5, xmax=15.5, ymin=-0.5, ymax=6.5), fill="white", colour="white") +
    geom_tile(aes(x=AZIMUTH/22.5, y=SLOPE/15, fill = QE/1000)) + 
    annotate("rect", xmin=data$AZIMUTH[row.max]/22.5-0.5, xmax=data$AZIMUTH[row.max]/22.5+0.5, ymin=data$SLOPE[row.max]/15-0.5, ymax=data$SLOPE[row.max]/15+0.5, colour="black", fill=NA) +
    annotate("text", x = 8, y = c(0:6), label = c(0:6)*15, size=2, color="gray30") +
#     geom_text(aes(label=paste(orientation.list[orientation], orientation.list[slope]))) +
    labs(x = NULL, y = NULL) + 
    scale_x_continuous(breaks=c(0:15), labels=c("South", "SSW", "SW", "WSW", "West", "WNW", "NW", "NNW", "North", "NNE", "NE", "ENE", "East", "ESE", "SE", "SSE")) +
    theme(
          text = element_text(size=12),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA, colour=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size=8),
          legend.text = element_text(size=8),
          axis.ticks = element_line(colour="black"),
          axis.ticks.y = element_blank(),
          axis.text = element_text(colour="gray30", size=8),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
    ) +
    scale_fill_gradient(limits=legend.range/1000, low = "yellow", high = "red", space = "Lab", guide = guide_colourbar(title = expression(paste("kWh/", plain("("), kW[p], plain("a)"))))) +
    coord_polar(theta="x", start=-pi*17/16)
    
    sample <- exampleplot.solar()
    legend <- g_legend(p)

#     text <- c(expression(bquote(paste("maximum: ", .(round(QE,0)), "kWh/", plain("("), kW[p], plain("a)")))),
#              paste("slope: ", slope, "?", sep=""),
#              paste("azimuth: ", azimuth, "?", sep=""))

    text <- c("maximum: ",
              expression(bquote(paste(.(round(QE,0)), "kWh/", plain("("), kW[p], plain("a)")))),
              "slope: ",
              paste(slope, "?", sep=""),
              "azimuth: ",
              paste(azimuth, "?", sep=""))

    grid.newpage()
    pushViewport(
      viewport(
        layout=grid.layout(3, 2,
                           widths=unit(c(2/3, 1/3),
                                        c("npc", "npc")),
                           heights=unit(c(2/10, 4/10, 4/10),
                                         c("npc", "npc", "npc")))))
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
    grid.text(paste(data$FILENAME, "\nenergy output from photovoltaic systems \nwith different slope and orientations", sep=" "), x=unit(5, "mm"), just=c("left", "center"), gp=gpar(fontsize=title_fontsize, col="black"))
    popViewport()
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=2:3))
    print(p+theme(legend.position = "none"), newpage=FALSE)
    popViewport()
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
    grid.draw(legend)
    popViewport()
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=3))
#     grid.text(eval(text[1]), x=0, y=unit(0.5,"npc")+unit(1,"lines"), just="left", gp = gpar(fontsize = 10))
#     grid.text(eval(text[2]), x=0, y=unit(0.5,"npc"), just="left", gp = gpar(fontsize = 10))
#     grid.text(eval(text[3]), x=0, y=unit(0.5,"npc")-unit(1,"lines"), just="left", gp = gpar(fontsize = 10))

grid.text(eval(text[1]), x=0.1, y=unit(0.5,"npc")+unit(1,"lines"), just="left", vjust=1, gp = gpar(fontsize = 8))
grid.text(eval(text[2]), x=0.4, y=unit(0.5,"npc")+unit(1,"lines"), just="left", vjust=1, gp = gpar(fontsize = 8))
grid.text(eval(text[3]), x=0.1, y=unit(0.5,"npc"), just="left", vjust=1, gp = gpar(fontsize = 8))
grid.text(eval(text[4]), x=0.4, y=unit(0.5,"npc"), just="left", vjust=1, gp = gpar(fontsize = 8))
grid.text(eval(text[5]), x=0.1, y=unit(0.5,"npc")-unit(1,"lines"), just="left", vjust=1, gp = gpar(fontsize = 8))
grid.text(eval(text[6]), x=0.4, y=unit(0.5,"npc")-unit(1,"lines"), just="left", vjust=1, gp = gpar(fontsize = 8))
    popViewport()
#     plot <- grid.arrange(arrangeGrob(p + theme(legend.position="none")), 
#                          arrangeGrob(sample,
#                                      legend,
#                                      ncol=1, nrow=2),
#                          ncol=2, widths=c(10, 5))
}

# singleplot.solar <- function (data, legend.range) {
#   p <- ggplot(data=data, aes(x=AZIMUTH/30, y=SLOPE/15, fill = QE/1000)) + 
#     geom_rect(aes(xmin=-0.5, xmax=11.5, ymin=-0.5, ymax=6.5), fill="white", colour="white") +
#     geom_tile() + 
#     #     geom_text(aes(label=paste(orientation.list[orientation], orientation.list[slope]))) +
#     labs(title = paste(data$FILENAME, "\nenergy output from photovoltaic systems \nwith different orientations", sep=" "), x = NULL, y = NULL) + 
#     scale_x_continuous(breaks=3*c(1:4)-3, labels=c("South", "West", "North", "East")) +
#     theme(
#       text = element_text(size=12),
#       panel.background = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.ticks = element_line(colour="black"),
#       axis.ticks.y = element_blank(),
#       axis.text = element_text(colour="black"),
#       axis.text.y = element_blank(),
#       axis.title.y = element_blank()
#     ) +
#     scale_fill_gradient(limits=legend.range/1000, low = "yellow", high = "red", space = "Lab", guide = guide_colourbar(title = "annual solar energy \n[ kWp ]")) +
#     coord_polar(theta="x", start=-pi*13/12)
#   
# }

exampleplot.solar <- function () {
  p <- qplot(1:10, 1:10/2, geom="blank") +
    annotation_custom(grob=circleGrob(x=0.4, y=0.5, r=c(0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)/2)) +
    annotation_custom(grob=segmentsGrob(0.4, 0.475+c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)/2, 0.7, 0.475+c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)/2, gp=gpar(col="black", lty="dotted"))) +
    annotation_custom(grob=textGrob(c("0", "15", "30", "45", "60", "75", "90"), 0.75, 0.475+c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)/2, gp=gpar(fontsize=small_fontsize, col="black"))) +
    labs(title = "slope") +                 
    theme( 
      legend.position="none",
      text = element_text(size=10),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

exampleplot.balance <- function () {
  polygon <- grid.polygon(x=c(.02, .05, .08, .02, .05, .08), y=c(.5, .2, .5, .6, .9, .6), id=rep(1:2, each=3), gp=gpar(fill=c("#00ADEF", "#FC98A2"), col=NA))
  text <- grid.text(c("relief", "load"), 0.1, c(0.4, 0.75), just = "centre", hjust=0, gp=gpar(fontsize=default_fontsize, col="black"))
  grid.draw(polygon)
  grid.draw(text)
}
# make plot for each location

# get relative position

relativePosition <- function (long, lat, map) {
  
#   location <- find.location(locations, locationTable)
  
  x <- (long-map$xmin)/(map$xmax-map$xmin)
  y <- (lat-map$ymin)/(map$ymax-map$ymin)
  position <- c(x,y)
}

subplot <- function (data, type, legend.range=NULL) {
  
  plotType <- function (data, type, legend.range){
    switch (type,
            heat.heating = subplot.heat(data, legend.range),
            heat.cooling = subplot.heat(data, legend.range),
            moisture.heating = subplot.moisture(data, legend.range),
            moisture.cooling = subplot.moisture(data, legend.range),
            balance.heating = subplot.balance(data, legend.range),
            balance.cooling = subplot.balance(data, legend.range),
            solar = subplot.solar(data, legend.range))
  }
  
  p <- plotType(data, type, legend.range)
}

singleplot <- function (data, type, legend.range) {
#   data is melt
  
  plotType <- function (data, type, legend.range){
    switch (type,
            solar = singleplot.solar(data, legend.range),
            heat.heating = singleplot.heat(data, type, legend.range),
            heat.cooling = singleplot.heat(data, type, legend.range),
            heat.heating.new = singleplot.heat.new(data, type, legend.range),
            heat.cooling.new = singleplot.heat.new(data, type, legend.range),
            moisture.heating = singleplot.moisture(data, type, legend.range),
            moisture.cooling = singleplot.moisture(data, type, legend.range),
            moisture.heating.new = singleplot.moisture.new(data, type, legend.range),
            moisture.cooling.new = singleplot.moisture.new(data, type, legend.range),
            balance.heating = singleplot.balance(data, type, legend.range),
            balance.cooling = singleplot.balance(data, type, legend.range),
            balance.heating.new = singleplot.balance.new(data, type, legend.range),
            balance.cooling.new = singleplot.balance.new(data, type, legend.range))
  }
  
  p <- plotType(data, type, legend.range)
}

plot.example <- function(data, type, legend.range){
  plot <- subplot(data, type, legend.range) + theme(legend.position = "right")
}

plot.subplot <- function (data, type, legend.range=NULL, position=NULL, i=NULL) {
  position.x <- position[1]
  position.y <- position[2]
  
  pushViewport(viewport(name=paste("subplot_", i),
                        x=position.x,
                        y=position.y,
                        width=0.2,
                        height=0.2,
                        just = c("center","center"),
                        clip=TRUE))
  subplot <- subplot(data, type, legend.range)
  print(subplot, newpage=FALSE)
  popViewport()
}

plot.chart <- function (data, type, locationNames, locationTable, regions, high = FALSE) {
  locations <- find.location(locationNames, locationTable)
  
  mainMap <- draw.map(locations, regions)
  proportion <- map.proportion(mainMap)
  
  # open a new page
  grid.newpage()
  # prepare the layout for plot
  pushViewport(
    viewport(name="maplay",
             layout=grid.layout(1,1,
                                widths=proportion[1],
                                heights=proportion[2],
                                respect=TRUE)))
  pushViewport(viewport(name="mapvp",
                        layout.pos.row=1,
                        layout.pos.col=1,
                        clip=TRUE))
  # used to test the propety is right
#   grid.rect(width=1, height=1, gp=gpar(col="blue",fill="blue"))
  
  print(mainMap$plot, newpage=FALSE)
  
#   popViewport()
  
  if (high) {
    mainMap <- draw.map.high(regions)
    print(regionMap, newpage=FALSE)
  }
  
  for(i in 1:nrow(locations)) {
    position <- relativePosition(locations$location[i], mainMap)
    subplot <- plot.subplot(data, type, position, i)
  }
}

plot.map <- function (data, type, locationNames, regions, high = FALSE, periods="Annual") {
  locations.selected <- locations.subset(locationTable, locationNames)
  
  mainMap <- draw.map(locations.selected, regions)
  proportion <- map.proportion(mainMap)
  
  # open a new page
  grid.newpage()
  # prepare the layout for plot
  pushViewport(
    viewport(name="maplay",
             layout=grid.layout(1,1,
                                widths=proportion[1],
                                heights=proportion[2],
                                respect=TRUE)))
  pushViewport(viewport(name="mapvp",
                        layout.pos.row=1,
                        layout.pos.col=1,
                        clip=TRUE))
  # used to test the propety is right
  #   grid.rect(width=1, height=1, gp=gpar(col="blue",fill="blue"))
  
  print(mainMap$plot, newpage=FALSE)
  
  #   popViewport()
  
  if (high) {
    mainMap <- draw.map.high(regions)
    print(regionMap, newpage=FALSE)
  }
  
  data.selected <- data.subset(data, locationNames, periods)
  
  legend.range <- get.range(data.selected, type)
  
  # print plot of each selected location
  for(i in 1:nrow(locations.selected)) {
    position <- relativePosition(locations.selected$long[i], locations.selected$lat[i], mainMap)
    
    data.single <- subset(data.selected, location == locations.selected$source[i])
    
    data.plot <- data.plot(data.single, type, locations.selected$source[i], periods)
    
    plot.subplot(data.plot, type, legend.range, position, i)
  }
  
  example <- plot.example(data.plot, type, legend.range)
  pushViewport(viewport(name="example",
                        x=1,
                        y=0,
                        width=0.4,
                        height=0.4,
                        just = c("right","bottom"),
                        clip=TRUE))
  print(example, newpage=FALSE)
}

save.map <- function (data, type, locationNames, regions, high = FALSE, periods="Annual", file="mapPlot", format = "png", width=1000, units="px", proportion=cavasProportion, dpi=300) {
  Cairo(file=paste(file, format, sep="."), 
        type=format,
        units=units, 
        width=width, 
        height=width*proportion, 
        pointsize=12,
        dpi=dpi,
        bg="white")
  
  plot.map(data, type, locationNames, regions, high = FALSE, periods)
  

  dev.off() 
}

plot.single <- function (data, type, legend.range=NULL) {
  if (is.null(legend.range)) 
    range <- get.range(data, type)
  else
    range <- legend.range
  singleplot(data, type, range)
}

save.batch <- function (locations, types, periods) {
  for (i in 1:length(locations)) {
    location <- locations[i]
    for (j in 1:length(types)) {
      type <- types[j]
      if (type == "solar") {data <- solarTable}
      if (type == "heat.heating" | type == "heat.cooling" | type == "moisture.heating" | type == "moisture.cooling" | type == "balance.heating" | type == "balance.cooling") {data <- balanceTable}
      
      subdata <- data.subset(data, locations, periods)
      range <- get.range(subdata, type)
      data.current <- data.subset(data, location, periods)
      file <- paste(location, type, periods, sep="_")
      save.single(data.current, type, location, periods, range, file, format = "png", width=16, height=12, units="cm", dpi=300)
    }
  }
}

# save.batch(locations=c('DEU_Berlin.103840_IWEC', 'DEU_Dusseldorf.104000_IWEC'), types=c('solar', 'heat.heating', 'heat.cooling', 'moisture.heating', 'moisture.cooling', 'balance.heating', 'balance.cooling'), period='Annual')

save.single <- function (data, type, legend.range=NULL, file="singlePlot", format = "png", width=16, height=12, units="cm", dpi=300) {
  Cairo(file=paste(file, format, sep="."), 
        type=format,
        units=units, 
        width=width, 
        height=height,
        pointsize=12,
        dpi=dpi,
        bg="white")
# data is not melt   
  p <- plot.single(data, type, legend.range)
  print (p)
  
  dev.off()
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# legend <- g_legend(p)

# grid.arrange(legend, p+ theme(legend.position = 'none'), 
#              ncol=2, nrow=1, widths=c(1/6,5/6))

solarsingletest <- function () {
  grid.newpage()
  vplay <- grid.layout(2, 2,
                       widths = unit(c(10, 6), "null"),
                       heights = unit(c(5, 5), "null"))
  pushViewport(viewport(layout=vplay))
  vp.main <- viewport(layout.pos.row = 1,
           layout.pos.col = 1)
  vp.sample <- viewport(layout.pos.row = 1,
                        layout.pos.col = 2)
  vp.legend <- viewport(layout.pos.row = 2,
                        layout.pos.col = 2)
  pushViewport(vp.main)
  print(p+theme(legend.position = 'none'))
#   popViewport()
  pushViewport(vp.sample)
  print(test)
#   popViewport()
  pushViewport(vp.legend)
  print (legend)

p3 <- grid.arrange(arrangeGrob(p + theme(legend.position="none")), 
                   arrangeGrob(test + theme(legend.position="none"),
                               legend,
                               ncol=1, nrow=2, respect=TRUE),
                   ncol=2, widths=c(10, 5))
}

# test area


# locationTable <- data.read("C:\\Users\\ilkyilu\\Documents\\Visual Studio 2013\\Projects\\RPlotForm\\RPlotForm\\data\\Orte.txt", FALSE)
# balanceTable <- data.read("C:\\Users\\ilkyilu\\Documents\\Visual Studio 2013\\Projects\\RPlotForm\\RPlotForm\\data\\CELL-template_A000_anual_output_summary.plt")
# solarTable <- data.read("C:\\Users\\ilkyilu\\Documents\\Visual Studio 2013\\Projects\\RPlotForm\\RPlotForm\\data\\PV-MuC-template_sum_of_power_output.sum")

# data.single <- data.subset(balanceTable, "DEU_Berlin.103840_IWEC", "Annual")
# legend.range <- get.range(data.single, "balance.cooling")
# save.single (balanceTable, "heat.cooling", "DEU_Berlin.103840_IWEC", periods="June")

# TODO should get directly in C#
# locationTable <- read.table(file = "Orte.txt", sep = ",", strip.white=T, header = TRUE, stringsAsFactors=FALSE)
# as.data.frame.matrix(locationTable) 
# 
# 
# regions <- c("Germany")
# 
# locationNames <- c("DEU_Berlin.103840_IWEC")


# needed 
# locations <- locations.subset(locationTable, locationNames)


# needed 
# mainMap <- draw.map(locations, regions)
# proportion <- map.proportion(mainMap)

# needed 
# cavasProportion <- proportion[2]/proportion[1]

# save.single(data=balanceTable, type="balance.heating", location="DEU_Berlin.103840_IWEC")
# 
# plot.map(data=balanceTable, type="bilanz", locationNames, regions, periods="Annual")

# save.chart(data=mtcars, type="pie", locationNames, locationTable, regions)

# balanceTable <- data.read("D:/singleplot.tmp", FALSE)
# mdata <- data.single.heat.heating(balanceTable)
# save.single("balanceTable", "heat.heating", c(-100:100))
# 
# subdata.heat <- subset(data, QHEAT>0)
# subdata.cool <- subset(data, QCOOL>0)
# 
# ggplot(subdata.heat) +geom_point(aes(x=INSU_THICKNESS, y=QHEAT, shape=factor(SLOPE), color=factor(SITUATION)))+
#   geom_line(aes(x=INSU_THICKNESS, y=QHEAT))+
#   geom_point(data=subdata.cool, aes(x=INSU_THICKNESS, y=QCOOL, shape=factor(SLOPE), color=factor(SITUATION)))+
#   geom_line(data=subdata.cool, aes(x=INSU_THICKNESS, y=QCOOL))
# 
#   geom_text(data=subdata.cool, aes(x=INSU_THICKNESS, y=QCOOL, label=SLOPE))


# balanceTable <- data.read("D:/singleplot.tmp", FALSE)
# 
# statisticTable <- data.read("D:/stat.tmp", FALSE)
# 
# save.single(balanceTable, "balance.heating", file="test")

plot.stat <- function (plottype, file="statPlot", format = "png", width=16, height=12, units="cm", dpi=300){
#   View(summaryTable)
  Cairo(file=paste(file, format, sep="."), 
        type=format,
        units=units, 
        width=width, 
        height=height, 
        pointsize=12,
        dpi=dpi,
        bg="white")
  p <- switch(plottype,
           "0" = plot.stat.heating(),
           "1" = plot.stat.cooling(),
           "2" = plot.stat.balance(),
           "3" = plot.stat.heating.primary(),
           "4" = plot.stat.cooling.primary(),
           "5" = plot.stat.balance.primary())
  
  print (p)
  
  dev.off()
}

plot.stat.heating <- function (){
  p <- ggplot(data=summaryTable) + 
    geom_point(aes(x=INSU_THICKNESS, y=abs(QHEAT), group=interaction(FILENAME, SLOPE, AZIMUTH), shape=factor(SLOPE)))+
    geom_line(aes(x=INSU_THICKNESS, y=abs(QHEAT), group=interaction(FILENAME, SLOPE, AZIMUTH)))
}

plot.stat.cooling <- function (){
  p <- ggplot(data=summaryTable) + 
    geom_point(aes(x=INSU_THICKNESS, y=abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH), shape=factor(SLOPE)))+
    geom_line(aes(x=INSU_THICKNESS, y=abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH)))
}

plot.stat.balance <- function (){
  p <- ggplot(data=summaryTable) + 
    geom_point(aes(x=INSU_THICKNESS, y=abs(QHEAT)+abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH), shape=factor(SLOPE)))+
    geom_line(aes(x=INSU_THICKNESS, y=abs(QHEAT)+abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH))) +
    geom_point(aes(x=INSU_THICKNESS, y=abs(QHEAT)+abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH), shape=factor(SLOPE)))+
    geom_line(aes(x=INSU_THICKNESS, y=abs(QHEAT)+abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH)))
}

plot.stat.heating.primary <- function (){
  primaryTable <- energy.primary(summaryTable, "heater")
  p <- ggplot(data=primaryTable) + 
    geom_point(aes(x=INSU_THICKNESS, y=Primary_Heater, color=factor(Heater)))
#     geom_line(aes(x=INSU_THICKNESS, y=abs(QHEAT), group=interaction(FILENAME, SLOPE, AZIMUTH)))
}

plot.stat.cooling.primary <- function (){
  primaryTable <- energy.primary(summaryTable, "cooler")
  p <- ggplot(data=primaryTable) + 
    geom_point(aes(x=INSU_THICKNESS, y=Primary_Cooler, color=factor(Cooler)))
#     geom_point(aes(x=INSU_THICKNESS, y=abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH), shape=factor(SLOPE)))+
#     geom_line(aes(x=INSU_THICKNESS, y=abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH)))
}

plot.stat.balance.primary <- function (){
  primaryTable <- energy.primary(summaryTable, "balance")
  p <- ggplot(data=primaryTable) + 
    geom_point(aes(x=INSU_THICKNESS, y=Primary_Heater+Primary_Cooler, color=factor(Setting)))
#     geom_point(aes(x=INSU_THICKNESS, y=abs(QHEAT), group=interaction(FILENAME, SLOPE, AZIMUTH), shape=factor(SLOPE)))+
#     geom_line(aes(x=INSU_THICKNESS, y=abs(QHEAT), group=interaction(FILENAME, SLOPE, AZIMUTH))) +
#     geom_point(aes(x=INSU_THICKNESS, y=abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH), shape=factor(SLOPE)))+
#     geom_line(aes(x=INSU_THICKNESS, y=abs(QCOOL), group=interaction(FILENAME, SLOPE, AZIMUTH)))
}

statisticTable.summary <- function (data) {
  statisticTable.summary <- ddply(data, .(DCK, FILENAME, TYPE, INSU_THICKNESS, SLOPE, AZIMUTH, ID, PERIOD), summarise, QHEAT = sum(QHEAT), QCOOL = sum(QCOOL))
}

# energy.primary <- function (data, heater, cooler){
#   factor.h <- switch(heater, "0" = 1.43, "1" = 1.32, "2" = 0.84)
#   factor.c <- switch(cooler, "0" = 0.756, "1" = 1.176, "2" = 1.1)
#   data[paste("Primary_Heater", heater, "_Cooler", cooler, sep="")] <- abs(factor.h * data$QHEAT) + abs(factor.c * data$QCOOL)
#   data
# }

# energy.primary <- function (data, type, heater=NULL, cooler=NULL){
#   factor.h <- switch(heater, "0" = 1.43, "1" = 1.32, "2" = 0.84)
#   factor.c <- switch(cooler, "0" = 0.756, "1" = 1.176, "2" = 1.1)
#   if (type =="heater"){
#     data[paste("Primary_", type, "_", heater, sep="")] <- abs(factor.h * data$QHEAT)
#   }
#   if (type =="cooler"){
#     data[paste("Primary_", type, "_", cooler, sep="")] <- abs(factor.c * data$QCOOL)
#   }
#   if (type =="balance"){
#     data[paste("Primary_", type, "_", heater, "_", cooler, sep="")] <- abs(factor.h * data$QHEAT) + abs(factor.c * data$QCOOL)
#   }
#   data
# }

energy.primary <- function (data, type){
#   heater <- c("0", "1", "2")
#   cooler <- c("0", "1", "2")
  if (type == "heater"){
    cnames <- c(colnames(data), c("Heater", "Primary_Heater"))
    primaryTable <- as.data.frame(setNames(replicate(length(cnames),numeric(0), simplify = F), cnames))
    # loop the energy table   
    nr <- 0
    for (i in 1:nrow(data)){
      for (j in 1:length(heaters)){
        nr <- nr + 1
        factor.h <- switch(heaters[j], "0" = 1.43, "1" = 1.32, "2" = 0.84)
        row <- data[i,]
        row["Heater"] <- j
        row["Primary_Heater"] <- abs(factor.h * row["QHEAT"])
        primaryTable[nr,] <- row
      }
    }
  }
  if (type == "cooler"){
    cnames <- c(colnames(data), c("Cooler", "Primary_Cooler"))
    primaryTable <- as.data.frame(setNames(replicate(length(cnames),numeric(0), simplify = F), cnames))
    # loop the energy table   
    nr <- 0
    for (i in 1:nrow(data)){
      for (j in 1:length(coolers)){
        nr <- nr + 1
        factor.c <- switch(coolers[j], "0" = 0.756, "1" = 1.176, "2" = 1.1)
        row <- data[i,]
        row["Cooler"] <- j
        row["Primary_Cooler"] <- abs(factor.c * row["QCOOL"])
        primaryTable[nr,] <- row
      }
    }
  }
  if (type == "balance"){
    cnames <- c(colnames(data), c("Setting", "Heater", "Cooler", "Primary_Heater", "Primary_Cooler"))
    primaryTable <- as.data.frame(setNames(replicate(length(cnames),numeric(0), simplify = F), cnames))
    # loop the energy table   
    nr <- 0
    for (i in 1:nrow(data)){
      for (j in 1:length(heaters)){
        for (k in 1:length(coolers)){
          nr <- nr + 1
          factor.h <- switch(heaters[j], "0" = 1.43, "1" = 1.32, "2" = 0.84)
          factor.c <- switch(coolers[k], "0" = 0.756, "1" = 1.176, "2" = 1.1)
          row <- data[i,]
          row["Setting"] <- paste("Heater", j, "Cooler", k, sep="_")
          row["Heater"] <- j
          row["Cooler"] <- k
          row["Primary_Heater"] <- abs(factor.h * row["QHEAT"])
          row["Primary_Cooler"] <- abs(factor.c * row["QCOOL"])
          primaryTable[nr,] <- row
        }
      }
    }
  }
  primaryTable
}

data.summary <- function (data){
  keeps <- c("QHCS",  "QHCL",	"QTRANS",	"QVENTS",	"QVENTL",	"QSOLAR",	"QINTS",	"QINTL")
  data <- data[keeps]
  data$QHCS <- as.numeric(data$QHCS)
  data$QHCL <- as.numeric(data$QHCL)
  data$QTRANS <- as.numeric(data$QTRANS)
  data$QVENTS <- as.numeric(data$QVENTS)
  data$QVENTL <- as.numeric(data$QVENTL)
  data$QSOLAR <- as.numeric(data$QSOLAR)
  data$QINTS <- as.numeric(data$QINTS)
  data$QINTL <- as.numeric(data$QINTL)
  data.heating <- subset(data, QHCS < 0)
  data.cooling <- subset(data, QHCS > 0)
  data.humidification <- subset(data, QHCL < 0)
  data.dehumidification <- subset(data, QHCL > 0)
  QTRANS.heating.load <- sum(subset(data.heating$QTRANS, data.heating$QTRANS < 0))
  QTRANS.heating.relief <- sum(subset(data.heating$QTRANS, data.heating$QTRANS > 0))
  QVENTS.heating.load <- sum(subset(data.heating$QVENTS, data.heating$QVENTS < 0))
  QVENTS.heating.relief <- sum(subset(data.heating$QVENTS, data.heating$QVENTS > 0))
  QSOLAR.heating.load <- sum(subset(data.heating$QSOLAR, data.heating$QSOLAR < 0))
  QSOLAR.heating.relief <- sum(subset(data.heating$QSOLAR, data.heating$QSOLAR > 0))
  QINTS.heating.load <- sum(subset(data.heating$QINTS, data.heating$QINTS < 0))
  QINTS.heating.relief <- sum(subset(data.heating$QINTS, data.heating$QINTS > 0))
  QVENTL.heating.load <- sum(subset(data.humidification$QVENTL, data.humidification$QVENTL < 0))
  QVENTL.heating.relief <- sum(subset(data.humidification$QVENTL, data.humidification$QVENTL > 0))
  QINTL.heating.load <- sum(subset(data.humidification$QINTL, data.humidification$QINTL < 0))
  QINTL.heating.relief <- sum(subset(data.humidification$QINTL, data.humidification$QINTL > 0))
  
  heating.load <- c("heating or humidification - load", QTRANS.heating.load, QVENTS.heating.load, QSOLAR.heating.load, QINTS.heating.load)
  heating.relief <- c("heating or humidification - relief", QTRANS.heating.relief, QVENTS.heating.relief, QSOLAR.heating.relief, QINTS.heating.relief)
  
  QTRANS.cooling.load <- sum(subset(data.cooling$QTRANS, data.cooling$QTRANS > 0))
  QTRANS.cooling.relief <- sum(subset(data.cooling$QTRANS, data.cooling$QTRANS < 0))
  QVENTS.cooling.load <- sum(subset(data.cooling$QVENTS, data.cooling$QVENTS > 0))
  QVENTS.cooling.relief <- sum(subset(data.cooling$QVENTS, data.cooling$QVENTS < 0))
  QSOLAR.cooling.load <- sum(subset(data.cooling$QSOLAR, data.cooling$QSOLAR > 0))
  QSOLAR.cooling.relief <- sum(subset(data.cooling$QSOLAR, data.cooling$QSOLAR < 0))
  QINTS.cooling.load <- sum(subset(data.cooling$QINTS, data.cooling$QINTS > 0))
  QINTS.cooling.relief <- sum(subset(data.cooling$QINTS, data.cooling$QINTS < 0))
  QVENTL.cooling.load <- sum(subset(data.dehumidification$QVENTL, data.dehumidification$QVENTL > 0))
  QVENTL.cooling.relief <- sum(subset(data.dehumidification$QVENTL, data.dehumidification$QVENTL < 0))
  QINTL.cooling.load <- sum(subset(data.dehumidification$QINTL, data.dehumidification$QINTL > 0))
  QINTL.cooling.relief <- sum(subset(data.dehumidification$QINTL, data.dehumidification$QINTL < 0))
  
  cooling.load <- c("cooling or dehumidification - load", QTRANS.heating.load, QVENTS.heating.load, QSOLAR.heating.load, QINTS.heating.load)
  cooling.relief <- c("cooling or dehumidification - relief", QTRANS.heating.relief, QVENTS.heating.relief, QSOLAR.heating.relief, QINTS.heating.relief)
  
  SITUATION <- c("heating or humidification - load", "heating or humidification - relief", "cooling or dehumidification - load", "cooling or dehumidification - relief")
  QTRANS <- c(QTRANS.heating.load, QTRANS.heating.relief, QTRANS.cooling.load, QTRANS.cooling.relief)
  QVENTS <- c(QVENTS.heating.load, QVENTS.heating.relief, QVENTS.cooling.load, QVENTS.cooling.relief)
  QSOLAR <- c(QSOLAR.heating.load, QSOLAR.heating.relief, QSOLAR.cooling.load, QSOLAR.cooling.relief)
  QINTS <- c(QINTS.heating.load, QINTS.heating.relief, QINTS.cooling.load, QINTS.cooling.relief)
  QVENTL <- c(QVENTL.heating.load, QVENTL.heating.relief, QVENTL.cooling.load, QVENTL.cooling.relief)
  QINTL <- c(QINTL.heating.load, QINTL.heating.relief, QINTL.cooling.load, QINTL.cooling.relief)
  summary <- data.frame(SITUATION, QTRANS, QVENTS, QSOLAR, QINTS, QVENTL, QINTL)
  summary[,c(2:7)] <- abs(summary[,c(2:7)])/1000
  summary[c(2,4),c(2:7)] <- -summary[c(2,4),c(2:7)]
  summary
}


# test area
# climatedata <- getdata("D:/CLIMATEDATA/CHN_Beijing.Beijing.545110_IWEC/CHN_Beijing.Beijing.545110_IWEC.epw")
# location.info <- getlocation("D:/CLIMATEDATA/CHN_Beijing.Beijing.545110_IWEC/CHN_Beijing.Beijing.545110_IWEC.epw")
# test <- climate.basic(climatedata)
# 
# value <- c(0.1)
# lab <- c(expression(bquote(paste(.(round(QE,0)), "kWh/", plain("("), kW[p], plain("a)")))),
#          expression(bquote(paste(.(value[1]*100), " and percentiles1", sep=""))),
#          bquote(expression(.(value[1]*100)*" and percentiles2")),
#          bquote(paste(.(value[1]*100), " and percentiles3", sep="")) )
# grid.newpage()
# grid.text(eval(lab[1]), x=0.5, y=unit(1,"npc")-unit(1,"lines"))
# grid.text(eval(lab[2]), x=0.5, y=unit(1,"npc")-unit(2,"lines"))
# grid.text(eval(lab[3]), x=0.5, y=unit(1,"npc")-unit(3,"lines"))
# grid.text(lab[4], x=0.5, y=unit(1,"npc")-unit(4,"lines"))



plot.insu <- function(data, type, range = 168){
  
  data$doy <- (data$TIME - 1) %/% 24 + 1
  
  switch(type,
         bar={
           p <- ggplot(data=data) + 
             geom_bar(aes(x=TIME, y=THICKNESS), width = 1, stat="identity")
         },
         boxplot={
           p <- ggplot(data=data, aes(x=factor(TIME%/%range), y=THICKNESS), environment=environment()) + 
             stat_boxplot(fill = "red") + 
             stat_summary(fun.y=mean, colour="blue", geom="point")
         },
         ydensity={
           p <- ggplot(data=data, aes(x=factor(TIME%/%range), y=THICKNESS), environment=environment()) + 
             stat_ydensity(fill = "red")
         },
         step={
           data <- data[order(data$THICKNESS, decreasing = TRUE),]
           data$order <- c(1:nrow(data))
           
           # Cumulative Fraction Function
           p <- ggplot(data=data) + 
             geom_step(aes(x=order, y=THICKNESS))
         }
    )
  p + theme_full()
}

save.insu <- function(data, type, range = 168, file="singlePlot", format = "png", width=16, height=12, units="cm", dpi=300){
  Cairo(file=paste(file, format, sep="."), 
        type=format,
        units=units, 
        width=width, 
        height=height,
        pointsize=12,
        dpi=dpi,
        bg="white")
  p <- plot.insu(data, type, range)
  print (p)
  dev.off()
}

plot.epw <- function(data, variable, type, range = 168, base){
  
  switch(type,
         bar={
           p <- ggplot(data=data, aes(x=TIME, y=abs(get(variable)-base)), environment=environment()) + 
             geom_bar(stat="identity")
         },
         boxplot={
           p <- ggplot(data=data, aes(x=factor(TIME%/%range), y=abs(get(variable)-base)), environment=environment()) + 
             stat_boxplot(fill = "red") + 
             stat_summary(fun.y=mean, colour="blue", geom="point")
         },
         ydensity={
           p <- ggplot(data=data, aes(x=factor(TIME%/%range), y=abs(get(variable)-base)), environment=environment()) + 
             stat_ydensity(fill = "red")
         },
#          step={
#            data <- data[order(data$THICKNESS, decreasing = TRUE),]
#            data$order <- c(1:nrow(data))
#            
#            # Cumulative Fraction Function
#            p <- ggplot(data=data, environment=environment()) + 
#              geom_step(aes(x=order, y=THICKNESS))
#          }
  )
  p + theme_full()
}

save.epw <- function(data, variable, type, range = 168, base, file="singlePlot", format = "png", width=16, height=12, units="cm", dpi=300){
  Cairo(file=paste(file, format, sep="."), 
        type=format,
        units=units, 
        width=width, 
        height=height,
        pointsize=12,
        dpi=dpi,
        bg="white")
  p <- plot.epw(data, variable, type, range, base)
  print (p)
  dev.off()
}

setwd("D:/Adaptive/Plot")

plot.adaptive <- function (filename){
  data <- data.read(paste("D:/Adaptive/", filename, ".epw_Optimal_InsuThickness_hourly.txt", sep=""), FALSE)
  
  save.insu(data, "bar", file=paste(filename, "_optInsu_bar", sep=""))
  save.insu(data, "boxplot", file=paste(filename, "_optInsu_boxplot", sep=""))
  save.insu(data, "ydensity", file=paste(filename, "_optInsu_ydensity", sep=""))
  save.insu(data, "step", file=paste(filename, "_optInsu_step", sep=""))
}

plot.epw.batch <- function (filename){
  data <- getdata(paste("D:/Adaptive/climatedata/", filename, ".epw", sep=""))
  
  base<-21
  
  save.epw(data, "drybulb", "bar", base=base, file=paste(filename, "_drybulb_bar", sep=""))
  save.epw(data, "drybulb", "boxplot", base=base, file=paste(filename, "_drybulb_boxplot", sep=""))
  save.epw(data, "drybulb", "ydensity", base=base, file=paste(filename, "_drybulb_ydensity", sep=""))
  
  base<-60
  
  save.epw(data, "relhum", "bar", base=base, file=paste(filename, "_relhum_bar", sep=""))
  save.epw(data, "relhum", "boxplot", base=base, file=paste(filename, "_relhum_boxplot", sep=""))
  save.epw(data, "relhum", "ydensity", base=base, file=paste(filename, "_relhum_ydensity", sep=""))
  
  base <- rh2ah(60, 21)
  
  save.epw(data, "abshum", "bar", base=base, file=paste(filename, "_abshum_bar", sep=""))
  save.epw(data, "abshum", "boxplot", base=base, file=paste(filename, "_abshum_boxplot", sep=""))
  save.epw(data, "abshum", "ydensity", base=base, file=paste(filename, "_abshum_ydensity", sep=""))
  
  base = 1.006*21 + (1.84*21 + 2501)*rh2ah(60, 21)/1000
  
  save.epw(data, "enthalpy", "bar", base=base, file=paste(filename, "_enthalpy_bar", sep=""))
  save.epw(data, "enthalpy", "boxplot", base=base, file=paste(filename, "_enthalpy_boxplot", sep=""))
  save.epw(data, "enthalpy", "ydensity", base=base, file=paste(filename, "_enthalpy_ydensity", sep=""))
}

plot.adaptive("CHN_Beijing.Beijing.545110_IWEC")
plot.adaptive("CHN_Chongqing.Chongqing.Shapingba.575160_SWERA")
plot.adaptive("CHN_Fujian.Xiamen.591340_CSWD")
plot.adaptive("CHN_Gansu.Lanzhou.528890_IWEC")
plot.adaptive("CHN_Guangdong.Guangzhou.592870_IWEC")
plot.adaptive("CHN_Guangdong.Shenzhen.594930_SWERA")
plot.adaptive("CHN_Guangxi.Zhuang.Nanning.594310_CSWD")
plot.adaptive("CHN_Hebei.Leting.545390_CSWD")
plot.adaptive("CHN_Heilongjiang.Harbin.509530_IWEC")
plot.adaptive("CHN_Henan.Zhengzhou.570830_CSWD")
plot.adaptive("CHN_Hubei.Wuhan.574940_CSWD")
plot.adaptive("CHN_Hunan.Changsha.576870_SWERA")
plot.adaptive("CHN_Jiangsu.Nanjing.582380_CSWD")
plot.adaptive("CHN_Liaoning.Dalian.546620_SWERA")
plot.adaptive("CHN_Liaoning.Shenyang.543420_CSWD")
plot.adaptive("CHN_Nei.Mongol.Hohhot.534630_CSWD")
plot.adaptive("CHN_Qinghai.Xining.528660_CSWD")
plot.adaptive("CHN_Shaanxi.Xian.570360_CSWD")
plot.adaptive("CHN_Shanghai.Shanghai.583670_IWEC")
plot.adaptive("CHN_Shanxi.Taiyuan.537720_CSWD")
plot.adaptive("CHN_Sichuan.Chengdu.562940_SWERA")
plot.adaptive("CHN_Tianjin.Tianjin.545270_CSWD")
plot.adaptive("CHN_Tibet.Lhasa.555910_SWERA")
plot.adaptive("CHN_Xinjiang.Urgur.Urumqi.514630_IWEC")
plot.adaptive("CHN_Xinjiang.Uygur.Yining.514310_CSWD")
plot.adaptive("CHN_Yunnan.Kunming.567780_IWEC")

plot.epw.batch("CHN_Beijing.Beijing.545110_IWEC")
plot.epw.batch("CHN_Chongqing.Chongqing.Shapingba.575160_SWERA")
plot.epw.batch("CHN_Fujian.Xiamen.591340_CSWD")
plot.epw.batch("CHN_Gansu.Lanzhou.528890_IWEC")
plot.epw.batch("CHN_Guangdong.Guangzhou.592870_IWEC")
plot.epw.batch("CHN_Guangdong.Shenzhen.594930_SWERA")
plot.epw.batch("CHN_Guangxi.Zhuang.Nanning.594310_CSWD")
plot.epw.batch("CHN_Hebei.Leting.545390_CSWD")
plot.epw.batch("CHN_Heilongjiang.Harbin.509530_IWEC")
plot.epw.batch("CHN_Henan.Zhengzhou.570830_CSWD")
plot.epw.batch("CHN_Hubei.Wuhan.574940_CSWD")
plot.epw.batch("CHN_Hunan.Changsha.576870_SWERA")
plot.epw.batch("CHN_Jiangsu.Nanjing.582380_CSWD")
plot.epw.batch("CHN_Liaoning.Dalian.546620_SWERA")
plot.epw.batch("CHN_Liaoning.Shenyang.543420_CSWD")
plot.epw.batch("CHN_Nei.Mongol.Hohhot.534630_CSWD")
plot.epw.batch("CHN_Qinghai.Xining.528660_CSWD")
plot.epw.batch("CHN_Shaanxi.Xian.570360_CSWD")
plot.epw.batch("CHN_Shanghai.Shanghai.583670_IWEC")
plot.epw.batch("CHN_Shanxi.Taiyuan.537720_CSWD")
plot.epw.batch("CHN_Sichuan.Chengdu.562940_SWERA")
plot.epw.batch("CHN_Tianjin.Tianjin.545270_CSWD")
plot.epw.batch("CHN_Tibet.Lhasa.555910_SWERA")
plot.epw.batch("CHN_Xinjiang.Urgur.Urumqi.514630_IWEC")
plot.epw.batch("CHN_Xinjiang.Uygur.Yining.514310_CSWD")
plot.epw.batch("CHN_Yunnan.Kunming.567780_IWEC")

filename <- "CHN_Beijing.Beijing.545110_IWEC"

plot.solar.line <- function(filename){
  data <- data.read(paste("D:/Results_PV-MuC/", filename, ".epw_PV-MuC-template.txt", sep=""), FALSE)
  
  data$QE <- data$QE/1000
#   data <- data[order(data$AZIMUTH, decreasing = FALSE),]
  data.major <- subset(data, AZIMUTH %in% c(0, 90, 180, 270))
  data.minor <- subset(data, AZIMUTH %in% c(45, 135, 225, 315))
  data.min <- subset(data, AZIMUTH %in% c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5))
  data.90 <- subset(data, SLOPE==90)
  data.90 <- data.90[order(data.90$QE, decreasing = TRUE),]
  mean.90 <- (max(data.90$QE) + min(data.90$QE))/2
  p <- ggplot(data=data, environment=environment()) +
  geom_line(aes(x=SLOPE, y=QE, group=AZIMUTH, order = AZIMUTH, color=factor(AZIMUTH), linetype=rep(factor(c(1, 2, 5, 2)), 28)), linetype=0, size=0.5) +
  geom_line(data = data.major, aes(x=SLOPE, y=QE, group=AZIMUTH, order = AZIMUTH, color=factor(AZIMUTH)), linetype = 1, size=1.5) +
  geom_line(data = data.minor, aes(x=SLOPE, y=QE, group=AZIMUTH, order = AZIMUTH, color=factor(AZIMUTH)), linetype = 5, size=1) +
  geom_line(data = data.min, aes(x=SLOPE, y=QE, group=AZIMUTH, order = AZIMUTH, color=factor(AZIMUTH)), linetype = 3, size=1) +
#     xlim(0, 110) +
  scale_x_continuous(limits=c(0,90), breaks=15*c(0:6), expand = c(0,0)) +
  scale_y_continuous(limits=c(-200,1500), breaks=300*c(0:5), expand = c(0,0)) +
#   scale_colour_manual(values = c("#FF00FF", "#FF40bf", "#ff8080", "#ffbf40", "#ffff00", "#bfbf30", "#808040", "#404030", "#000000", "#304040", "#408080", "#30bfbf", "#00ffff", "#40bfff", "#8080ff", "#bf40ff")) +
  
  scale_colour_manual(values = c("#00ff00", "#00bb44", "#008888", "#0044bb", "#0000ff", "#0000bb", "#000088", "#000044", "#000000", "#440000", "#880000", "#bb0000", "#ff0000", "#bb4400", "#888800", "#44bb00")) +
#   geom_text(data=data.90, size=2, color="gray50", aes(label=data.90$AZIMUTH, hjust=0, vjust=0.5, x=100, y=mean.90+50*(c(15:0)-7.5))) +
#     geom_segment(data=data.90, color="gray50", linetype=2, aes(x=91, y=QE, xend=99, yend=mean.90+50*(c(15:0)-7.5))) +
    theme( 
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border =element_rect(size=1, color="black", fill=NA),
      panel.grid.major = element_line(linetype=3, color="black"),
#       panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
#       panel.margin = unit(0,"null"),
#       plot.margin = rep(unit(0,"null"),4),
#       legend.position = c("bottom"),
      text = element_text(size=6),
      legend.title = element_text(size=8),
      legend.text = element_text(size=6),
      axis.title = element_text(size=12),
      axis.text = element_text(size=8, color="black"),
      legend.position = "none",
      legend.key = element_rect(fill=NA)
    ) +
  labs(x="slope [ ° ]", y=expression(paste("energy output from PV system [ kWh/", plain("("), kW[p], plain("a) ]"))))+
  guides(color=guide_legend(title="Azimuth [ ° ]", override.aes = list(fill=NA, linetype=rep(c(1, 5, 3, 5), 4), size=rep(c(1.5, 1, 0.8, 1), 4))))
  p
}

legend.pv <- function(){
  sample <- data.frame(x=22.5*c(0:16))
  major <- data.frame(x=90*c(0, 1, 2, 3))
  minor <- data.frame(x=45*(2*c(0, 1, 2, 3)+1))
  min <- data.frame(x=45*c(0:7)+22.5)
  p <- ggplot(data=sample, environment=environment()) + 
    geom_point(aes(x=x, y=0, color=factor(x)), size=0)+
    ylim(0, 7) +
    scale_colour_manual(values = c("#00ff00", "#00bb44", "#008888", "#0044bb", "#0000ff", "#0000bb", "#000088", "#000044", "#000000", "#440000", "#880000", "#bb0000", "#ff0000", "#bb4400", "#888800", "#44bb00", "#00ff00")) +
    geom_segment(data=major, aes(x = x, y = 1.5, xend = x, yend = 6.5, color=factor(x)), linetype = 1, size=1.5) +
    geom_segment(data=minor, aes(x = x, y = 1.5, xend = x, yend = 6.5, color=factor(x)), linetype = 5, size=1) +
    geom_segment(data=min, aes(x = x, y = 1.5, xend = x, yend = 6.5, color=factor(x)), linetype = 3, size=0.8) +
    coord_polar(start = pi)+
    scale_x_continuous(limits=c(0, 360), breaks=22.5*c(0:15), labels=c("S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE")) +
    theme(
      axis.title=element_blank(),
      axis.text.x=element_text(size=6),
      axis.text.y=element_blank()
      )+
    theme_full()
  p
}


plot.solar.table <- function(filename){
  data <- data.read(paste("D:/Results_PV-MuC/", filename, ".epw_PV-MuC-template.txt", sep=""), FALSE)
  
  df.sum <- ddply(data,~AZIMUTH,summarise,max=round(max(QE)/1000))
  sub <- subset(data, SLOPE==0)
  df.sum$horizontal <- sub$QE/1000
  sub <- subset(data, SLOPE==90)
  df.sum$vertical <- sub$QE/1000
  df.sum$hp <- paste(round(df.sum$horizontal/df.sum$max*100),"%")
  df.sum$vp <- paste(round(df.sum$vertical/df.sum$max*100), "%")
  df.sum$AZIMUTH <- c("S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE")
  
  tableGrob(df.sum[,c(1,2,5,6)], 
            cols = c("azimuth", "max", "h", "v"),
            show.rownames=FALSE, 
            gp=gpar(fontsize=7, lwd=2), 
            padding.v = unit(1.4, "lines"),
            gpar.colfill = gpar(fill = NA, col = "black"),
            gpar.corefill = gpar(fill = NA, col = "black"))
}

save.solar.line <- function (filename, legend.range=NULL, file="singlePlot", format = "png", width=16, height=12, units="cm", dpi=300) {
  file=paste(filename, "_PV_line", sep="")
  Cairo(file=paste(file, format, sep="."), 
        type=format,
        units=units, 
        width=width, 
        height=height,
        pointsize=12,
        dpi=dpi,
        bg="white")
  # data is not melt   
  p <- plot.solar.line(filename)
  
  grid.newpage()
  pushViewport(
    viewport(
      layout=grid.layout(1, 2,
                         widths=unit(c(7/10, 3/10),
                                      c("npc", "npc"))
                         )))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  
  vp <- viewport(width = 0.35, height = 0.35, x = unit(4, "lines"),
                 y = unit(2.5, "lines"), just = c("left",
                                                "bottom"))
  print(p, newpage=FALSE)
  print(legend.pv(), vp = vp, newpage=FALSE)
  grid.draw(textGrob(filename, x=unit(0.93, "npc"), y=unit(0.13, "npc"), just = "right",gp=gpar(fontsize=7)))
  popViewport()
  
  pushViewport(viewport(layout.pos.col=2, layout.pos.row=1, just="top"))
  grob <- plot.solar.table(filename)
  width <- grobWidth(grob)
  height <- grobHeight(grob)
  grid.draw(grobTree(grob, vp=viewport(x=width*0.5,
                                     y=unit(1,"npc") - height*0.5 - unit(1, "lines"))))
  popViewport()
  
  dev.off()
}

setwd("D:/pv_diagramme")

save.solar.line("CHN_Beijing.Beijing.545110_IWEC")
save.solar.line("CHN_Chongqing.Chongqing.Shapingba.575160_SWERA")
save.solar.line("CHN_Fujian.Xiamen.591340_CSWD")
save.solar.line("CHN_Gansu.Lanzhou.528890_IWEC")
save.solar.line("CHN_Guangdong.Guangzhou.592870_IWEC")
save.solar.line("CHN_Guangdong.Shenzhen.594930_SWERA")
save.solar.line("CHN_Guangxi.Zhuang.Nanning.594310_CSWD")
save.solar.line("CHN_Hebei.Leting.545390_CSWD")
save.solar.line("CHN_Heilongjiang.Harbin.509530_IWEC")
save.solar.line("CHN_Henan.Zhengzhou.570830_CSWD")
save.solar.line("CHN_Hubei.Wuhan.574940_CSWD")
save.solar.line("CHN_Hunan.Changsha.576870_SWERA")
save.solar.line("CHN_Jiangsu.Nanjing.582380_CSWD")
save.solar.line("CHN_Liaoning.Dalian.546620_SWERA")
save.solar.line("CHN_Liaoning.Shenyang.543420_CSWD")
save.solar.line("CHN_Nei.Mongol.Hohhot.534630_CSWD")
save.solar.line("CHN_Qinghai.Xining.528660_CSWD")
save.solar.line("CHN_Shaanxi.Xian.570360_CSWD")
save.solar.line("CHN_Shanghai.Shanghai.583670_IWEC")
save.solar.line("CHN_Shanxi.Taiyuan.537720_CSWD")
save.solar.line("CHN_Sichuan.Chengdu.562940_SWERA")
save.solar.line("CHN_Tianjin.Tianjin.545270_CSWD")
save.solar.line("CHN_Tibet.Lhasa.555910_SWERA")
save.solar.line("CHN_Xinjiang.Urgur.Urumqi.514630_IWEC")
save.solar.line("CHN_Xinjiang.Uygur.Yining.514310_CSWD")
save.solar.line("CHN_Yunnan.Kunming.567780_IWEC")

