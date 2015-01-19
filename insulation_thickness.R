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

data <- data.read("EnergyBalance.txt", FALSE)

# summarize the value where the ID is same
# Grouping & Summarizing Data in R, check http://www.slideshare.net/jeffreybreen/grouping-summarizing-data-in-r

library(plyr)
library(ggplot2)
data.summury <- ddply(data,.(ID, INSU_THICKNESS),summarise,qheat=sum(abs(QHEAT)), qcool=sum(abs(QCOOL)))

data.summury$material <- "porenbeton"
pf_heat <- 1.1
pf_cool <- 0.8

for (i in 1:length(data.summury$INSU_THICKNESS)){
  
  qp_heat_delta <- pf_heat*(data.summury$qheat[i-1]-data.summury$qheat[i])
  qp_cool_delta <- pf_cool*(data.summury$qcool[i-1]-data.summury$qcool[i])
  qp_total_delta <- qp_heat_delta + qp_cool_delta
  thickness_delta <- (data.summury$INSU_THICKNESS[i]-data.summury$INSU_THICKNESS[i-1])*100
  
  # how much can 1cm of 1 m2 facade can save in one year
#   if(i>1) data.summury$save[i] <- (qp_total_delta/thickness_delta)/18
  
  # nur heizen
  
  if(i>1) data.summury$save[i] <- (qp_heat_delta/thickness_delta)/18
}

# for (i in 1:length(data.summury$INSU_THICKNESS)){
#   if(i>1) data.summury$save[i] <- pf_heat*(data.summury$qheat[i-1]-data.summury$qheat[i])/(data.summury$INSU_THICKNESS[i]-data.summury$INSU_THICKNESS[i-1])
# }

# convert the value to other materials
# in the simulation porenbeton was used

# Step 1: Create material database

lifecycle <- 30

data.summury$save_total <- data.summury$save * lifecycle

data <- na.omit(data.summury)


# for loop doesn't work for ggplot2 
p <- ggplot(data=data)

reconst <- function(data, lamda){
  # data$save are the value for total 36m2 area
  gg.data <- data.frame(x = data$INSU_THICKNESS*lamda/0.09, y = data$save_total)
}

add_layer <- function (data, p, lamda){
  gg.data <- reconst(data, lamda)
  p <- p + geom_line(data=gg.data, aes(x=x, y=y))
}

for (i in 1:10){
  p <- add_layer(data, p, i*0.005)
}

p <- p + scale_x_continuous(limits=c(0, 0.3))
p <- p + scale_y_continuous(limits=c(-1, 150))

# the function to get the x value from data frame with the y value
# TODO set x and y axis also as parameter
get_target <- function(data, y_seed){
  
  for (i in 1:(length(data$y)-1)){
    
    target <- data$y[i]
    
    if( !is.na(target) & target>y_seed){
      x1 <- data$x[i]
      x2 <- data$x[i+1]
      y1 <- data$y[i]
      y2 <- data$y[i+1]
    }
    else{
      break
    }
  }
  
  x_seed <- x1-((x1-x2)*(y1-y_seed)/(y1-y2))
  
}

# materials = c("EPS", "Glaswolle", "PIR", "Steinwolle", "PU", "VIP", "Schaumglas", "Holzfaserplatte", "Aerogel", "XPS") 
# 
# lamda <- c(0.035, 0.033, 0.023, 0.04, 0.026, 0.007, 0.042, 0.04, 0.015, 0.031)
# 
# embodied <- c(853.69, 688.07, 2550, 564.82, 2907.69, 10586.67, 2340, 1612, 25000, 2860)
# 
# matlib <- data.frame(mat=materials, lamda=lamda, embodied=embodied)

library(xlsx)

matlib <- read.xlsx("D:/Stoffe_Datenbank_Jochum.xlsx", 1)

p.lamda2embodied <- ggplot(data=matlib, aes(x=lamda, y=embodied)) + geom_point() + geom_text(aes(label=mat)) + scale_y_continuous(limits=c(0, 1500))

.e <- environment()

for (i in 1:nrow(matlib)){
  lamda <- matlib$lamda[i]
  seed_data <- reconst(data, lamda)
  
  # embodied energy of each cm insulation for facade area, here 18 m2
  
#   matlib$y_seed[i] <- matlib$embodied[i] * 0.01 * 18 / 3.6
  matlib$y_seed[i] <- matlib$embodied[i] * 0.01
  
  matlib$x_seed[i] <- get_target(seed_data, matlib$y_seed[i])
}



# p <- p + geom_point(aes(x=get_target(seed_data, y_seed), y=y_seed), environment = .e) + geom_text(data=mataes(label=))
p <- p + geom_point(data=matlib, aes(x=x_seed, y=y_seed)) + geom_text(data=matlib, aes(x=x_seed, y=y_seed, label=mat))

# hello