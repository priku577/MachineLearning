set.seed(1234567890)
library(geosphere)
#file encoding is given for linux.CAn be removed if it is a windows system
stations <- read.csv("/home/george/Documents/732A95/LAB3/stations.csv",sep = ",",fileEncoding = 'WINDOWS-1252')
temps <- read.csv("/home/george/Documents/732A95/LAB3/temps50k.csv",sep = ",")
temps<-temps[which( as.POSIXct(temps$date) < as.POSIXct("2015-07-12")),]
st <- merge(stations,temps,by="station_number")
st <- st[,c("longitude", "latitude" , "date", "time", "air_temperature")]
st$time<-as.POSIXct(paste(Sys.Date(), st$time), 
                    format="%Y-%m-%d %H:%M:%S")

#Kernel's smoothing coefficient or width
h_distance <- 10000000
h_time <-7
h_date <- 12

#Predicted place, day and time
a <- 58.4274
b <- 14.826
date <- c("2015-07-12") # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00","08:00:00","10:00:00","12:00:00","14:00:00","16:00:00",
           "18:00:00","20:00:00","22:00:00","24:00:00")
temp_add <- vector(length=length(times))
temp_mult <- vector(length=length(times))

Predicted = data.frame(date=rep(date,length(times)), time=(times), longitude=rep(a,length(times)), latitude=rep(b,length(times)))
Predicted$time<-as.POSIXct(paste(Sys.Date(), Predicted$time), 
                           format="%Y-%m-%d %H:%M:%S")

# Three gaussian kernels
distance_pos = function(current_pos,predicted_pos){
  dist = distHaversine(current_pos,predicted_pos) 
  return(exp(-(dist/ h_distance)^2))
  
}
distance_day = function(current_date,predicted_date){
  dist = as.numeric(difftime(current_date,predicted_date, units = "days")) 
  return(exp(-(dist/ h_date)^2))
}

distance_hour = function(current_time,predicted_time){
  dist = as.numeric(difftime(current_time,predicted_time,units = "hours"))
  return(exp(-(dist/ h_time)^2))
}

#Additive and multiplicative kernel
kernel = function(current, predicted, index){
  dist_pos = distance_pos(current[c("longitude", "latitude")],c(predicted$longitude, predicted$latitude))
  print(head(dist_pos,3))
  dist_date = distance_day(current$date,predicted$date)
  print(head(dist_date,3))
  dist_time = distance_hour(current$time,predicted$time)
  print(head(dist_time),3)
  #addition
  dist = dist_pos + dist_date + dist_time
  
  distance<-data.frame(dist,dist_pos,dist_date,dist_time)
  selection = data.frame(st,distance)
  selected<-sum(selection$dist * selection$air_temperature) / sum(selection$dist)
  
  #multiplication
  dist2<-   dist_pos * dist_date * dist_time  
  distance2<-data.frame(dist2,dist_pos,dist_date,dist_time)
  selection2 = data.frame(st,distance2)
  selected2<-sum(selection2$dist2 * selection2$air_temperature) / sum(selection2$dist2)
  
  return(list(selected,selected2))
}

temp <- vector(length=length(date))
temp2 <- vector(length=length(date))
for(i in 1:nrow(Predicted)){
  temp[i]  = kernel(st, Predicted[i,],i)[1]
  temp2[i]  = kernel(st, Predicted[i,],i)[2]
}

#Printing and plotting
print(unlist(temp))
plot(c(4,6,8,10,12,14,16,18,20,22,24),unlist(temp),type="o",xlab = "Time",ylab = "Temperature",main="Temperature prediction using Summation of kernels")

print(unlist(temp2))
plot(c(4,6,8,10,12,14,16,18,20,22,24),unlist(temp2), type="o",xlab = "Time",ylab = "Temperature",main="Temperature prediction using Multiplication of kernels")
