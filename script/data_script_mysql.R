library("RMySQL")
library("DBI")
library("GPArotation")
library("ggplot2")
library("tidyverse")
library("marelac")
library("dplyr")
library("lubridate")

#Guide: http://www.ahschulz.de/2013/07/23/installing-rmysql-under-windows/

# Download the MySQL connector https://dev.mysql.com/downloads/connector/odbc/


con <- dbConnect(MySQL(),
                 user="LCRoysterproject", 
                 password="HLLV6Pske0vTzhIZfSya",
                 dbname="LCRoysterproject", 
                 host="ict-prod-hosting05.mysql.osg.ufl.edu", 
                 port= 3359)

# Listing all of the columns in the database
dbListTables(con)

wq <- dbReadTable(conn = con, name = 'lcroyster_buoyobservation')

#test<- dbReadTable(conn = con, name = 'LCRoysterproject.buoy_temp')

wq$Date<- ymd_hms(wq$observation_datetime, tz="EST") %>%
  round_date("hour")
#wq$date<- as.POSIXct(wq$date, tz="GMT")

#test$date<-as.POSIXct(test$observation_datetime, tz="EST",usetz=TRUE)

#standard=42.914
#wq$sal <- convert_RtoS(wq$conductivity_mS_cm/standard, 
                                #t= wq$temperature_c, p= 0)

#test$sal<-convert_RtoS(test$conductivity_mS_cm/standard, 
                       #t= test$temperature_c, p= 0)



#Removing observations that have been affected by outside elements, barnacles or oyster growths on the sensor

#Need to convert location_id into a numeric value so that it can be filtere with dplyr



wq %>% 
  filter(location_id ==7 & Date > "2018-08-06")



ggplot(data= wq, aes( x= Date, y= salinity_psu_calculated, color= "Salinity with Marelac")) +
  geom_point(color= "black") +
  ggtitle("Calculated Salinity for Sites 1-9") +
  facet_wrap (~location_id, ncol=2)






#Quick plots to check calculated salinity and temperature

ggplot(data= wq, aes( x= date, y= salinity_psu_calculated, color= "Salinity with Marelac")) +
  geom_point(color= "black") +
  ggtitle("Calculated Salinity for Sites 1-9") +
  facet_wrap (~location_id, scales= "free_y")


ggplot(data= wq, aes( x= date, y= temperature_c)) + 
  ggtitle("Temperature for Sites 1-9") +
  geom_point(color= "black") +
  facet_wrap (~location_id, scales= "free_y")


#Moving averages
#https://www.rdocumentation.org/packages/tidyquant/versions/0.5.5/topics/geom_ma
# n= Number of periods to average over. Must be between 1 and nrow(x), inclusive.


  ggplot(wq, aes(x = date, y = salinity_psu_calculated)) +
  geom_point(alpha=0.6) +                         
  geom_ma(ma_fun = SMA, n = 100, color= "red",linetype=1.1, size= 1.1)+
    coord_x_datetime(xlim = c(today() - weeks(12), today()))+
    facet_wrap(~location_id, ncol=2)
  
class(wq$date)


