# Analysis of Chicago Divvy Bike Data
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggmap) # map data 
library(sp) # to get distances from lats and longs 
library(plotly)
bike <- read_csv('~/Desktop/Divvy/divvybikedata.csv')
summary(bike) # Confirms no NA values, we have clean data. 

### year ###
unique(bike$year)
# Observe growth by year 
x <- bike %>% group_by(year) %>% summarize('Num_Rides' = n())
ggplot(x, aes(year, Num_Rides, fill = as.character(year))) + 
   geom_bar(stat = 'identity') + theme_fivethirtyeight() + 
   scale_fill_canva(palette = 'Cool blues') + 
   theme(legend.position = 'none', 
         axis.text.y =  element_text(angle = 45, margin = margin(0,0,0,5)), 
         axis.title = element_text(size = 16),
         plot.title = element_text(size = 20, hjust = 0.5)) + 
   scale_y_continuous(labels = scales::comma) + ylab('Number of Rides') + xlab('Year') +
   ggtitle('Yearly Growth of Divvy Rides')

### lat/long start/end ### 
# Trying to create a map to visualize rides 
# Attempting all the data 
m1 <- filter(bike, year == 2017 & from_station_id == 359) %>% select(1, 16:17)
m2 <- filter(bike, year == 2017 & from_station_id == 359) %>% select(1, 21:22)
colnames(m1) <- c('id','lat', 'lon')
colnames(m2) <- c('id','lat', 'lon')
mapdf <- bind_rows(m1, m2)
rm(m1, m2) # m1, m2 no longer necessary 
City <- "Chicago, Illinois"
basemap <- get_map(location = City, maptype = 'roadmap') # first is satellite, second is roadmap
ggmap(basemap) + geom_point(data = mapdf, aes(x=lon, y=lat), alpha = .09, colour = 'red') + 
   geom_path(aes(x=lon, y=lat, group=id), data=mapdf, alpha = .09, colour = 'red')

m1 <- filter(bike, year == 2017 & from_station_id == 2) %>% select(1, 16:17)
m2 <- filter(bike, year == 2017 & from_station_id == 2) %>% select(1, 21:22)
colnames(m1) <- c('id','lat', 'lon')
colnames(m2) <- c('id','lat', 'lon')
mapdf <- bind_rows(m1, m2)
rm(m1, m2) # m1, m2 no longer necessary 
City <- "Chicago, Illinois"
basemap <- get_map(location = City, maptype = 'roadmap') # first is satellite, second is roadmap
ggmap(basemap) + geom_point(data = mapdf, aes(x=lon, y=lat), alpha = .09, colour = 'red') + 
   geom_path(aes(x=lon, y=lat, group=id), data=mapdf, alpha = .09, colour = 'red')

### month ### 
x <- bike %>% group_by(month) %>% summarize('Num_Rides' = n())
ggplot(x, aes(month, Num_Rides, fill = as.character(month))) + 
   geom_bar(stat = 'identity') + theme_fivethirtyeight() +
   theme(legend.position = 'none', 
         axis.text.y =  element_text(angle = 45, margin = margin(0,0,0,5)), 
         axis.title = element_text(size = 16),
         plot.title = element_text(size = 20, hjust = 0.5)) + 
   scale_y_continuous(labels = scales::comma, 
                      breaks = c(250000, 500000, 750000, 1000000, 1250000)) +
   scale_x_continuous(breaks = 1:12) + 
   ylab('Number of Rides') + xlab('Month') +
   ggtitle('Monthly Distribution of Divvy Rides')

### hour ###
x <- bike %>% group_by(hour) %>% summarize('Num_Rides' = n())
ggplot(x, aes(hour, Num_Rides, fill = as.character(hour))) + 
   geom_bar(stat = 'identity') + theme_fivethirtyeight() +
   theme(legend.position = 'none', 
         axis.text.y =  element_text(angle = 45, margin = margin(0,0,0,5)), 
         axis.title = element_text(size = 16),
         plot.title = element_text(size = 20, hjust = 0.5)) + 
   scale_y_continuous(labels = scales::comma, 
                      breaks = c(250000, 500000, 750000, 1000000, 1250000)) +
   scale_x_continuous(breaks = 0:23) + 
   ylab('Number of Rides') + xlab('Hour') +
   ggtitle('Hourly Distribution of Divvy Rides')

### day ### 
x <- bike %>% group_by(day) %>% summarise('Num_Rides' = n())
ggplot(x, aes(day, Num_Rides, fill = as.character(day))) + 
   geom_bar(stat = 'identity') + theme_fivethirtyeight() +
   theme(legend.position = 'none', 
         axis.text.y =  element_text(angle = 45, margin = margin(0,0,0,5)), 
         axis.title = element_text(size = 16),
         plot.title = element_text(size = 20, hjust = 0.5)) + 
   scale_y_continuous(labels = scales::comma, 
                      breaks = c(250000, 500000, 750000, 1000000, 1250000)) +
   scale_x_continuous(breaks = 0:6) + 
   ylab('Number of Rides') + xlab('Day') +
   ggtitle('Daily Distribution of Divvy Rides')

### temperature ### 
d1 <- ggplot(bike) + geom_density(aes(temperature), fill = '#FF2700', color = '#FF2700') + 
   theme_fivethirtyeight() + 
   scale_color_fivethirtyeight() + theme(plot.title = element_text(hjust = .5, size = 12)) + 
   scale_x_continuous(breaks = seq(0,90,10)) + theme(axis.title = element_text()) + 
   ggtitle('Distribution of Temperature During Rides') + 
   xlab('Temperature') + ylab('Density')

d2 <- ggplot(bike, aes(temperature)) + stat_ecdf(colour = "#008FD5") + 
   ggtitle('ECDF of Temperature') + theme_fivethirtyeight() + 
   theme(plot.title = element_text(hjust = .5)) + xlab('Temperature') + ylab('ECDF') + 
   scale_x_continuous(breaks = seq(0,90,10))
grid.arrange(d1, d2, ncol = 2)


########################
### Spatial Analysis ###
########################

### STATIONS MAP ### 
City <- "Chicago, Illinois"
basemap <- get_map(location = City, maptype = 'roadmap')
# Going to create a map df 
# lat and long average lat and long of each start lat/long for that id 
# 586 unique ids, so there will be 586 rows 
map_df <- select(bike, from_station_name, latitude_start, longitude_start)
map_df <- bike %>% group_by(from_station_name) %>% summarise('lat' = mean(latitude_start), 
                                                           'long' = mean(longitude_start))
ggmap(basemap) + 
   geom_point(data = map_df, aes(x=long, y=lat), alpha = .6, colour = 'red') + 
   scale_x_continuous(limits = c(-87.82, -87.48)) + 
   scale_y_continuous(limits = lat <- c(41.7, 42.1))


### FROM AND TO STATIONS ###
x <- bike %>% group_by(from_station_name) %>% summarize('Num_Rides' = n()) %>% 
   top_n(50, Num_Rides)
# Reorder x to be descending
stations1 <- ggplot(x, aes(reorder(from_station_name, Num_Rides), Num_Rides, 
                           fill = from_station_name)) + 
   geom_bar(stat = 'identity') + coord_flip() + theme_fivethirtyeight() + 
   theme(legend.position = 'none', axis.title = element_text()) + 
   scale_fill_manual(values = heat.colors(50)) + 
   ggtitle('Top 50 Most Used Start Stations') + ylab('Number of Rides') + xlab('')

y <- bike %>% group_by(to_station_name) %>% summarize('Num_Rides' = n()) %>% 
   top_n(50, Num_Rides)
stations2 <-  ggplot(y, aes(reorder(to_station_name, Num_Rides), Num_Rides, 
                            fill = to_station_name)) + 
   geom_bar(stat = 'identity') + coord_flip() + theme_fivethirtyeight() + 
   theme(legend.position = 'none', axis.title = element_text()) + 
   scale_fill_manual(values = heat.colors(50)) + 
   ggtitle('Top 50 Most Used Start Stations') + ylab('Number of Rides') + xlab('')
grid.arrange(stations1, stations2, ncol = 1)

### RIDE DISTANCES ### 
# Experimental framework, deprecated 
# x <- head(mapdf[order(mapdf$id), ])
# x <- select(x, 1, 3, 2)
# spDistsN1(pts = as.matrix(x[1,-1]), pt = as.matrix(x[2,-1]), longlat = T)
# 
# distances <- c()
# i <- 1
# while(i <= nrow(x) - 1){
#    distances <- c(distances, spDistsN1(pts = as.matrix(x[i,-1]), 
#                                        pt = as.matrix(x[i+1,-1]), longlat = T))
#    i <- i+2
# }

# Actual implementation 
# function to calculate distance per row 
mydist <- function(row){
   start <- matrix(as.numeric(row[2:1]), ncol = 2)
   end <- matrix(as.numeric(row[4:3]), ncol = 2)
   distance <- spDistsN1(pts = start, pt = end, longlat = T)
   return(distance)
}
# select relevant columns 
distDf <- select(bike, 16:17, 21:22) 
x <- numeric(nrow(distDf)) # pre-allocate memory 
x <- apply(distDf, 1, mydist)
bike$ride_distance <- x

### DISTANCE TRAVELED ###
summary(bike$ride_distance)
# There appear to be rides of 0 km. Let's remove these values. 
condition <- bike$ride_distance!=0 # pre-allocate condition 
bike <- filter(bike, condition) 
ggplot(bike) + geom_density(aes(ride_distance), fill = "#008FD5", alpha = .7) + 
   theme_fivethirtyeight() + ggtitle('Density Plot of Ride Distances') + 
   scale_color_fivethirtyeight() + scale_fill_fivethirtyeight() + 
   theme(axis.title = element_text(), plot.title = element_text(hjust = .5, size = 24),
         axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
   xlab('Ride Distance (km)') + 
   ylab('Density') 


### DISTANCE TRAVELED BY STATIONS ###
x <- bike %>% group_by(from_station_name) %>% summarise('mean_dist' = mean(ride_distance))
head(x[order(x$mean_dist, decreasing = T), ])

# Check out Humphrey Ave & Ontario St, and Racine Ave & 65th St to see if they look different 
# than the average map 
m1 <- filter(bike, from_station_name == 'Humphrey Ave & Ontario St') %>% select(1, 16:17)
m2 <- filter(bike, from_station_name == 'Humphrey Ave & Ontario St') %>% select(1, 21:22)
colnames(m1) <- c('id','lat', 'lon')
colnames(m2) <- c('id','lat', 'lon')
mapdf <- bind_rows(m1, m2)
rm(m1, m2) # m1, m2 no longer necessary 
ggmap(basemap) + geom_point(data = mapdf, aes(x=lon, y=lat), alpha = .5, colour = 'red') + 
   geom_path(aes(x=lon, y=lat, group=id), data=mapdf, alpha = .5, colour = 'red') + 
   scale_x_continuous(limits = c(-87.82, -87.48)) + 
   scale_y_continuous(limits = lat <- c(41.8, 42.0)) + xlab('Longitude') + ylab('Latitude') + 
   ggtitle('Map of Humphrey/Ontario') + theme(plot.title = element_text(hjust = .5))

# Racine Ave & 65th St
m1 <- filter(bike, from_station_name == 'Racine Ave & 65th St') %>% select(1, 16:17)
m2 <- filter(bike, from_station_name == 'Racine Ave & 65th St') %>% select(1, 21:22)
colnames(m1) <- c('id','lat', 'lon')
colnames(m2) <- c('id','lat', 'lon')
mapdf <- bind_rows(m1, m2)
rm(m1, m2) # m1, m2 no longer necessary 
ggmap(basemap) + geom_point(data = mapdf, aes(x=lon, y=lat), alpha = .5, colour = 'red') + 
   geom_path(aes(x=lon, y=lat, group=id), data=mapdf, alpha = .5, colour = 'red') + 
   scale_x_continuous(limits = c(-87.73, -87.48)) + 
   scale_y_continuous(limits = lat <- c(41.7, 41.94)) + xlab('Longitude') + ylab('Latitude') + 
   ggtitle('Map of Divvy Stations') + theme(plot.title = element_text(hjust = .5))

# Clinton St & Washington Blvd
m1 <- filter(bike, from_station_name == 'Columbus Dr & Randolph St') %>% select(1, 16:17)
m2 <- filter(bike, from_station_name == 'Columbus Dr & Randolph St') %>% select(1, 21:22)
colnames(m1) <- c('id','lat', 'lon')
colnames(m2) <- c('id','lat', 'lon')
mapdf <- bind_rows(m1, m2)
rm(m1, m2) # m1, m2 no longer necessary 
ggmap(basemap) + geom_point(data = mapdf, aes(x=lon, y=lat), alpha = .1, colour = 'red') + 
   geom_path(aes(x=lon, y=lat, group=id), data=mapdf, alpha = .1, colour = 'red') + 
   scale_x_continuous(limits = c(-87.8, -87.4)) + 
   scale_y_continuous(limits = lat <- c(41.7, 42.05)) + xlab('Longitude') + ylab('Latitude') + 
   ggtitle('Map of Divvy Stations') + theme(plot.title = element_text(hjust = .5))


### DIST OF TRIPS PER STATION AND PER TIME ###
x <- select(bike, from_station_name, latitude_start, longitude_start, ride_distance)
x <- bike %>% group_by(from_station_name) %>% summarise('lat' = mean(latitude_start), 
                                                        'long' = mean(longitude_start),
                                                        'dist' = mean(ride_distance),
                                                        'dist2' = mean(ride_distance))
# Rearrange order of data to enable higher distances showing up in front 
x <- x %>% arrange(dist) %>% 
   mutate(from_station_name = factor(from_station_name, unique(from_station_name))) 
# Plot 
dist_map <- ggplot(x) + geom_point(aes(x = long, y = lat, size = dist2, color = dist)) + 
   theme_fivethirtyeight() + 
   ggtitle('Map of Average Distance Per Station') + 
   theme(legend.position = 'right', legend.direction = 'vertical', 
         plot.title = element_text(hjust = .5, size = 22, face = 'bold'), 
         axis.text = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank()) + 
   scale_color_gradient(low = "#4C00FFFF", high = "#FFE0B3FF", name = 'Ave. Dist') + 
   scale_size_continuous(guide = FALSE)
ggplotly(dist_map, tooltip = c('~dist'))
 

### NUMBER OF RIDES FROM START AND STOP IN EVENING AND MORNING ### 
# This is a map of where folks are leaving from in the morning 
x <- select(bike, from_station_name, latitude_start, longitude_start, hour) %>% 
   filter(hour <= 10 & hour >= 5) %>% group_by(from_station_name) %>% 
   summarise('lat' = mean(latitude_start), 'long' = mean(longitude_start), 
             'Num_Rides' = n()) %>% arrange(Num_Rides) %>% 
   mutate(from_station_name = factor(from_station_name, unique(from_station_name)))

morning1 <- ggplot(x) + geom_point(aes(x = long, y = lat, size = Num_Rides, 
                                       color = Num_Rides), alpha = .5) + 
   theme_hc(bgcolor = 'darkunica') + 
   ggtitle('Stations Morning Riders Leave From') + 
   theme(legend.position = 'right', legend.direction = 'vertical', 
         plot.title = element_text(hjust = .5, size = 18, face = 'bold'), 
         axis.text = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         panel.grid.major.y = element_blank()) + 
   scale_color_gradient(low = "#4C00FFFF", high = "#FFE0B3FF", name = 'Number of Riders') + 
   scale_size_continuous(guide = FALSE)

# This is a map where folks are arriving to in the morning 
x <- select(bike, to_station_name, latitude_end, longitude_end, hour) %>% 
   filter(hour <= 10 & hour >= 5) %>% group_by(to_station_name) %>% 
   summarise('lat' = mean(latitude_end), 'long' = mean(longitude_end), 
             'Num_Rides' = n()) %>% arrange(Num_Rides) %>% 
   mutate(to_station_name = factor(to_station_name, unique(to_station_name)))

morning2 <- ggplot(x) + geom_point(aes(x = long, y = lat, size = Num_Rides, 
                                       color = Num_Rides), alpha = .5) + 
   theme_hc(bgcolor = 'darkunica') + 
   ggtitle('Stations Morning Riders Arrive In') + 
   theme(legend.position = 'right', legend.direction = 'vertical', 
         plot.title = element_text(hjust = .5, size = 18, face = 'bold'), 
         axis.text = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         panel.grid.major.y = element_blank())+ 
   scale_color_gradient(low = "#4C00FFFF", high = "#FFE0B3FF", name = 'Number of Riders') + 
   scale_size_continuous(guide = FALSE)
#grid.arrange(morning1, morning2, ncol = 1)

# This is a map of where folks are leaving from in the evening 
x <- select(bike, from_station_name, latitude_start, longitude_start, hour) %>% 
   filter(hour <= 20 & hour >= 16) %>% group_by(from_station_name) %>% 
   summarise('lat' = mean(latitude_start), 'long' = mean(longitude_start), 
             'Num_Rides' = n()) %>% arrange(Num_Rides) %>% 
   mutate(from_station_name = factor(from_station_name, unique(from_station_name)))

evening1 <- ggplot(x) + geom_point(aes(x = long, y = lat, size = Num_Rides, 
                                       color = Num_Rides), alpha = .5) + 
   theme_hc(bgcolor = 'darkunica') + 
   ggtitle('Stations Evening Riders Leave From') + 
   theme(legend.position = 'right', legend.direction = 'vertical', 
         plot.title = element_text(hjust = .5, size = 18, face = 'bold'), 
         axis.text = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         panel.grid.major.y = element_blank()) + 
   scale_color_gradient(low = "#4C00FFFF", high = "#FFE0B3FF", name = 'Number of Riders') + 
   scale_size_continuous(guide = FALSE)

# This is a map of where evening riders arrive in 
x <- select(bike, to_station_name, latitude_end, longitude_end, hour) %>% 
   filter(hour <= 20 & hour >= 16 ) %>% group_by(to_station_name) %>% 
   summarise('lat' = mean(latitude_end), 'long' = mean(longitude_end), 
             'Num_Rides' = n()) %>% arrange(Num_Rides) %>% 
   mutate(to_station_name = factor(to_station_name, unique(to_station_name)))

evening2 <- ggplot(x) + geom_point(aes(x = long, y = lat, size = Num_Rides, 
                                       color = Num_Rides), alpha = .5) + 
   theme_hc(bgcolor = 'darkunica') + 
   ggtitle('Stations Evening Riders Arrive In') + 
   theme(legend.position = 'right', legend.direction = 'vertical', 
         plot.title = element_text(hjust = .5, size = 18, face = 'bold'), 
         axis.text = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         panel.grid.major.y = element_blank()) + 
   scale_color_gradient(low = "#4C00FFFF", high = "#FFE0B3FF", name = 'Number of Riders') + 
   scale_size_continuous(guide = FALSE)
#grid.arrange(evening1, evening2, ncol = 1)

grid.arrange(morning1, morning2, evening1, evening2, ncol = 2)

### TODO: TRY A FACET_WRAP WITH MAP AND TIMING 

### NIGHTLIFE HOURS ### 
x <- select(bike, from_station_name, latitude_start, longitude_start, hour, day) %>% 
   filter(day %in% 5:7 & (hour > 22 | hour <=5))
# Remove Friday before 10pm 
x <- x[!(x$day == 5 & x$hour <= 5), ]
x <- x %>% group_by(hour) %>% summarise('Num_Rides' = n())
x$hour <- factor(x$hour, levels = c(23:24, 1:5))
ggplot(x, aes(hour, Num_Rides, fill = as.character(hour))) + geom_bar(stat = 'identity') + theme_fivethirtyeight() +
   scale_fill_manual(values = heat.colors(7)) + 
   theme(legend.position = 'none', 
         axis.text.y =  element_text(angle = 45, margin = margin(0,0,0,5)), 
         axis.title = element_text(size = 16),
         plot.title = element_text(size = 20, hjust = 0.5)) + 
   scale_y_continuous(labels = scales::comma, 
                      breaks = c(20000, 40000, 60000, 80000)) +
   scale_x_discrete(labels = c(paste(10:11, 'pm', sep = ''), paste(1:5, 'am', sep = ''))) + 
   ylab('Number of Rides') + xlab('Hour') +
   ggtitle('Hourly Distribution of Nightlife Divvy Rides')


### AVE DISTANCE BY MORNING AND NIGHT 
x <- select(bike, from_station_name, latitude_start, longitude_start, 
            hour, day, ride_distance) %>% 
   filter(hour <= 10 & hour >= 5 & day %in% 1:5) %>% group_by(from_station_name) %>% 
   summarise('lat' = mean(latitude_start), 'long' = mean(longitude_start), 
             'Med_Dist' = median(ride_distance)) %>% arrange(Med_Dist) %>% 
   mutate(from_station_name = factor(from_station_name, unique(from_station_name)))

morning3 <- ggplot(x) + geom_point(aes(x = long, y = lat, size = Med_Dist, 
                                       color = Med_Dist), alpha = .5) + 
   theme_hc(bgcolor = 'darkunica') + 
   ggtitle('Stations Morning Riders Leave From') + 
   theme(legend.position = 'right', legend.direction = 'vertical', 
         plot.title = element_text(hjust = .5, size = 18, face = 'bold'), 
         axis.text = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         panel.grid.major.y = element_blank()) + 
   scale_color_gradient(low = "#4C00FFFF", high = "#FFE0B3FF", name = 'Median Distance') + 
   scale_size_continuous(guide = FALSE)


 
x <- select(bike, from_station_name, latitude_start, longitude_start, 
            hour, day, ride_distance) %>% 
   filter(hour <= 20 & hour >= 16 & day %in% 1:5) %>% group_by(from_station_name) %>% 
   summarise('lat' = mean(latitude_start), 'long' = mean(longitude_start), 
             'Med_Dist' = median(ride_distance)) %>% arrange(Med_Dist) %>% 
   mutate(from_station_name = factor(from_station_name, unique(from_station_name)))

evening3 <- ggplot(x) + geom_point(aes(x = long, y = lat, size = Med_Dist, 
                                       color = Med_Dist), alpha = .5) + 
   theme_hc(bgcolor = 'darkunica') + 
   ggtitle('Stations Evening Riders Leave From') + 
   theme(legend.position = 'right', legend.direction = 'vertical', 
         plot.title = element_text(hjust = .5, size = 18, face = 'bold'), 
         axis.text = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         panel.grid.major.y = element_blank()) + 
   scale_color_gradient(low = "#4C00FFFF", high = "#FFE0B3FF", name = 'Median Distance') + 
   scale_size_continuous(guide = FALSE)
grid.arrange(morning3, evening3, ncol = 2)

### AVE DISTANCE NIGHTLIFE ###

