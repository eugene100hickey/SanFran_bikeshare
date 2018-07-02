setwd("~/Desktop/Academic/Research/Blogs/SF_bikeshare")

library(leaflet)
library(dplyr)
library(elevatr)
library(ggplot2)
library(ggmap)
library(lubridate)

df <- read.csv("201801_fordgobike_tripdata.csv", sep=",")

df <- df %>% 
  mutate(start_time = ymd_hms(start_time), 
         end_time = ymd_hms(end_time))

stations <- data.frame(lat=unique(df$start_station_latitude),
                       long=unique(df$start_station_longitude),
                       id=unique(df$start_station_id),
                       name=unique(df$start_station_name))

z_out <- df %>% 
  group_by(start_station_id) %>% 
  summarise(start_no=n())
names(z_out)[1] <- "id"
z_in <- df %>% 
  group_by(end_station_id) %>% 
  summarise(end_no=n())
names(z_in)[1] <- "id"

z <- inner_join(z_out, z_in)
stations <- inner_join(stations, z)

sub_stations <- stations %>% 
  filter(lat>37.75, long< -122.38)

journey_distances <- expand.grid(x=sub_stations$name, 
                                 y=sub_stations$name)
journey_distances$x <- paste(as.character(journey_distances$x), 
                             ", San Francisco", sep="")
journey_distances$y <- paste(as.character(journey_distances$y), 
                             ", San Francisco", sep="")


# (my_map <- stations %>% 
#     filter(lat>37.7, long< -122.4) %>% 
#     leaflet() %>% 
#     addTiles() %>% 
#     addMarkers(popup = stations$name))

# locat <- data.frame(x=stations[,2], y=stations[,1])
# ll_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# z <- get_elev_point(locations = locat, src = "epqs", units="meters", 
#                prj = ll_prj)
# stations <- cbind(stations, elevation=z$elevation)

stations <- read.csv("stations.csv", sep=",")
stations <- stations %>% dplyr::select(-X)

sub_stations <- stations %>% 
  filter(lat>37.75, long< -122.38)

connections <- df %>% 
  filter(start_station_name %in% sub_stations$name,
         end_station_name %in% sub_stations$name) %>% 
  group_by(start_station_name, end_station_name) %>% 
  summarise(count=n()) %>% 
  left_join(sub_stations, 
            by=c("start_station_name" = "name")) %>% 
  left_join(sub_stations, 
            by=c("end_station_name" = "name")) %>% 
  arrange(elevation.x, elevation.y)
connections$id.x <- as.factor(connections$id.x)
connections$id.x <- reorder(connections$id.x, 
                            connections$elevation.x)
connections$id.y <- as.factor(connections$id.y)
connections$id.y <- reorder(connections$id.y, 
                            connections$elevation.y)
departures <- connections %>% 
  group_by(start_station_name) %>% 
  summarise(departs=sum(count))
connections <- left_join(connections, departures)
arrivals <- connections %>% 
  group_by(end_station_name) %>% 
  summarise(arrives=sum(count))
connections <- left_join(connections, arrivals)
connections <- connections %>% 
  mutate(normal_count=count/departs*100)


connections %>% 
  filter(departs > 500, arrives > 500) %>% 
  ggplot(aes(x=id.x, y=id.y)) +
  geom_tile(aes(fill=log(normal_count))) +
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1), 
        axis.text.y = element_text(size = 6)) +
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61",
                                  "#fee08b","#e6f598","#abdda4","#ddf1da"),
                         na.value="grey90")


(my_map <- leaflet() %>% 
    addTiles() %>% 
    addCircles(lat=sub_stations$lat,
               lng=sub_stations$long,
               popup = sub_stations$name, 
               radius=sub_stations$elevation*3))


distances <- read.csv("new_distances.csv", sep=",")
distances <- distances %>% dplyr::select(-X)


# distances4b <- mapdist(from=journey_distances$x[301:400], 
#                       to=journey_distances$y[301:400], 
#                       mode="bicycling", output="simple")
# distances4b <- mapdist(from=missing$from[1:231], 
#                        to=missing$to[1:231], 
#                        mode="bicycling", output="simple")
# distances <- rbind(distances, distances4b)
# distances$from <- gsub(", San Francisco", "", distances$from)
# distances$to <- gsub(", San Francisco", "", distances$to)
# write.csv(distances, "new_distances.csv")

df1 <- df %>% 
  filter(start_station_name %in% sub_stations$name, 
         end_station_name %in% sub_stations$name)
df1 <- df1 %>% 
  left_join(., sub_stations, 
            by = c("start_station_name"="name")) %>% 
  select(c(names(df1), "elevation")) %>% 
  rename(start_elevation=elevation)
df1 <- df1 %>% 
  left_join(., sub_stations, 
            by = c("end_station_name"="name")) %>% 
  select(c(names(df1), "elevation")) %>% 
  rename(end_elevation=elevation)
df1 <- df1 %>% left_join(., distances, 
                         by = c("start_station_name"="from", 
                                "end_station_name"="to")) %>% 
  select(c(names(df1), "m", "seconds"))

df1 <- df1 %>% 
  mutate(actual_vel=m/as.numeric(end_time-start_time)/1000*60)

df1 <- df1 %>% mutate(rise = end_elevation - start_elevation)

df1 %>% 
  filter(member_gender %in% c("Female", "Male")) %>% 
  group_by(member_gender) %>% 
  ggplot(aes(actual_vel, fill=member_gender)) + 
  geom_histogram(bins=100, alpha=0.6, position = "identity") + 
  xlim(0, 40)

df1 %>% ggplot(aes(rise)) + geom_histogram(bins=100)


#test dispersal pattern of bike stations
#shows they're not spread out but clustered
#need to correct long for spherical coords
#need to lop off top of grid where there's nothing
x <- sub_stations$long
y <- sub_stations$lat

window <- owin(xrange=c(min(x), max(x)), 
               yrange=c(min(y), max(y)))
point_pattern <- ppp(x=x, y=y, window=window)
quadrat.test(point_pattern, alternative = "regular")

