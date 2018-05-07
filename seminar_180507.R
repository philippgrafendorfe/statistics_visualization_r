library(ggmap)
library(dplyr)

df <- crime
test <- get_map()
test <- ggmap(test)
ggmapplot(test)

geocode("houston texas")

m <- ggmap(get_map('Houston'))
plot(m)

m + geom_point(data = df, mapping=aes(x=lon, y=lat, color=offense), pch=1)

salzburg <- get_map('Salzburg')
salzburg <- ggmap(salzburg)
plot(salzburg)

mapdist("Hauptbahnhof, Salzburg", "Bahnhof Aigen, Salzburg", mode='driving')

route_df <- route("Kapitelgasse 4, 5020 Salzburg", "Hellbrunnerstrasse 34, 5020 Salzburg"
                 , mode='walking'
                 , structure='route'
                 , alternatives=F)

route_df <- route("Prater, Wien", "Stephansdom, Wien", mode='bicycling', structure='route', alternatives = F)

p <- ggmap(get_map('Vienna', zoom = 13))

p + 
  geom_point(data=route_df, mapping=aes(x=lon, y=lat)) +
  geom_line(data=route_df, mapping=aes(x=lon, y=lat), size=1.4, color='red')


library(leaflet)




