require(leaflet)  
require(dplyr)
require(geosphere) # for flight arc paths 
require(htmltools)

ttl <- ""

# title
tag.map.title <- tags$style(
  HTML(".leaflet-control.map-title { 
       transform: translate(-50%,20%);
       position: fixed !important;
       left: 50%;
       text-align: center;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 0.7;
       font-size: 50px;
       font-family: Optima;
       }"
       ))

title <- tags$div(
  tag.map.title, HTML(ttl)
)  


setview <- c(9.145000,40.489674)
mp <- data.frame(
  "name" = c("Melbourne","Amsterdam","Leipzig","Medellin","Atlanta"),
  "lat" = c(-37.813629,52.370216,51.339695,6.244203,33.748997),
  "lon" = c(144.963058,4.895168,12.373075,-75.581215,-84.387985)
)
# write.csv(mp,"/Users/malishev/Documents/Data/reveal_test/reveal_test/data/mp.csv")



latlon_matrix <- matrix(c(mp[,"lon"],mp[,"lat"]),ncol=2)
# custom_tile <- "http://d.sm.mapstack.stamen.com/((darkmatter,$00ffff[hsl-color]),(mapbox-water,$00589c[hsl-color]),(parks,$ff9a30[source-in]))/{z}/{x}/{y}.png"
custom_tile <- "http://c.sm.mapstack.stamen.com/(positron,(mapbox-water,$3BCDD5[hsl-color]),(parks,$291C9C[hsl-color]))/{z}/{x}/{y}.png"

colv <- "#291C9C"
opac <- 1
site_names <- mp$name
map <- leaflet() %>% 
  setView(0,0,zoom=2) %>% 
  addTiles(custom_tile) %>% 
  addCircleMarkers(mp[,"lon"],
                   mp[,"lat"],
                   radius = 10,
                   stroke = TRUE,
                   weight = 3, 
                   opacity = opac,
                   color = colv,
                   fillColor = colv,
                   # popup=paste0("<br/>",mp$name,"<br/>"),
                   # popupOptions = 
                   #   popupOptions(closeButton = T,autoPan = T),
                   label=paste(site_names),
                   labelOptions = 
                     labelOptions(textsize = "20px",opacity = 0.7)
  ) %>% 
  addControl(title, position = "topleft", className="map-title")
map
