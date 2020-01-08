require(leaflet)  
require(dplyr)
require(geosphere) # for flight arc paths 
require(htmltools)


setview <- c(26.335100,17.228331) # libya
mp <- data.frame(
  "name" = c("Melbourne, Australia",
             "Auckland, New Zealand",
             "Maine, USA",
             "Cartagena, Colombia",
             "Atlanta, USA",
             "Salzburg, Austria"),
  "lat" = c(-37.813629,
            -36.848461,
            45.253784,
            10.422930,
            33.748997,
            47.800499
            ),
  "lon" = c(144.963058,
            174.763336,
            -69.445473,
            -75.537262,
            -84.387985,
            13.044410
            ),
  "year" = c(
    "2012-2015",
    "2013",
    "2014",
    "2017",
    "2018-2019",
    "2019"
    ),
  # "conf" = c(
  #   c("Ecological Society of Australia, ESA2012", "University of Melbourne", "UoM"),
  #   "Ecological Society of Australia and New Zealand, EcoTas13",
  #   "Unifying Ecology Across Scales, GRC 2014",
  #   "International Conference for Conservation Biology, ICCB2017",
  #   c("Center for Disease Control and Prevention (CDC)","Emory University", "Clayton State University"),
  #   "International Society for Ecological Modelling, ISEM2019"
  # ),
  "quantity" = c(
    5,
    1,
    1,
    1,
    5,
    1
  )
)
# write.csv(mp,"/Users/malishev/Documents/Data/reveal_test/reveal_test/data/mp.csv")


conf_names <- paste(sep = "<br/>",
                    "Year: ", mp$year,
                    # "Conference: ", mp$conf,
                    "",
                    "Presentations: ", mp$quantity
)

conf_names_style <- tags$style(
    HTML(".leaflet-control.conf_names_css { 
         transform: translate(-50%,20%);
         position: fixed !important;
         left: 50%;
         text-align: center;
         padding-left: 10px; 
         padding-right: 10px; 
         background: white; opacity: 0.7;
         font-weight: bold;
         font-size: 20px;
         }"
       ))

conf_names_style_popup <- tags$div(
  conf_names_style, HTML(conf_names)
)  

text_label_opt <- labelOptions(noHide = T, direction = "top", 
                               textOnly = T, opacity = 1, offset = c(0,0),
                               style = style, permanent = T
)



# fixed text --------------------------------------------------------------

fixed_text <- "Places where I've given presentations"
fixed_text2 <- "given presentations"
fixed_text_latlon <- c(26.441866,-40.578384)
style <- list(
  "color" = "white",
  "font-family" = "Optima",
  "font-style" = "bold",
  "font-size" = "20px",
  "border-color" = "rgba(0,0,0,0.5)",
  "font-weight" = "bold",
  "font-size" = "10px",
  "border-color" = "rgba(0,0,0,0.5)",
  "background" = "white; opacity: 0.7;",
  "z-index" = "-1"
)

text_label_opt <- labelOptions(noHide = T, direction = "top", 
                               textOnly = T, opacity = 1, offset = c(0,0),
                               style = style
)


# map ---------------------------------------------------------------------

latlon_matrix <- matrix(c(mp[,"lon"],mp[,"lat"]),ncol=2)
# custom_tile <- "http://d.sm.mapstack.stamen.com/((darkmatter,$00ffff[hsl-color]),(mapbox-water,$00589c[hsl-color]),(parks,$ff9a30[source-in]))/{z}/{x}/{y}.png"
custom_tile <- "http://c.sm.mapstack.stamen.com/(toner-lite,(mapbox-water,$000[@80]),(parks,$000[@70]),(buildings,$fabe68[hsl-color]))/{z}/{x}/{y}.png"

colv <- "#F90F40"
opac <- 1
site_names <- mp$name
map <- gcIntermediate(latlon_matrix[6,],
                      latlon_matrix,
                      n=100,
                      addStartEnd=TRUE,
                      breakAtDateLine = T,
                      sp=TRUE
) %>% 
  leaflet() %>% 
  setView(setview[2],setview[1],zoom=3) %>% 
  addTiles(custom_tile) %>% 
  addPolylines(
    opacity = opac,
    color = colv) %>% 
  addLabelOnlyMarkers(fixed_text_latlon[2], fixed_text_latlon[1], 
                      label = fixed_text, labelOptions = text_label_opt) %>% 
  addCircleMarkers(mp[,"lon"],
                   mp[,"lat"],
                   radius = 10,
                   stroke = TRUE,
                   weight = 2, 
                   opacity = opac,
                   color = colv,
                   fillColor = colv,
                   fillOpacity = 0.5,
                   popup=conf_names,
                   popupOptions =
                     popupOptions(conf_names_style_popup, # try 'style = style' here
                                  className = "conf_names_css", 
                                  closeButton = T,autoPan = T,maxWidth = 1000),
                   label=site_names,
                   labelOptions = 
                     labelOptions(textsize = "20px",opacity = 0.7)) 
map

