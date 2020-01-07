setwd("/Users/malishev/Documents/Data/mybio")
require(crosstalk)
require(plotly)

# load mock data
df <- readr::read_csv("data/call_activity.csv")
xinter <- seq(min(df$Date),max(df$Date),length.out = length(df$Date))
df$Person <- rep(c("Andreas","Christina","Matt"),rep=3,length.out=length(df$Date))
names(df)[4] <- "Analyses"

# plot data
p <- ggplot() +
  geom_vline(mapping=NULL, xintercept=xinter,colour='grey80',size=0.03) +
  geom_point(data=df,aes(Date,Hour,color=Person,size=Calls)) +
  scale_y_continuous(limits=c(1,23)) +
  scale_x_datetime(date_breaks = "1 week", 
                   date_minor_breaks = "1 day",
                   date_labels = "%D") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Activity per hour of day",
       x = "Date (M/D/Y)",
       y = "Hour of day") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_line(color = "gray"), 
        panel.grid.minor = element_line(color = "light gray"), 
        axis.line = element_line(color = "gray"))
p <- p + theme_hc()
ggplotly(p)

# plotly crosstalk --------------------------------------------------------

calls_person <- highlight_key(df, ~Hour)
date_person <- highlight_key(df, ~Date)

# set margins 
m <- list(
  t = 100,
  r = 5,
  b = 5,
  l = 0,
  padding = 4
)


pp <- bscols(
  widths = c(9,NA), # max 12
  # device = c("xs", "sm", "md", "lg"),
  p1 <- plot_ly(date_person, # plot 1
          x = ~Date,
          y = ~Hour,
          color = ~Person,
          size = ~Analyses, 
          type = "scatter",
          hoverinfo = "text",
          text = ~paste0("Date: ", Date,
                         "\nName: ", Person,
                         "\nNo. of analyses: ", Analyses),
          height = 500
  ) %>%
    # plotly layout 
    layout(title = "Analysis per hour of day",
           xaxis = list(tickangle=45,showgrid=T),
           yaxis = list(range=c(0,23), showgrid=T),
           autosize = T,
           margin = m
    ), 
  list( # stack plot elements in dash  
    filter_select("id", "Select hour of day", calls_person, ~Hour, multiple = F),
    p2 <-  plot_ly(calls_person, # plot 2
            x = ~Person, 
            color = ~Person,
            type = "histogram",
            height = 400
    ) %>% # plotly layout 
      layout(title = "Data per person", 
             yaxis = list(showgrid = F),
             autosize = T,
             margin = list(rep(0,4))
      )
  ) 
) 
pp

# h <- pp %>% sizingPolicy(browser.padding = 0,
#                           viewer.padding = 0,
#                          padding = 0, browser.fill = T)
# h
fh <- "dv1.html"
htmltools::save_html(pp,paste0(getwd(),"/",fh))


ggmap::revgeocode(c(41.83174254, -75.87998774))




# airbnb -------------------------------------------------------------------

require(leaflet)  
require(readr)
require(viridis)
require(colorspace)
require(htmltools)
require(dplyr)

custom_tile <- "http://d.sm.mapstack.stamen.com/(positron,(parks,$3bcdd5[hsl-color]),(streets-and-labels,$e2d2f0[hsl-color]),(buildings,$e2d2f0[hsl-color]))/{z}/{x}/{y}.png"

# load data 
url <- "http://data.insideairbnb.com/greece/attica/athens/2019-11-19/data/listings.csv.gz"
airbnb <-  read_csv(url)
airbnb <- airbnb[1:5000,] # get smaller dataset

setview <- c(38.005760,23.664530)
lon <- airbnb[,"longitude"]$longitude
lat <- airbnb[,"latitude"]$latitude 


# clean data 
airbnb$review_scores_rating[is.na(airbnb$review_scores_rating)] <- 0
airbnb$review_scores_checkin[is.na(airbnb$review_scores_checkin)] <- 0
airbnb$review_scores_cleanliness[is.na(airbnb$review_scores_cleanliness)] <- 0
airbnb$review_scores_location[is.na(airbnb$review_scores_location)] <- 0

#title
ttl <- "Athens Airbnb data analysis"  
tag.map.title <- tags$style(
  HTML(".leaflet-control.map-title { 
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

title <- tags$div(
  tag.map.title, HTML(ttl)
)  

fixed_text <- "Click on a listing to see more info"
fixed_text_latlon <- c(38.00128,23.8)
style <- list(
  "color" = "black",
  "font-family" = "Optima",
  "font-style" = "bold",
  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
  "font-size" = "15px",
  "border-color" = "rgba(0,0,0,0.5)",
  "background" = "white; opacity: 0.7;"
)

text_label_opt <- labelOptions(noHide = T, direction = "top", 
                               textOnly = T, opacity = 1, offset = c(0,0),
                               style = style
)

# popup text
weblink <- airbnb$host_url # weblink
webname <- airbnb$host_name
href <- paste0("<b><a href=",weblink,">",webname,"</a></b>")
text_label <- paste(sep = "<br/>",
                    href,"",
                    "Neighbourhood: ",airbnb$neighbourhood_cleansed,
                    "",
                    "No. of reviews: ", airbnb$number_of_reviews,
                    "",
                    "Location: ", airbnb$review_scores_location,
                    "",
                    "Cleanliness: ", airbnb$review_scores_cleanliness,
                    "",
                    "\nCheck-in: ", airbnb$review_scores_checkin
)

site_names <- paste("Overall rating: ", airbnb$review_scores_rating
                    # "lat=", airbnb$latitude,
                    # "lon=", airbnb$longitude
                    # "Location: ", airbnb$review_scores_location
                    # "\n\nCheck-in: ", airbnb$review_scores_checkin, 
                    # "\nCleanliness: ", airbnb$review_scores_cleanliness,
                    # 
) 

# label options 
marker_label_opt <- labelOptions(textsize = "20px",opacity = 0.5,offset = c(0,0))

# cols
colv <- "orange"
colv <- RColorBrewer::brewer.pal(10,"Spectral")
colv_vec <- colv[airbnb$review_scores_cleanliness]

# user defined gradient
colv <- colorRampPalette(sequential_hcl(2000, "Red-Blue") %>% sort)
colv_vec <- colv(length(airbnb$review_scores_rating))[as.numeric(cut(airbnb$review_scores_rating, breaks = length(airbnb$review_scores_rating)))]  # define breaks in col gradient

# colv <- sequential_hcl(100, "Red_Blue")  # continuous
# colv_vec <- colv[airbnb$review_scores_rating]


# colv_vec <- magma(200)[airbnb$review_scores_cleanliness %>% unique]
# colv_vec <- colv[]


map <- leaflet() %>% 
  setView(setview[2],setview[1],zoom=12) %>% 
  addTiles() %>% 
  addCircleMarkers(lon,
                   lat,
                   radius = 5,
                   stroke = TRUE,
                   weight = 1, 
                   opacity = 1,
                   color = colv_vec,
                   fillColor = colv_vec,
                   fillOpacity = 0.5,
                   fill = colv_vec,
                   popup=text_label,
                   popupOptions =
                     popupOptions(closeButton = T,autoPan = T,maxWidth = 1000),
                   label=site_names) %>%
  # addLabelOnlyMarkers(fixed_text_latlon[2],fixed_text_latlon[1],
  #                     fixed_text,
  #                     labelOptions = labelOptions(textOnly = T,
  #                                                 permanent = T,
  #                                                 textsize = "100px")) %>% 
  addControl(title, position="topleft",className = "map-title") %>% 
  addProviderTiles(names(providers)[110]) %>% 
  addLabelOnlyMarkers(fixed_text_latlon[2], fixed_text_latlon[1], 
                      label = fixed_text, labelOptions = text_label_opt)
map
# names(providers)

