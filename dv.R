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



