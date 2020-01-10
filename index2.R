---
  title: "Matt Malishev"
author: "<div style=\"color:#2a1a9d;\"> <center> **A biosketch** </center> </div>"
date: 
  img: "img/mm.png"
params:
  width: 1000 
height: 700
# autosize: true
header-includes: \usepackage{float}
always_allow_html: yes
output:
  revealjs::revealjs_presentation:
  css: style/styles.css
theme: simple #default moon night black sky
highlight: pygments
incremental: true
center: true
transition: slide
background-transition: slide
mathjax: local # null
self_contained: false
slide_level: 1
# reveal_plugins: ["notes", "search", "zoom", "chalkboard"] # ALT + mouseclick to zoom
reveal_options:
  slideNumber: true
fig_caption: true
# fig_width: 10
# fig_height: 10
# showNotes: true
previewLinks: true
minScale: 1.0
maxScale: 1.0
---
  
  ```{r, set-options, echo = FALSE, cache = FALSE}
knitr::opts_chunk$set(
  eval = T, # run all code
  echo = F, # show code chunks in output 
  tidy=T, # make output as tidy
  message = F,  # mask all messages
  warning = F, # mask all warnings 
  out.width="100%",
  size="small", # set code chunk size, 
  knitr::opts_chunk$set(fig.pos = 'H')
)
# options(width = 2000)
# get device output size
# dev.size("px")
```

<!-- install packages -->
  ```{r, load packages, include=T, cache=F, message=F, warning=F, results='hide',echo=F}
packages <- c("ggplot2","dplyr","tidyverse","zoo","RColorBrewer","viridis","plyr","leaflet","plotly","widgetframe","geosphere","lubridate","ggthemes","gridExtra","crosstalk")  
if (require(packages)) {
  install.packages(packages,dependencies = T)
  require(packages)
}
suppressPackageStartupMessages(
  lapply(packages,library,character.only=T) 
)

# update packages 
# update.packages(ask = FALSE, checkBuilt = TRUE)

require(RCurl)
require(RColorBrewer)
require(dplyr)
require(plotly)
script <- getURL("https://raw.githubusercontent.com/darwinanddavis/plot_it/master/plot_it.R", ssl.verifypeer = FALSE)
eval(parse(text = script))
# Set global plotting parameters
cat("plot_it( \n0 for presentation, 1 for manuscript, \nset colour for background, \nset colour palette 1. use 'display.brewer.all()', \nset colour palette 2. use 'display.brewer.all()', \nset alpha for colour transperancy, \nset font style \n)")
plot_it(0,"blue","Set3","Spectral",0.8,"") # set plot function params       
plot_it_gg("black","white") # same as above for ggplot 

```

<!-- ____________________________________________________________________________ -->
  <!-- start body -->
  
  <!-- <a href="https://darwinanddavis.github.io/conference_sites/" title="Should work">Work</a> -->
  
  
  <!-- ________________________________________________________ slide break -->
  
  <p class="small">
  Best viewed in Chrome  
Press `F` for fullscreen   
Interactive slides  
</p>
  
  <!-- ________________________________________________________ slide break -->
  
  ---    
  
  <img src="img/mm.png",width=350, height=350, align="right">
  
  ### Born 
  Melbourne, Australia  

### Education
PhD, Computational Biology, University of Melbourne    
BSc (Hons), Monash University  

### Interests  
Cartography, data viz, writing  