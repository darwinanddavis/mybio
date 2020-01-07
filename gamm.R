setwd("/Users/malishev/Documents/Emory/research/mesocosm/GAMM")
### Variables and factor-smooth interactions are set up according to
### advice from http://www.sfs.uni-tuebingen.de/~jvanrij/Tutorial/GAMM.html

### TO DO
# - Figure out how to best present the fact that gradual model curves (snail density) and more peaked model curves (cercariae and infected density) against time are not accurately showing the time lag difference in what's happening in the lab.  
# - Fit gamms for just first few weeks
# - conceptual model showing predictions of lag effect of infection based on body size


# 1. figure out how ordering of factors works    

### gamm reference curve
# reference curve should have the best data so that other curves are best informed,
## however, the curve with the best fit will be most rigid and thus constrain the flexibility of the other curves  

# packages
packages <- c("nlme", "mgcv", "lattice", "itsadug", "MASS", "viridis","MuMIn","RColorBrewer","ggplot2","ggthemes","sp","RCurl","dplyr")
if(require(packages)){
  install.packages(packages,dependencies = T)
}
lapply(packages,library,character.only=T)

# load plotting function
script <- getURL("https://raw.githubusercontent.com/darwinanddavis/plot_it/master/plot_it.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

# Set global plotting parameters
cat("plot_it( \n0 for presentation, 1 for manuscript, \nset colour for background, \nset colour palette. use 'display.brewer.all()', \nset alpha for colour transperancy, \nset font style \n)")
plot_it(0,"blue","Spectral","Greens",1,"serif") # set col function params
plot_it_gg("blue","white") # same as above

#set plot cols
col1 <- colv[1:3]
col2 <- colv[9:11]
colv <<- c(col1,col2) # USES <<- OPERATOR
colv <- rev(colv) # reverse color vector for all plots

colv2 <- colv2[c(4,6,8)] # choose colour palette
colv3 <- "grey"

#### end plotting params ####  
tanks = read.csv("Schisto_tanks_2016_for_GAMMs.csv")
snails = read.csv("Schisto_tanks_2016_Individual_Infections.csv")
head(tanks)
head(snails)

# This matches the Periphyton productivity data to the correct individual in the shedding dataset so that it can be used as a predictor
Peri_for_individual_shedding = numeric()
for(i in 1:length(snails[,1])){
  Peri_for_individual_shedding[i] = subset(tanks, Tank==snails[i,"Tank"] & Week == snails[i,"Week"], select="Peri_F")
}
Peri_F_snails = unlist(Peri_for_individual_shedding)
snails[,"Peri_F"] = Peri_F_snails

# Exclude tanks that never received schisto eggs
tanks = subset(tanks, Schisto == "Yes" & Week != 0)
tanks[,"High"] = as.numeric(tanks[,"N...P"] == "High")
tanks[,"Snail_density"] = 3*tanks[,"Snail"]/tanks[,"Sampling_Effort"]

# ln transformed stuff
tanks[,"ln_density"] = log(tanks$Snail_density+1)
tanks[,"ln_Peri"] = log(tanks[,"Peri_F"])
tanks[,"ln_BD"] = log(tanks$Biomass_density+0.1)
tanks[,"ln_Inf"] = log(tanks$Infected_abundance+1)
# Compiling means and sds, eventually sds...
mn_dens = aggregate(ln_density ~ Week*Size*N...P, FUN = mean, data=tanks)
mn_dens
mn_peri = aggregate(ln_Peri ~ Week*Size*N...P, FUN = mean, data=tanks)
mn_peri
mn_BD = aggregate(ln_BD ~ Week*Size*N...P, FUN = mean, data=tanks)
mn_BD
mn_Inf = aggregate(ln_Inf ~ Week*Size*N...P, FUN = mean, data=tanks)
mn_Inf

#### GAMM analysis of schisto tank experiment ####
#### Analyses conducted following advice from mgcv package documentation, Zurr 2012 "A beginners guide to generalized additive models with R", Simon Woods' pages
####    some additional web pages, and consultation with Lance Waller
## Error distribution decisions ##
# 1: Biomass densities, periphyton productivity - lognormal
# 2: Snail, cercarial counts - quasipoisson

# a) Does nutrient enrichment increase snail abundance?
# This treats size as an ordered factor and nutrient enrichment as a binary predictor
m_abundance = gamm(round(Snail_density) ~ Size + s(Week, by=Size) + s(Week, by=High), family=quasipoisson, correlation=corCAR1(form=~Week|Tank), data=tanks, method="REML")
m_abundance1 = gamm(round(Snail_density) ~ Size + s(Week, by=Size) , family=quasipoisson, correlation=corCAR1(form=~Week|Tank), data=tanks, method="REML")
m_abundance2 = gamm(round(Snail_density) ~ Size+ s(Week) + s(Week, by=High), family=quasipoisson, correlation=corCAR1(form=~Week|Tank), data=tanks, method="REML")
m_abundance3 = gamm(round(Snail_density) ~ Size + s(Week), family=quasipoisson, correlation=corCAR1(form=~Week|Tank), data=tanks, method="REML")
compareML(m_abundance$gam, m_abundance1$gam)
AIC(m_abundance$lme, m_abundance1$lme, m_abundance2$lme, m_abundance3$lme) # Model comparison via AIC is unreliable

gam_abund = gam(round(Snail_density) ~ Size + s(Week, by=Size) + s(Week, by=High), family=quasipoisson, data=tanks, method="REML")
summary(m_abundance$gam) # Extract significance of terms from here

# set plotting window
par(mfrow=c(1,3),mar=c(4,5,3,3))
xlim <- c(0,14)
ylim <- c(0,300)
n.grid <- 200 # smooth factor (0 = coarse; 200 = silky)
se <- 1.96 # SE (1.96 = 95% CI; 1 = 1 SE)

ppoints <- 0  # plot points?
plot_smooth(m_abundance$gam, view="Week", plot_all=c("Size","High"), rug=F, transform=exp, main="", se=se,
            # col=as.numeric(mn_dens$Week),
            col=colv,ylim=ylim,n.grid = n.grid, #lty=c(1,2,3,4,5,6),
            ylab="Host density (individuals/sample)", xlab="", xlim=c(0,14),hide.label=T)
if(ppoints==1){points(exp(mn_dens$ln_density) - 0.1 ~ mn_dens$Week,
                      pch=mn_dens$ln_density,col=mn_dens$ln_density)}

plot_smooth(m_abundance$gam, view="Week", plot_all=c("Size"), rug=F, transform=exp, main="", se=se,
            col=colv2,ylim=ylim,n.grid = n.grid, #lty=c(1,2,3),
            ylab="", xlab="Time (weeks)", xlim=c(0,14),hide.label=T)
if(ppoints==1){points(exp(mn_dens$ln_density) - 0.1 ~ mn_dens$Week,
                      pch=mn_dens$ln_density,col=mn_dens$ln_density)}

plot_smooth(m_abundance$gam, view="Week", plot_all=c("High"), rug=F, transform=exp, main="", se=se,
            col=colv3,ylim=ylim,n.grid = n.grid, lty=c(1,2),
            ylab="", xlab="", xlim=c(0,14),hide.label=T)
if(ppoints==1){points(exp(mn_dens$ln_density) - 0.1 ~ mn_dens$Week,
                      pch=mn_dens$ln_density,col=mn_dens$ln_density)}
# Nutrient enrichment creates larger peaks in snail density
# Larger initial snail populations lead to higher, slightly earlier peaks

# b) Does nutrient enrichment increase periphyton productivity?
m_peri = gamm(ln_Peri ~ Size +  s(Week, by=Size) + s(Week, by=High), family=gaussian, correlation=corCAR1(form=~Week|Tank), data=tanks)
summary(m_peri$gam)

par(mfrow=c(1,3))
ylim <- c(0,20000)
n.grid <- 200 # smooth factor (0 = coarse; 200 = silky)
se <- 1.96 # SE (1.96 = 95% CI; 1 = 1 SE)

ppoints <- 0 # plot points 
plot_smooth(m_peri$gam, view="Week", plot_all=c("Size", "High"), rug=F, transform=exp, main="", se=se,
            # col=as.numeric(mn_peri$Week),
            col=colv,ylim=ylim,n.grid = n.grid,
            ylab=expression(paste("Periphyton productivity, fluor m" ^ "2", " time, "^ "-1" )), xlab="", xlim=c(0,14),hide.label=T)
if(ppoints==1){points(exp(mn_peri$ln_Peri) ~ mn_peri$Week, 
                      pch=mn_peri$ln_Peri,col=mn_peri$ln_Peri)}

# Periphyton productivity size class
plot_smooth(m_peri$gam, view="Week", plot_all=c("Size"), rug=F, transform=exp, main="", se=se,
            # col=as.numeric(mn_peri$Week),
            col=colv2,ylim=ylim,n.grid=n.grid,
            ylab="",xlab="Time (weeks)", xlim=c(0,14),hide.label=T)
if(ppoints==1){points(exp(mn_peri$ln_Peri) ~ mn_peri$Week,
                      pch=mn_peri$ln_Peri,col=mn_peri$ln_Peri)}
title(expression(paste("Periphyton productivity \n fluor m"^"2", " time "^ "-1")))

# Periphyton productivity nutrient level
plot_smooth(m_peri$gam, view="Week", plot_all=c("High"), rug=F, transform=exp, main="", se=se,
            # col=as.numeric(mn_peri$Week),
            col=colv3,ylim=ylim,n.grid = n.grid, lty=c(1,2),
            ylab="",xlab="", xlim=c(0,14),hide.label=T)
if(ppoints==1){points(exp(mn_peri$ln_Peri) ~ mn_peri$Week,
                      pch=mn_peri$ln_Peri,col=mn_peri$ln_Peri)}
# Nutrient enrichment increases periphyton productivity
# Initial size has no effect

# b) Does nutrient enrichment increase snail dry mass?
m_DW = gamm(ln_BD ~ Size + s(Week, by=Size) + s(Week, by=High), family=gaussian, correlation=corCAR1(form=~Week|Tank), data=tanks)
summary(m_DW$gam)