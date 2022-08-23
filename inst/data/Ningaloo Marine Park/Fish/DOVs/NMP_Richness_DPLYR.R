library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Monitoring/Ningaloo/Data")
dir()

#Load data, fill in blank genus with family and create taxa column
raw.data <- read.csv("Ningaloo_DOVs_2010-2019_Checked.csv",na.strings=c("","NA"))%>% 
  filter(!is.na(period))%>%
  mutate(genus=as.character(genus))%>%
  mutate(family=as.character(family))%>%
  mutate(genus = ifelse(is.na(genus),family,genus))%>% 
  mutate(taxa = paste(genus, species, sep = ' '))

#Check column names
colnames (raw.data)
firstletter <- toupper(substring(names(raw.data),1,1))
others <- tolower(substring(names(raw.data), 2))
newnames<- paste(firstletter, others, sep="")
names(raw.data)<- newnames

#Limit to essential columns
ess.data = raw.data %>% select(Year,Zone,Site,Period,Taxa,Number)%>%
  mutate(Number=as.integer(Number))

#Sum abundance values for individual species within site and period

sum.ab = ess.data %>% group_by(Year, Zone, Site, Period, Taxa)%>%
  summarise(total = sum(Number))

#Calc richness and remove patchy years in terms of sites
richness = sum.ab %>% group_by(Year, Zone, Site, Period)%>%
  summarise(richness = length(unique(Taxa)))%>% 
  filter(!Year %in% c("2010","2011", "2013"))

#Check sites that were not sampled consitantly across all years
table = table(richness$Site,richness$Year)
table
row_sub = apply(table, 1, function(row) all(row !=0 ))
finalsites = table[row_sub,]
finalsites
sites.limited = richness %>% filter(Site %in% rownames(finalsites))
ordered(unique(sites.limited$Site))

#Obtain mean and SE values for each zone across years
rich.means = sites.limited %>% group_by(Year,Zone)%>%
  summarise(N = length(richness),mean = mean(richness),sd = sd(richness),se = sd(richness)/ sqrt(length(richness)))

#Plot
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetFig <- ggplot(rich.means, aes(x=Year, y=mean, group=Zone, shape=Zone, colour = Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd, colour = "black") + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=4), se=F, size = 1) +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(aes(fill = Zone),position=pd, size=5,colour = "black") +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Species per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(rich.means$Year-0.25),max(rich.means$Year+0.25)), breaks=min(rich.means$Year):max(rich.means$Year)) +
  scale_shape_manual(values=c(21, 22, 24))+
  scale_fill_manual(values=c('#F7F323',"#99CC33"))+
  scale_colour_manual(values=c('#F7F323',"#99CC33"))+
  theme_bw() +
  theme(strip.background = element_blank(),
        #axis.title=element_text(face="bold"), 
        #strip.text.x=element_text(size = 12,face="bold"),
        axis.text.x=element_text(angle=45, vjust=0.4),
        strip.text.y=element_text(size = 11,face="italic"),
        axis.title.x=element_text(size = 12,face="bold"),
        axis.title.y=element_text(size = 12,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),  
        legend.position = "bottom",
        axis.line = element_line(colour = 'black'),
        axis.text=element_text(size=12,colour = 'black'), 
        panel.border = element_rect(colour = "black"))

TargetFig
