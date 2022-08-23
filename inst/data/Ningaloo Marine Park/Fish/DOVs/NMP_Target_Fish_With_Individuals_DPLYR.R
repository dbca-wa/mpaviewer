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

#Check most abundant species
species = raw.data %>% group_by(Taxa)%>%
  summarise(total = sum(Number))%>%
  arrange(desc(total))

#Rename some species to lump
raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus maculatus' = "Plectropomus spp")
raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus leopardus' = "Plectropomus spp")
raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus areolatus' = "Plectropomus spp")
raw.data$Taxa = recode(raw.data$Taxa,'Epinephelus fasciatus' = "Epinephelus rivulatus")

#Limit to essential columns
ess.data = raw.data %>% select(Year,Zone,Site,Period,Taxa,Number)%>%
mutate(Number=as.integer(Number))

#Sum abundance values for individual species within site and period

sum.ab = ess.data %>% group_by(Year, Zone, Site, Period, Taxa)%>%
              summarise(total = sum(Number))

#Add 0 values to dataset for transects where fish species weren't counted
wide.ab = sum.ab %>% spread(Taxa, total, fill = 0)

#Add a column that totals all the target species you want
wide.ab$Targeted = rowSums(wide.ab[,c("Epinephelus rivulatus","Lethrinus nebulosus","Plectropomus spp","Lutjanus carponotatus")] )

#Gather back together (check number of columns)
long.ab  = wide.ab %>% 
gather(key = Taxa, value = Number,5:472)

#Remove 2010/11 DOVs not swum correctly and 2013 as only a few sites were done
long.ab = long.ab %>% 
  filter(!Year %in% c("2010","2011", "2013"))

#Check sites that were not sampled consitantly across all years
table = table(long.ab$Site,long.ab$Year)
table
row_sub = apply(table, 1, function(row) all(row !=0 ))
finalsites = table[row_sub,]
finalsites

#Have also removed L2 due to abnormally large schools of nebs in 2014
#Note will show all sites in separate plot
sites.limited = long.ab %>% filter(Site %in% rownames(finalsites))%>% 
  filter(!Site %in% "L2")
ordered(unique(finaldat$Site))

#Fix this it is repeating too much
#Obtain mean and SE values for each zone across years
means = sites.limited %>% group_by(Year,Zone,Taxa)%>%
  summarise(N = length(Number),mean = mean(Number),sd = sd(Number),se = sd(Number)/ sqrt(length(Number)))
  
#Subset species you want in the plot and order them
target.sp = means %>% filter(Taxa %in% c("Targeted","Lethrinus nebulosus","Plectropomus spp","Epinephelus rivulatus","Lutjanus carponotatus"))%>%
  mutate(Taxa = factor(Taxa,levels = c("Targeted","Lethrinus nebulosus","Plectropomus spp","Epinephelus rivulatus","Lutjanus carponotatus")))
  
#Plot
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetFig <- ggplot(target.sp, aes(x=Year, y=mean, group=Zone, shape=Zone, colour = Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd, colour = "black") + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=4), se=F, size = 1) +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(aes(fill = Zone),position=pd, size=5,colour = "black") +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(target.sp$Year-0.25),max(target.sp$Year+0.25)), breaks=min(target.sp$Year):max(target.sp$Year)) +
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

TargetFig+facet_grid(Taxa~.,scales = "free_y")
