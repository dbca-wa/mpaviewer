library(ggplot2)
library(dplyr)
library(tidyr)
library(googlesheets4)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Monitoring/Ningaloo/Data")
dir()

#Can use size to distinguish rather than species if want to look at things like "large herbivores"
#Load data, fill in blank genus with family and create taxa column
raw.data <- read.csv("Ningaloo_DOVs_2010-2019_Checked.csv",na.strings=c("","NA"))%>% 
  filter(!is.na(period))%>%
  mutate(genus=as.character(genus))%>%
  mutate(family=as.character(family))%>%
  mutate(genus = ifelse(is.na(genus),family,genus))%>% 
  mutate(taxa = paste(genus, species, sep = ' '))

#Add Community Info
####----  5.1 Pull out community data and make Scientific name col----####
species.info = read_sheet("https://docs.google.com/spreadsheets/d/1qhqh1GSPWPaO6xRKAyI0FQEHVvlEOMnz_0hGqRKs5SA/edit?usp=sharing")%>%
  setNames(tolower(names(.)))%>%
  filter(region.code=="NMP")%>% #Filter by marine park
  mutate(scientific=paste(family,genus,species, sep=" "))

####----  5.2 Make sure all datasets contain a 'scientific' column----####
raw.data<-raw.data%>%
  mutate(scientific=paste(family,taxa, sep=" "))

####----  5.3 Add community and target species data----####
combined.data<- species.info%>%
  select(-c(family,genus, species))%>%
  right_join(raw.data, by="scientific")

####----  5.4 Pull out species with no community data----####
missing.com<-combined.data%>%
  dplyr::filter(is.na(community))%>%
  dplyr::distinct(scientific)%>%
  glimpse()

#Write the data - you need to check this add missing species to the GS 
#and make sure you are happy with the rest i.e. spp could fit in multiple categories
write.csv(missing.com,"Community.missing.csv",row.names=FALSE )

#Check column names

colnames (combined.data)
firstletter <- toupper(substring(names(combined.data),1,1))
others <- tolower(substring(names(combined.data), 2))
newnames<- paste(firstletter, others, sep="")
names(combined.data)<- newnames

#Limit to essential columns
ess.data = combined.data %>% select(Year,Zone,Site,Period,Taxa,Number,Community)%>%
  mutate(Number=as.integer(Number))

#Sum abundance values for individual species within site and period

sum.com.ab = ess.data %>% group_by(Year, Zone, Site, Period, Community)%>%
  summarise(total = sum(Number))

#Add 0 values to dataset for transects where fish species weren't counted
wide.com.ab = sum.com.ab %>% spread(Community, total, fill = 0)

#Gather back together (check number of columns)
long.com.ab  = wide.com.ab %>% 
  gather(key = Community, value = Number,5:11)

#Remove 2010/11 DOVs not swum correctly and 2013 as only a few sites were done
long.com.ab = long.com.ab %>% 
  filter(!Year %in% c("2010","2011", "2013"))

#Check sites that were not sampled consitantly across all years
table = table(long.com.ab$Site,long.com.ab$Year)
table
row_sub = apply(table, 1, function(row) all(row !=0 ))
finalsites = table[row_sub,]
finalsites

#Have also removed L2 due to abnormally large schools of nebs in 2014
#Note will show all sites in separate plot
sites.limited = long.com.ab %>% filter(Site %in% rownames(finalsites))%>% 
  filter(!Site %in% "L2")

#Obtain mean and SE values for each zone across years
means = sites.limited %>% group_by(Year,Zone,Community)%>%
  summarise(N = length(Number),mean = mean(Number),sd = sd(Number),se = sd(Number)/ sqrt(length(Number)))

#Plot
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetFig <- ggplot(means, aes(x=Year, y=mean, group=Zone, shape=Zone, colour = Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd, colour = "black") + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=4), se=F, size = 1) +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(aes(fill = Zone),position=pd, size=5,colour = "black") +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(means$Year-0.25),max(means$Year+0.25)), breaks=min(means$Year):max(means$Year)) +
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

TargetFig+facet_grid(Community~.,scales = "free_y")
