library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Monitoring/Ningaloo/Data")
dir()

#Load data, fill in blank genus with family and create taxa column
raw.data <- read.csv("Ningaloo_DOVs_2010-2019_Checked.csv",na.strings=c("","NA"))%>% 
  filter(!is.na(period))%>%
  filter(!is.na(length..mm.))%>%
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

#Replicate rows where n is >1 for length
raw.data = raw.data[rep(row.names(raw.data),raw.data$Number),]

#Rename some species to lump
raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus maculatus' = "Plectropomus spp")
raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus leopardus' = "Plectropomus spp")
raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus areolatus' = "Plectropomus spp")
raw.data$Taxa = recode(raw.data$Taxa,'Epinephelus fasciatus' = "Epinephelus rivulatus")

#Limit to essential columns & remove 2010/11 DOVs not swum correctly and 2013 as only a few sites were done
ess.data = raw.data %>% 
  mutate(Length = Length..mm.)%>%
  select(Year,Zone,Site,Period,Taxa,Length,Number)%>%
  filter(!Year %in% c("2010","2011", "2013"))

#Work out which species can be plotted
totals = ess.data %>% group_by(Year, Zone, Taxa)%>%
  summarise(total = sum(Number))%>%
  filter(total > 20)

#Subset species you want in the plot and order them
target.sp = ess.data %>% filter(Taxa %in% c("Lethrinus nebulosus","Plectropomus spp","Epinephelus rivulatus","Lutjanus carponotatus"))%>%
  mutate(Taxa = factor(Taxa,levels = c("Lethrinus nebulosus","Plectropomus spp","Epinephelus rivulatus","Lutjanus carponotatus")))

# Set fill
# Pre-Plot Sets Colours, Fill and Dodge
ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() } 

PaletteFill<- scale_fill_manual(values=c('#00CCFF','#99CC33'))
PaletteColour<- scale_colour_manual(values=c('#00CCFF','#99CC33'))

Plot<-ggplot(target.sp,aes(x=Length,fill = Zone))+
  #geom_histogram(aes(y=..density..),binwidth=50,colour="black", fill="white")+
  #geom_density(data = species[which(species$Broad_1_Hard.Corals>0),],alpha=.2, aes(fill = "Broad_1_Hard.Corals",colour = "Broad_1_Hard.Corals"))+
  #geom_density(data = species[which(species$Broad_2_Macroalgae>0),],alpha=.2, aes(fill = "Broad_2_Macroalgae",colour = "Broad_2_Macroalgae"))+
  #geom_density(data = species[which(species$Broad_4_Unconsoldated.Substrate>0),],alpha=.2, aes(fill = "Broad_4_Unconsoldated.Substrate",colour = "Broad_4_Unconsoldated.Substrate"))+
  geom_density(aes(y = ..density..*1000),alpha=0.5, size = 0.7)+
  #geom_vline(data = means, aes(xintercept=mean,colour =Zone),linetype="dashed", size=1)+
  #geom_vline(xintercept=c(450), linetype="dotted",colour = "red",size = 1)+
  #geom_text(data = P.leopardus,label = "P < 0.001", size=5, y =5.6,fontface=4)+
  #geom_text(data = C.rubescens,label = "P = 0.021", size=5, y =4.1,fontface=4)+
 # geom_text(data = C.auratus,label = "P = 0.02", size=5, y =2.5,fontface=4)+
 # geom_text(data = L.miniatus,label = "P = 0.053", size=5, y =8.2,fontface=3)+
  theme(legend.position = ("bottom"))+
  theme(strip.text.y = element_text(size = 12,angle = 270),
        strip.background = element_blank(),
        axis.title=element_text(face="bold"), 
        plot.title=element_text(face="italic",hjust = 0.5),
        strip.text.x=element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.key = element_blank(),
        #legend.position = c(1,0),legend.justification = c(-0.5, 0.1),
        #axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = "black"))+
        #plot.margin=unit(c(0.5,0.5,3,0),"cm"))+
  PaletteFill+
  PaletteColour+
  scale_x_continuous(expand = c(0, 0), breaks=seq(0,800,100)) + 
  scale_y_continuous(expand = c(0, 0.1))+
  coord_cartesian(xlim = c(60, 700))+
  #ggtitle("Plectropomus spp")+
  ylab("Weighted KDE (*1000)")+
  xlab("Total Length (mm)")
Plot <- Plot + facet_grid(Year~Taxa) 
Plot

