
#### R script written to create plots for coral cover at Ningaloo  ####
# Written by George Shedrawi DBCA
# Last updated: 31/01/2022 

# Files required:
     # NIN_EcoPAAS_all.csv
     # nmp_metadata_2.csv
     # preEcoPAASdataNINTgshcorrected.csv
     # nmpcoords.csv

# Clear memory
rm(list=ls())
if(!is.null(dev.list())) dev.off() # clear plot panel
cat("\014") # Clear console


# Load libraries
library(plyr) 
library(reshape2)
library(ggplot2)
library(gridExtra)
library(MuMIn) 
library(mgcv)
library(visreg)
library(xlsx)

# Set working directory 
data.path <- "C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover"
setwd(data.path)

# Create data frame from raw EcoPAAS data
nmpcoral=read.table("C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover/NIN_EcoPAAS_all.csv",sep=",",header=TRUE)

# Check the formatting of column names - consistency in scripts and files exported from EcoPAAS 
# 'site' and 'year' spelt with lower case in excel files and script
# 'SiteCode' always spelt with uppercase 'S' and 'C' but no full stop in between

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Notes: 
# These scripts are made to work on data straight from the raw ECOPAAS export
# Then the errors with year/spelling/surveys are fixed here - see below

##### Fix problems in original EcoPAAS data #########
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5A-2013124103206"] <- 2012
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5B-2013124104234"] <- 2012
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5C-2013124105143"] <- 2012
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6A-2013124123718"] <- 2012
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6B-2013124124621"] <- 2012
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6C-2013124125656"] <- 2012
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-TSZ-N1-N1A-20091219143521"] <- 2010
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-TSZ-N1-N1B-20091219145035"] <- 2010
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-TSZ-N1-N1C-20091219150516"] <- 2010
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6A-201517141422"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6B-201517142154"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6C-201517143154"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-MUSZ-N2-N2A-201516123326"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-MUSZ-N2-N2B-201516124543"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-MUSZ-N2-N2C-201516125635"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-OSZ-N3-N3A-201516143440"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-OSZ-N3-N3B-201516144509"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-OSZ-N3-N3C-201516145437"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6A-2015111122755"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6B-2015111121604"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6C-2015111120340"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5A-2015111135822"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5B-2015111140952"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5C-2015111142018"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7A-201519085254"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7B-201519090027"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-201519090851"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-STH-CFSZ-S5-S5A-201519131117"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-STH-CFSZ-S5-S5B-201519132131"] <- 2014
nmpcoral$year[nmpcoral$Survey == "NIN-STH-CFSZ-S5-S5C-201519133042"] <- 2014
nmpcoral$Replicate[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-2010126093153"] <- 'S7B'
nmpcoral$ReplicateCode[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-2010126093153"] <- 'NIN-STH-3MSZ-S7-S7B'
levels(nmpcoral$Survey) = c(levels(nmpcoral$Survey), "NIN-STH-3MSZ-S7-S7B-2010126093153")
nmpcoral$Survey[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-2010126093153"] <-'NIN-STH-3MSZ-S7-S7B-2010126093153'

# Rename a few surveys from 2018 that were mis-labelled 
# (i.e. the survey lavel for the 49th or 50th image was different from the rest for that exact year/site/transect)
nmpcoral$Survey[nmpcoral$Survey == "NIN-STH-TUSZ-S8-S8C-20181125133009"] <-'NIN-STH-TUSZ-S8-S8C-20181125132838'
nmpcoral$Survey[nmpcoral$Survey == "NIN-BUN-MMA-B2-B2C-2018121142944"] <-'NIN-BUN-MMA-B2-B2C-2018121142910'
nmpcoral$Survey[nmpcoral$Survey == "NIN-BUN-BSZ-B4-B4B-20181130143650"] <-'NIN-BUN-BSZ-B4-B4B-20181130143438'
nmpcoral$Survey[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5A-2018126142311"] <-'NIN-BUN-BRZ-B5-B5A-2018126142226'
nmpcoral$Survey[nmpcoral$Survey == "NIN-STH-MSZ-S3-S3B-20181122140215"] <-'NIN-STH-MSZ-S3-S3B-20181122140045'
nmpcoral$Survey[nmpcoral$Survey == "NIN-STH-MSZ-S3-S3B-20181122140215"] <-'NIN-STH-MSZ-S3-S3B-20181122140045'
nmpcoral$Survey[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5C-20191118212240"] <-'NIN-BUN-BRZ-B5-B5C-20191118211840' 

# Allocate early Jan 2019 surveys to 2018 'survey year' for consistency
# Lefroy surveyed in Jan 2019 but is part of the Nov/Dec 2018 survey group 
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6A-201918095939"] <- 2018
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6B-201918101044"] <- 2018
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6C-201918102203"] <- 2018
# Winderbandi surveyed in Jan 2019 but is part of the Nov/Dec 2018 survey group 
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5A-201918115715"] <- 2018
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5B-201918120435"] <- 2018
nmpcoral$year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5C-201918121053"] <- 2018

# to remove those ID's made on points 6 to 20 to make it comparable for future years with only 6 points per image - must follow these steps in order
colnames(nmpcoral)
nmpcoral$year.image.point.total  <- paste(nmpcoral$year, nmpcoral$ImageName, nmpcoral$TotalPoints, sep = ".") # step 1
nmpcoral$year.image.point.total<-as.character(nmpcoral$year.image.point.total) # step 2
nmpcoral <- nmpcoral[!(nmpcoral$year.image.point.total == "2010.NIN-BUN-BSZ-B3-B3A-L_20100122140114.20"),] #step 3
nmpcoral = subset(nmpcoral, nmpcoral$PointNo < 6 ) # Step 4 
nmpcoral = nmpcoral[,1:45] # step 5 

## Re-name sites for consistency 
nmpcoral$site<-as.character(nmpcoral$site)
nmpcoral$site[nmpcoral$site == "Bruboodjoo"] <- "Bruboodijoo"
nmpcoral$site[nmpcoral$site == "Gnarraloo Bay"] <- "Gnaraloo Bay"
nmpcoral$site[nmpcoral$site == "Pelican"] <- "Pelican Point"
nmpcoral$site[nmpcoral$site == "Turquoise"] <- "Turquoise Bay"
nmpcoral$site[nmpcoral$site == "Osprey"] <- "Osprey Bay"
nmpcoral$site[nmpcoral$site == "Turquoise Bay 1"] <- "Turquoise Bay High Use"
nmpcoral$site[nmpcoral$site == "Oyster Stacks 1"] <- "Oyster Stacks High Use"
nmpcoral$site<-as.factor(nmpcoral$site)

# Save the corrected EcoPass data - this is NIN_EcoPAAS_all but with sites and names corrected 
# Export is optional:
# write.csv(nmpcoral, "DBCA_Ningaloo_Raw_Coral_20220401.csv")

# OPTIONAL: 
## To either keep or delete the high use sites and Bundegi site 7 (new coral cover site created in 2019 - is a recruitment site)
#nmpcoral = nmpcoral[!(nmpcoral$site == "Turquoise Bay High Use" | nmpcoral$site == "Oyster Stacks High Use" | nmpcoral$site == "Bundegi SZ South Upper"),]
#nmpcoral$site<-droplevels(nmpcoral$site)
#levels(nmpcoral$site)
#colnames(nmpcoral)
#nmpcoral$SiteCode<-nmpcoral$SiteCode
#deleted by george #nmpcoral1$SiteCode<-as.character(nmpcoral1$SiteCode)
#nmpcoral$SiteCode <- as.character(lapply(strsplit(as.character(nmpcoral$SiteCode), "\\-"), "[", 4))
#nmpcoral$SiteCode<-as.factor(nmpcoral$SiteCode)
#class(nmpcoral$SiteCode)

# ----------------------------------------------------------------------------------------------------------------------------------
# Data Analysis for clean summarized data (zero corrected)
# E.g. for James McGree UQ - level 4 class counts
nmpcoral_raw_JM=plyr::count(nmpcoral, c("Survey", "Level2Class", "Level3Class", "Level4Class")) #counts number of observations of LVL1 per site, per year
nmpsite_JM=plyr::count(nmpcoral_raw_JM, c("Survey"), "freq") #counts number of observations made at each site per year
nmpLVL4_replicate_JM <- join(nmpcoral_raw_JM, nmpsite_JM, by = "Survey") #adds count of site observations against the right site/year to allow percentage calculation

# Re-name the columns to make more sense
colnames(nmpLVL4_replicate_JM)
names(nmpLVL4_replicate_JM)[3] <- "Level3Class" 
names(nmpLVL4_replicate_JM)[4] <- "Level4Class" 
names(nmpLVL4_replicate_JM)[5] <- "Level4_Count" 
names(nmpLVL4_replicate_JM)[6] <- "Replicate_Count" 

nmpsector=nmpcoral[,c("Zone", "Sector", "site", "Replicate", "Survey","Date", "year")] # Create a list of sectors, zones and sites
nmpsectors = unique(nmpsector) # Makes the list unique values only
nmpLVL4_JM = join(nmpLVL4_replicate_JM, nmpsectors, by = "Survey") # Adds the site, sector and zone information to percent information
head(nmpLVL4_JM)

# Now include zeros when something wasn't seen for each survey
colnames(nmpLVL4_JM)

# As well as casting the data it puts a zero for each species in the sample it wasn't seen, otherwise just using the imported data does not have the zeroes....
# This is super important !
dat.all.cast<-dcast(nmpLVL4_JM, Survey + year + Date + Zone + Sector + site + Replicate + Replicate_Count  ~ Level2Class+Level3Class+Level4Class, value.var = "Level4_Count", fill = 0, drop=T) 

# This then returns it to column views for graphing
nintotalwithzeroes<-melt(dat.all.cast, c("Survey", "year", "Date", "Sector", "Zone","site", "Replicate", "Replicate_Count")) 
colnames(nintotalwithzeroes)
head(nintotalwithzeroes)
names(nintotalwithzeroes)[9] <- "GenusLevel4Class" # Rename column
names(nintotalwithzeroes)[10] <- "PointCount" # Rename column
head(nintotalwithzeroes)
nintotalwithzeroes$percent_cover = nintotalwithzeroes$PointCount/nintotalwithzeroes$Replicate_Count *100 #Calculate percent cover
head(nintotalwithzeroes)

# Add in standardized latitude and longitude per 'Replicate' (i.e. one set of coordinates per transect rather than per image)
nmpcoords=read.table("C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover/nmpcoords.csv",sep=",",header=TRUE)
nintotalwithzeroes2= join (nmpcoords, nintotalwithzeroes, by = "Replicate")
head(nintotalwithzeroes2)

# Export/save and sort for data sharing 
write.csv(nintotalwithzeroes2, file = "C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover/DBCA_Ningaloo_Level4_benthic_count_data_zeroes.csv", row.names = F)

# Now triple check the exported csv file in excel 
# ----------------------------------------------------------------------------------------------------------------------------------
# Data Analysis for plotting 
nmpcoral_raw=plyr::count(nmpcoral, c("Survey", "Level2Class")) #counts number of observations of LVL1 per site, per year
nmpsite=plyr::count(nmpcoral_raw, c("Survey"), "freq") #counts number of observations made at each site per year
nmpLVL2_replicate <- join(nmpcoral_raw, nmpsite, by = "Survey") #adds count of site observations agains the right site/year to allow percentage calculation
names(nmpLVL2_replicate)[3] <- "LVL2_Count" #Rename column to make more sense
names(nmpLVL2_replicate)[4] <- "Replicate_Count" #Rename column to make more sense

nmpsector=nmpcoral[,c("Zone", "Sector", "site", "Replicate", "Survey", "year")] #Create a list of sectors, zones and sites
nmpsectors = unique(nmpsector) #Makes the list unique values only
nmpLVL2 = join(nmpLVL2_replicate, nmpsectors, by = "Survey") #adds the site, sector and zone information to percent information

# ----------------------------------------------------------------------------------------------------------------------------------------------
# Create Coral data.frame for Coral cover plots
nmpLVL2coral = subset(nmpLVL2, Level2Class %in% c("Hard coral","Octocorals - Hard")) # Extracts coral cover information only
nmpLVL2corala = join (nmpsectors, nmpLVL2coral, by = "Survey")
nmphardcoral = nmpLVL2corala[,c("Zone", "Sector", "site", "Replicate", "Survey", "year", "Level2Class",  "LVL2_Count")]
nmphardcoral = join (nmphardcoral, nmpsite, by = "Survey")
names(nmphardcoral)[9] <- "Replicate_Count"
nmphardcoral$LVL2_Count[is.na(nmphardcoral$LVL2_Count)] <- 0
nmphardcoral$Level2Class[is.na(nmphardcoral$Level2Class)] <- 'Hard coral'
nmphardcoral$percentcover = nmphardcoral$LVL2_Count/nmphardcoral$Replicate_Count *100 #Calculate percent cover
# ----------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the old Pre-Ecopass data (1991 to 2010 - data is to family level only)
Old_nmpLVL2coral=read.table("C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover/preEcoPAASdataNINTgshcorrected.csv", sep=",", header=TRUE) 
#Old_nmpLVL2coral=read.xlsx("C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover/preEcoPAASdataNINTgshcorrected.xlsx", sheetIndex = 1)

# Check column name spelling 
colnames(Old_nmpLVL2coral)
head(Old_nmpLVL2coral, n=2)

nmphardcoralb = tapply(nmphardcoral$LVL2_Count,nmphardcoral$Survey,sum)
Coral=data.frame(nmphardcoralb)   
Coral$Survey=rownames(Coral)
nmphardcorals=join(nmphardcoral,Coral,"Survey")
nmphardcorals$LVL2_Count = NULL
names(nmphardcorals)[9] = "LVL2_Count"
nmphardcorald=subset(nmphardcoral, Level2Class == "Hard coral")
nmphardcorald$percentcover = nmphardcorald$LVL2_Count/nmphardcorald$Replicate_Count *100 

# Join EcoPAAS data and other data together into one data.frame
nmpcoralcover_all=rbind(nmphardcorald, Old_nmpLVL2coral)
delete<-data.frame(unique(nmpcoralcover_all$site))
nmpmetadata2=read.table("C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover/nmp_metadata_2.csv",sep=",",header=TRUE)
nmpcoralcover_all1 = join (nmpmetadata2, nmpcoralcover_all, by = "site")

# Also - note this is the data we use for the ePower tests (Hard coral point counts)
write.csv(nmpcoralcover_all, file = "C:/Users/claireross/OneDrive - Department of Biodiversity, Conservation and Attractions/Data/Monitoring/SPP_2012_008_Coral/NMP/Coral_cover/NINcoralcover_all.csv", row.names = F)

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create Sector data.frame for Sector plots
nmpLVL1coral_allMurion = subset(nmpcoralcover_all, site %in% c("North Muiron 1", "North Muiron 2", "North Muiron 3", "Muiron Island North", "Muiron Island South"))
nmpLVL1coral_allBundegi = subset(nmpcoralcover_all, site %in% c("Bundegi 1", "Bundegi 2", "Bundegi 3","BUNDEGI", "Bundegi", "N1 Bundegi", "N19 Bundegi Sanctuary", "Bundegi North", "Bundegi South", "Bundegi SZ North", "Bundegi SZ South", "Bundegi SZ South Upper", "Murat"))
nmpLVL1coral_allNorth = subset(nmpcoralcover_all, Sector %in% c("Ningaloo-North", "North"))
nmpLVL1coral_allSouth = subset(nmpcoralcover_all, Sector %in% c("Ningaloo-South", "South", "South "))

# Create Zone data.frame for Zone plots
nmpLVL1coral_allSanctuary = subset(nmpcoralcover_all, Zone %in% c("Sanctuary", "Sanctuary Zone - Bundegi", "Sanctuary Zone - Cape Farquhar", "Sanctuary Zone - Cloates", "Sanctuary Zone - Gnarraloo Bay", "Sanctuary Zone - Mandu", "Sanctuary Zone - Maud", "Sanctuary Zone - Murat", "Sanctuary Zone - Osprey", "Sanctuary Zone - Pelican", "Sanctuary Zone - Tantabiddi", "Sanctuary Zone - Three Mile", "Sanctuary Zone - Turtles", "Sanctuary Zone - Winderabandi"))
nmpLVL1coral_allRecreation = subset(nmpcoralcover_all, Zone %in% c("Recreation", "Recreation Zone - Bundegi", "Recreation Zone - Cloates", "Recreation Zone - Winderabandi"))
nmpLVL1coral_allConservation = subset(nmpcoralcover_all, Zone %in% c("Conservation Area", "Conservation Area - Muiron Islands North", "Management Area - Muiron Islands"))

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Code from here is used to create all plots
# Sector Plots
nmpcoralcover_mean=rbind(nmpLVL1coral_allMurion, nmpLVL1coral_allBundegi, nmpLVL1coral_allNorth, nmpLVL1coral_allSouth)

# Re-name as sectors: Northern, Eastern and Southern
nmpcoralcover_mean$Sector<-as.character(nmpcoralcover_mean$Sector)
nmpcoralcover_mean$site<-as.character(nmpcoralcover_mean$site)
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "Ningaloo-North"] <-"Northern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "North"] <-"Northern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "Ningaloo-South"] <-"Southern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "South "] <-"Southern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "South"] <-"Southern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$site == "Bundegi SZ South"] <-"Eastern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$site == "Bundegi SZ South Upper"] <-"Eastern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$site == "Bundegi SZ North"] <-"Eastern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$site == "Bundegi"] <-"Eastern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$site == "Murat"] <-"Eastern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$site == "Muiron Island North"] <-"Eastern Sector"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$site == "Muiron Island South"] <-"Eastern Sector"
nmpcoralcover_mean$Sector<-as.factor(nmpcoralcover_mean$Sector)
nmpcoralcover_mean$site<-as.factor(nmpcoralcover_mean$site)
nmpcoralcover_mean<- droplevels(nmpcoralcover_mean)

# Export the summarized data (optional)
#write.csv(nmpcoralcover_mean, "nmpcoralcover_mean.csv")
colnames(nmpcoralcover_mean)
levels(nmpcoralcover_mean$Sector)
nmpcoralcover_mean$year<-as.numeric(nmpcoralcover_mean$year)

# Re-name 1999 to 1998 because these are part of the same annual survey and were quick trips to finish off sites 
# Other surveys in recent years fixed above e.g. for 2019
nmpcoralcover_mean$Sector <- factor(nmpcoralcover_mean$Sector, levels= c("Eastern Sector", "Northern Sector", "Southern Sector"))
nmpcoralcover_mean<-nmpcoralcover_mean[order(nmpcoralcover_mean$Sector), ]
nmpcoralcover_mean_site<-nmpcoralcover_mean
nmpcoralcover_mean_site <- within(nmpcoralcover_mean_site, year[ year == "1999" & Sector == "Northern Sector"] <- "1998")
nmpcoralcover_mean_site <- within(nmpcoralcover_mean_site, year[ year == "1999" & Sector == "Southern Sector"] <- "1998")
nmpcoralcover_mean_site <- within(nmpcoralcover_mean_site, year[ year == "1999" & Sector == "Eastern Sector"] <- "1998")

## Mean by year for entire park all sites 
nmpcoralcover_mean_site <- ddply(nmpcoralcover_mean, .(year), .inform=TRUE, summarise, 
                                 N    = length(unique(site)),
                                 Mean = mean(percentcover),
                                 sd   = sd(percentcover),
                                 SE   = sd(percentcover) / sqrt(length(unique(site))))

## Optional: very simple mean per year for entire park 
#write.csv(nmpcoralcover_mean_site, "nmpcoralcover_mean_site.csv", row.names = F)


# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Coral cover plot (Plot #1)
# This figure shows the mean for the park
Plot1 =ggplot(data=subset(nmpcoralcover_mean_site, !year %in% c("1999")), aes(x=year, y=Mean)) +
  geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.025) + 
  stat_smooth(method = "gam", formula = y ~ s(x,k=5), se=T, size = 1,col="black",linetype="solid") +
  geom_point(size=2) +              
  xlab("") +
  ylab("% Coral Cover (mean ± SE)") +
  scale_y_continuous(limits=c(-5,80)) +
  scale_x_continuous(limits=c(1990,max(nmpcoralcover_mean_site$year)+1), breaks = seq(min(nmpcoralcover_mean_site$year), max(nmpcoralcover_mean_site$year), 2))+
  theme_bw() +                                    
  theme(
    strip.text.x = element_text(size=12),           
    strip.background = element_blank(),            
    axis.text.x = element_text(size=11,angle = 90, vjust=0.5, hjust=1),
    axis.text.y = element_text(size=11),        
    axis.title.x=element_blank(),                  
    axis.title.y=element_text(size=12),               
    axis.line.x=element_line(colour="black"),        
    axis.line.y=element_line(colour="black"),
    panel.grid.minor = element_blank(),              
    panel.grid.major = element_blank(),               
    panel.border=element_blank(),                    
    panel.background=element_blank(),                 
    legend.justification=c(1,1), legend.position=c(1,0.3), 
    legend.title = element_blank(),                  
    legend.key = element_blank())                    
Plot1

## Save
ggsave("NMP_Coral_Cover_All_Park_BW_2019.png", plot = Plot1, width = 16, height = 8, units = c( "cm"))

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Means for plotting by sector 
nmpcoralcover_mean_sector<-nmpcoralcover_mean
nmpcoralcover_mean_sector<-nmpcoralcover_mean
nmpcoralcover_mean_sector$site<-as.factor(nmpcoralcover_mean_sector$site)
nmpcoralcover_mean_sector<-droplevels(nmpcoralcover_mean_sector)
head(nmpcoralcover_mean_sector, n=5)

nmpcoralcover_mean_sector$year<-as.numeric(nmpcoralcover_mean_sector$year)
nmpcoralcover_mean_sector <- ddply(nmpcoralcover_mean_sector, .(year, Sector, site), .inform=TRUE, summarise, 
                                   N    = length(unique(site)),
                                   Mean = mean(percentcover),
                                   sd   = sd(percentcover),
                                   SE   = sd(percentcover) / sqrt(length(unique(site))))
nmpcoralcover_mean_sector$meanbinomial=nmpcoralcover_mean_sector$Mean/100
class(nmpcoralcover_mean_sector$meanbinomial)
class(nmpcoralcover_mean_sector$Mean)
class(nmpcoralcover_mean_sector$year)

# Rename the sites for consistency
nmpcoralcover_mean_sector <- within(nmpcoralcover_mean_sector, site[site == "Turquoise Bay"] <- "Turquoise")
nmpcoralcover_mean_sector <- within(nmpcoralcover_mean_sector, site[site == "Bruboodijoo"] <- "Bruboodjoo")
nmpcoralcover_mean_sector <- within(nmpcoralcover_mean_sector, site[site == "Osprey Bay"] <- "Osprey")
nmpcoralcover_mean_sector <- within(nmpcoralcover_mean_sector, site[site == "Gnarraloo Bay"] <- "Gnaraloo Bay")
nmpcoralcover_mean_sector <- within(nmpcoralcover_mean_sector, site[site == "Pelican Point"] <- "Pelican")

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove inconsistent sites (Bundegi SZ South, Bundegi SZ North, and Murat) for stats (see: nmpcoralcover_mean_sector2) 
# Makes Sweet FA difference
# But keep these sites in for plotting to see the site specific trends (see: nmpcoralcover_mean_sector)
nmpcoralcover_mean_sector2<-nmpcoralcover_mean
nmpcoralcover_mean_sector2<-nmpcoralcover_mean
nmpcoralcover_mean_sector2$site<-as.factor(nmpcoralcover_mean_sector2$site)
nmpcoralcover_mean_sector2 <- nmpcoralcover_mean_sector[!(nmpcoralcover_mean_sector2$site == "Bundegi SZ South" | nmpcoralcover_mean_sector2$site == "Bundegi SZ South Upper" |nmpcoralcover_mean_sector2$site == "Bundegi SZ North" | nmpcoralcover_mean_sector2$site == "Murat" ),]
nmpcoralcover_mean_sector2<-droplevels(nmpcoralcover_mean_sector2)
head(nmpcoralcover_mean_sector2, n=5)

nmpcoralcover_mean_sector2$year<-as.numeric(nmpcoralcover_mean_sector2$year)
nmpcoralcover_mean_sector2 <- ddply(nmpcoralcover_mean_sector2, .(year, Sector, site), .inform=TRUE, summarise, 
                                   N    = length(unique(site)),
                                   Mean = mean(percentcover),
                                   sd   = sd(percentcover),
                                   SE   = sd(percentcover) / sqrt(length(unique(site))))
nmpcoralcover_mean_sector2$meanbinomial=nmpcoralcover_mean_sector2$Mean/100
class(nmpcoralcover_mean_sector2$meanbinomial)
class(nmpcoralcover_mean_sector2$Mean)
class(nmpcoralcover_mean_sector$year)

# -----------------------------------------------------------------------------------------------------------------------------------------------
# Sector plot (plot # 2)
# For this plot, if you don't want to include Bundegi SZ South, Bundegi SZ North, and Murat use nmpcoralcover_mean_sector2 (version 2) 
# because they don't have consistent sampling pre-2010 and aren't included in GAM stats
Plot2 = ggplot(data=subset(nmpcoralcover_mean_sector, !year %in% c("1999")), aes(group=Sector,x=year, y=Mean)) +
  stat_smooth(data=nmpcoralcover_mean_sector, method = "gam", formula = y ~ s(x), se=F, colour = "black") +                                     
  geom_point(size=2) +                   
  xlab("") +
  ylab("% Coral Cover ") +
  facet_wrap(~ Sector, ncol= 1, scales = "free") +                  
  scale_y_continuous(limits=c(-10,max(nmpcoralcover_mean_sector$Mean)+10)) +
  scale_x_continuous(limits=c(min(nmpcoralcover_mean_sector$year),max(nmpcoralcover_mean_sector$year)), breaks = seq(min(nmpcoralcover_mean_sector$year), max(nmpcoralcover_mean_sector$year), 2))+
  theme_bw() +                                 
  theme(
  strip.text.x = element_text(size=8),           
  strip.background = element_blank(),            
  axis.text.x = element_text(size=8,angle = 90, vjust=0.5, hjust=1),            
  axis.text.y = element_text(size=8),              
  axis.title.x=element_blank(),                     
  axis.title.y=element_text(size=10),               
  axis.line.x=element_line(colour="black"),        
  axis.line.y=element_line(colour="black"),
  panel.grid.minor = element_blank(),              
  panel.grid.major = element_blank(),               
  panel.border=element_blank(),                    
  panel.background=element_blank(),                
  legend.title = element_blank())                  
Plot2

## Save 
ggsave("NMP_Coral_Cover_Sector_2019.png", plot = Plot2, width = 19, height = 19, units = c( "cm"))

# To export data (optional):
# write.csv(nmpcoralcover_mean, "nmpcoralcover_mean.csv")

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Site level plots (plot # 3)
nmpcoralcover_site_means <- ddply(nmpcoralcover_mean, .(year, Sector, site), .inform=TRUE, summarise, 
                                  N    = length(unique(site)),
                                  Mean = mean(percentcover),
                                  sd   = sd(percentcover),
                                  SE   = sd(percentcover) / sqrt(length(unique(site))))

levels(nmpcoralcover_site_means$site)

# Rename the sites for consistency
nmpcoralcover_site_means <- within(nmpcoralcover_site_means, site[site == "Turquoise Bay"] <- "Turquoise")
nmpcoralcover_site_means <- within(nmpcoralcover_site_means, site[site == "Bruboodijoo"] <- "Bruboodjoo")
nmpcoralcover_site_means <- within(nmpcoralcover_site_means, site[site == "Osprey Bay"] <- "Osprey")
nmpcoralcover_site_means <- within(nmpcoralcover_site_means, site[site == "Gnarraloo Bay"] <- "Gnaraloo Bay")
nmpcoralcover_site_means <- within(nmpcoralcover_site_means, site[site == "Pelican Point"] <- "Pelican")

Plot3 = ggplot(nmpcoralcover_site_means,  aes(group=site,x=year, y=Mean)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.025) + # error bars
  stat_smooth(method = "gam", formula = y ~ poly(x, 3), se=F, size = 0.3,col="black",linetype="dashed") +                                     
  geom_point(size=1) +                
  xlab("") +
  ylab(" % Coral Cover (mean ± SE)") +
  facet_wrap(~site, ncol= 3,  scales = "free") +                  
  scale_y_continuous(limits=c(0,75)) +
  scale_x_continuous(limits=c(1991,2019))+
  theme_bw() +                                    
  theme(
    strip.text.x = element_text(size=9),           
    strip.background = element_blank(),             
    axis.text.x = element_text(size=7),               
    axis.text.y = element_text(size=7),               
    axis.title.x=element_blank(),                     
    axis.title.y=element_text(size=9),              
    axis.line.x=element_line(colour="black"),       
    axis.line.y=element_line(colour="black"),
    panel.grid.minor = element_blank(),             
    panel.grid.major = element_blank(),             
    panel.border=element_blank(),                     
    panel.background=element_blank(),                 
    legend.justification=c(1,1), legend.position=c(1,0.3), 
    legend.title = element_blank(),                   
    legend.key = element_blank())                   
Plot3

## Save plot
ggsave("NMP_Coral_Cover_Site_Per_Panel_2019.png", plot = Plot3, width = 18, height = 19, units = c( "cm"))


# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Stats
# Create Gam for data averaged across all sites
nmpcoralcover_mean_site$year<-as.integer(nmpcoralcover_mean_site$year)
gamallsites<-gam(formula = Mean ~ s(year, k=4), data = nmpcoralcover_mean_site, na.action = "na.fail" )
summary(gamallsites)
AICc(gamallsites)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# GAMS
gamallsector<-gam(formula = Mean ~ s(year, by = Sector, k=4), data = nmpcoralcover_mean_sector, na.action = "na.fail")
summary(gamallsector)
plot(gamallsector, pages=3,scheme=1, unconditional=F)
vis.gam(gamallsector)
print(gamallsector)
AICc(gamallsector)
plot<-visreg(gamallsector, line=list(col="black"), legend = T, "year", by = "Sector", type ="conditional", ylab = "coral cover", ncol = 1 )

gamCoverEast = gam(data=subset(nmpcoralcover_mean_sector2, Sector == "Eastern Sector"), formula = Mean ~ s(year, k=4), na.action = "na.fail" )
summary(gamCoverEast)

gamCoverNorth = gam(data=subset(nmpcoralcover_mean_sector2, Sector == "Northern Sector"), formula = Mean ~ s(year, k=7), na.action = "na.fail" )
summary(gamCoverNorth)

gamCoverSouth = gam(data=subset(nmpcoralcover_mean_sector2, Sector == "Southern Sector"), formula = Mean ~ s(year, k=4), na.action = "na.fail" )
summary(gamCoverSouth) 

# --------------------------------------------------------------------------------------------------------------------------------------------------------
## More gam's
class(nmpcoralcover_mean$year)
length(unique(nmpcoralcover_mean$year))
gam.cc.site.reps.k.1<-gam(data=nmpcoralcover_mean, percentcover~s(year, k=length(unique(nmpcoralcover_mean$year))-1), method = "REML", family = tw())
plot(gam.cc.site.reps.k.1 , residuals = T)
par(mfrow = c(2,2))
gam.check(gam.cc.site.reps.k.1)

gam.cc.site.reps.k3<-gam(data=nmpcoralcover_mean, percentcover~s(year, k=3))
plot(gam.cc.site.reps.k3)

gam.cc.site.reps.k4<-gam(data=nmpcoralcover_mean, percentcover~s(year, k=4))
plot(gam.cc.site.reps.k4)

gam.cc.site.reps.k5<-gam(data=nmpcoralcover_mean, percentcover~s(year, k=5))
plot(gam.cc.site.reps.k5)

gam.cc.site.reps.k6<-gam(data=nmpcoralcover_mean, percentcover~s(year, k=6))
plot(gam.cc.site.reps.k6)

gam.cc.site.reps.k7<-gam(data=nmpcoralcover_mean, percentcover~s(year, k=7))
plot(gam.cc.site.reps.k7, residuals = T)

visreg(gam.cc.site.reps.k.1)

anova(gam.cc.site.reps.k.1,gam.cc.site.reps.k3,gam.cc.site.reps.k4,gam.cc.site.reps.k5,gam.cc.site.reps.k6,gam.cc.site.reps.k7)
MuMIn::AICc(gam.cc.site.reps.k.1,gam.cc.site.reps.k3,gam.cc.site.reps.k4,gam.cc.site.reps.k5,gam.cc.site.reps.k6,gam.cc.site.reps.k7)

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# The end 
