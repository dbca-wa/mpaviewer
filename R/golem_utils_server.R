#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList


## _______________________________________________________ ----
##                FUNCTIONS AND DIRECTORIES                ----
## _______________________________________________________ ----


addLegendSR <- function(map, colors, labels, sizes, opacity = 1, group) {
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0(
    "<div style='display: inline-block;height: ",
    sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
    labels, "</div>"
  )

  return(addLegend(map,
    colors = colorAdditions,
    labels = labelAdditions, opacity = opacity, title = "Species richness", group = group, position = "bottomright"
  ))
}



### ► Leaflet spinner ----
options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 2
)

### ► functions for summarising data on plots ----
se <- function(x) {
  sd(x) / sqrt(length(x))
}

se.min <- function(x) {
  (mean(x)) - se(x)
}

se.max <- function(x) {
  (mean(x)) + se(x)
}

### ► Function for reading in DBCA data


### ► Set data directory
# data.dir <- "./data"



# ### ► TEST KDE
# kde.plot <- ggplot(fished.complete.length, aes(x = length, fill = status))+
#   geom_density(aes(y = ..density..*1000),alpha=0.5, size = 0.7)+
#   theme(legend.position = ("bottom"))+
#   theme(strip.text.y = element_text(size = 12,angle = 270),
#         strip.background = element_blank(),
#         axis.title=element_text(face="bold"),
#         plot.title=element_text(face="italic",hjust = 0.5),
#         strip.text.x=element_text(size = 14),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         # panel.border = element_rect(colour = "black")
#   )+
#   scale_x_continuous(expand = c(0, 0), breaks=seq(0,1000,100)) +
#   scale_y_continuous(expand = c(0, 0.1))+
#   # coord_cartesian(xlim = c(60, 700))+
#   ylab("Weighted KDE (*1000)")+
#   xlab("Total Length (mm)")

# kde.plot <- kde.plot + facet_grid(year~scientific)
# kde.plot
#

# test.data.frame <- complete.length %>%
#   filter(method == "stereo-DOVs") %>%
#   # filter(number > 0) %>%
#   mutate(marine.park = as.factor(marine.park)) %>%
#   filter(scientific %in% c("Arripis georgianus", "Bodianus frenchii")) %>%
#   glimpse()


# test <- test.data.frame %>%
#   distinct(marine.park, scientific) %>%
#   group_by(scientific) %>%
#   summarise(n = n())
#
# split <- split(test.data.frame, f = test.data.frame$marine.park, drop = TRUE)
#
# plot_list <- lapply(split, function(dat) {
#
#   # label <- grobTree(textGrob(as.character(dat$marine.park), x = 0.01,  y = 0.97, hjust = 0,
#   #                            gp = gpar(col = "black", fontsize = 13, fontface = "italic")))
#
#   ggplot(dat, aes(x = length, fill = status)) +
#     geom_density(aes(y = ..density..*1000),alpha=0.5, size = 0.7) +
#     theme(legend.position = ("bottom")) +
#     theme(strip.text.y = element_text(size = 12,angle = 270),
#           strip.background = element_blank(),
#           axis.title=element_text(face="bold"),
#           plot.title=element_text(face="italic",hjust = 0.5),
#           strip.text.x=element_text(size = 14),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank()) +
#     scale_y_continuous(expand = c(0, 0.1))+
#     ylab("Weighted KDE (*1000)") +
#     xlab("Total Length (mm)") +
#     ggtitle(dat$marine.park) +
#     # annotation_custom(label) +
#     facet_grid(year~scientific)
# })
#
# plot_grid(plotlist = plot_list, ncol = 1) #, labels = names(plot_list)

# dat <- fished.complete.length
#
#
#
# plot_list <- lapply(data, function(dat) {
#   marine.park <- dat$marine.park
#   fished.complete.length
#   ggplot(fished.complete.length, aes(x = length, fill = status))+
#     geom_density(aes(y = ..density..*1000),alpha=0.5, size = 0.7)+
#     theme(legend.position = ("bottom"))+
#     theme(strip.text.y = element_text(size = 12,angle = 270),
#           strip.background = element_blank(),
#           axis.title=element_text(face="bold"),
#           plot.title=element_text(face="italic",hjust = 0.5),
#           strip.text.x=element_text(size = 14),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           # panel.border = element_rect(colour = "black")
#     )+
#     scale_x_continuous(expand = c(0, 0), breaks=seq(0,1000,100)) +
#     scale_y_continuous(expand = c(0, 0.1))+
#     # coord_cartesian(xlim = c(60, 700))+
#     ylab("Weighted KDE (*1000)")+
#     xlab("Total Length (mm)")
# })


# #Load data, fill in blank genus with family and create taxa column
# raw.data <- read.csv("data/NMP/Fish/DOVs/Ningaloo_DOVs_2010-2019_Checked.csv",na.strings=c("","NA"))%>%
#   filter(!is.na(period))%>%
#   mutate(genus=as.character(genus))%>%
#   mutate(family=as.character(family))%>%
#   mutate(genus = ifelse(is.na(genus),family,genus))%>%
#   mutate(taxa = paste(genus, species, sep = ' '))
#
# firstletter <- toupper(substring(names(raw.data),1,1))
# others <- tolower(substring(names(raw.data), 2))
# newnames<- paste(firstletter, others, sep="")
# names(raw.data)<- newnames
#
# #Rename some species to lump
# raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus maculatus' = "Plectropomus spp")
# raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus leopardus' = "Plectropomus spp")
# raw.data$Taxa = recode(raw.data$Taxa,'Plectropomus areolatus' = "Plectropomus spp")
# raw.data$Taxa = recode(raw.data$Taxa,'Epinephelus fasciatus' = "Epinephelus rivulatus")
#
# #Limit to essential columns
# ess.data = raw.data %>% select(Year,Zone,Site,Period,Taxa,Number)%>%
#   mutate(Number=as.integer(Number))
#
# #Sum abundance values for individual species within site and period
#
# sum.ab = ess.data %>% group_by(Year, Zone, Site, Period, Taxa)%>%
#   summarise(total = sum(Number))
#
# #Add 0 values to dataset for transects where fish species weren't counted
# wide.ab = sum.ab %>% spread(Taxa, total, fill = 0)
#
# #Add a column that totals all the target species you want
# wide.ab$Targeted = rowSums(wide.ab[,c("Epinephelus rivulatus","Lethrinus nebulosus","Plectropomus spp","Lutjanus carponotatus")] )
#
# #Gather back together (check number of columns)
# long.ab  = wide.ab %>%
#   gather(key = Taxa, value = Number,5:472)
#
# #Check sites that were not sampled consitantly across all years
# table = table(long.ab$Site,long.ab$Year)
# table
#
# #Remove sites with inconsistent sampling
# time.series <- long.ab %>%filter(!Site %in% c("CB4","CB5","CB6","CF1","CF2","CF3","CL1","L3","L4","M2","MB1","MB3","MB4","MI3","O1","O2","P1","W1","W2"))
# table2 = table(time.series$Site,time.series$Year)
# table2
#
# #Remove years for some sites and not others
# time.series = time.series%>%
#   mutate(Year_Site=paste(Year,Site, sep="_"))%>%
#   filter(!Year_Site %in% c("2014_L1","2014_L2","2011_P2","2011_P3","2013_MB2","2011_MB2","2013_L2","2011_CL2","2010_B3","2010_B4","2010_CB1","2010_CB2"))
#
# #Up to recode here! Need to convert to character
# #Lump sites into locations
# time.series = time.series%>%
#   mutate(Location = gsub("[^a-zA-Z]", "",Site,))%>%
#   mutate(Location = recode(Location,L = "Lighthouse",T="Tantabiddi",B = "Bundegi",CB = "Coral Bay",MI = "Muiron Islands",CL="Cloates",M="Mandu",O="Osprey",P="Pelican",MB = "Mangrove Bay"))
#
# #Obtain mean and SE values for each zone across years
# means = time.series %>% group_by(Year,Zone,Location,Taxa)%>%
#   summarise(N = length(Number),mean = mean(Number),sd = sd(Number),se = sd(Number)/ sqrt(length(Number)))
#
# #Subset species you want in the plot and order them
# target.sp = means %>% filter(Taxa %in% c("Targeted","Lethrinus nebulosus"))%>%
#   mutate(Taxa = factor(Taxa,levels = c("Targeted","Lethrinus nebulosus")))
#
# #Plot
# pd <- position_dodge(0.25)
# limits <- aes(ymax = mean + se, ymin = mean - se)
# TargetFig <- ggplot(target.sp, aes(x=Year, y=mean, group=Zone, shape=Zone, colour = Zone)) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd, colour = "black") + # error bars
#   #geom_smooth(method = "gam", formula = y ~ s(x,k=4), se=F, size = 1) +
#   #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
#   #geom_line(position=pd) +                      # line
#   geom_point(aes(fill = Zone),position=pd, size=5,colour = "black") +             # points
#   #stat_smooth(method = "lm", colour = "black", se = FALSE) +
#   xlab("Survey Year") +
#   ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
#   scale_x_continuous(limits=c(min(target.sp$Year-0.25),max(target.sp$Year+0.25)), breaks=min(target.sp$Year):max(target.sp$Year)) +
#   scale_shape_manual(values=c(21, 22, 24))+
#   scale_fill_manual(values=c('#F7F323',"#99CC33"))+
#   scale_colour_manual(values=c('#F7F323',"#99CC33"))+
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         #axis.title=element_text(face="bold"),
#         #strip.text.x=element_text(size = 12,face="bold"),
#         axis.text.x=element_text(angle=90, vjust=0.4),
#         strip.text.y=element_text(size = 11,face="italic"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.key = element_blank(),
#         legend.position = "bottom",
#         axis.line = element_line(colour = 'black'),
#         axis.text=element_text(size=12,colour = 'black'),
#         panel.border = element_rect(colour = "black"))
#
# #png(filename = "TargetJBMP_1999-2018.png",
# #  width = 800, height = 600, units = "px", pointsize = 6)
# TargetFig+facet_grid(Taxa~Location,scales = "free_y")
#

### ► Theme for plotting ----
Theme1 <- ggplot2::theme_bw() +
  ggplot2::theme( # use theme_get() to see available options
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_blank(),
    # legend.position = "top",
    text = ggplot2::element_text(size = 12),
    strip.text.y = ggplot2::element_text(size = 12, angle = 0),
    axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
    axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    strip.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
  )


