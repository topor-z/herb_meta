help.search("data input")
install.packages("dplyr")
install.packages("vegan")
install.packages("sp")
install.packages("rgeos")
install.packages("geosphere")
install.packages("mapproj")
install.packages("maps")
install.packages("ggmap")
install.packages("raster")
install.packages("fields")
##load packages from library onto workspace
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
library(sp)
library(rgeos)
library(geosphere)
library(mapproj)
library(maps)
library(ggmap)
##import csv into r, define pathway to get there, name it
herb_meta <- read.csv(file = "~/Dropbox/Herbivory_Meta/herb_meta_csv.csv")
rls<- read.csv(file="~/Dropbox/Herbivory_Meta/Reef_Life_Survey.csv")
##check data
str(herb_meta)
head(herb_meta)
head(rls)
d<- herb_meta[,c(1,10,11)]
d<-unique(d)
head(d)
write.table(d, file = "study_coords.txt", row.names = FALSE, sep = " ")
#####beginnings######
## taking Gil et al. out (#16) and Yabsley (52) from the dataframe, != Does not equal, ',' all columns included
herb_vetted <- herb_meta[ which(herb_meta$paper.ID != 16 
                          & herb_meta$paper.ID != 52),]
herb_vetted$paper.ID
head(herb_vetted)

##removing estimates from studies that removed epipiphytes
herb_noepi<- herb_vetted[which(herb_vetted$Epiphites.removed !="yes"),]
summary(herb_noepi)

##only studies using Sargassum
herb_sarg<- herb_vetted[which(herb_vetted$GENUS =="sargassum"),]
summary(herb_sarg)

##only studies on the GBR
herb_GBR<- herb_out[which(herb_out$Ecoregion =="GBR"),]
summary(herb_GBR)
##plotting number of species against %loss/hr   "~" = is a function of ie. herbrate is a func of diversity (y is a func of x)
plot( herb_meta$Standardize_herbrate ~ herb_meta$Diversity, ylab = "% loss/hr", xlab = "number of species")

plot(herb_meta$Standardize_herbrate ~ herb_meta$fam_div, ylab = "% loss/hr", xlab = "number of species")

##using graph with ggplot, limiting the y to 200
##"jitter" adds randomness to points to better see overlapping

qplot(Diversity, Standardize_herbrate, data=herb_vetted, geom="jitter")

qplot(Longitude, Standardize_herbrate, ylim=c(0,200), data=herb_meta, geom="jitter")


plot( herb_vetted$Standardize_herbrate ~ herb_vetted$Diversity, ylab = "% loss/hr", xlab = "number of species")
str(herb_out)
###ggplot
## assigning herb.div the plot     data set, aesthetics, type of graph, line
herb.div <- ggplot(herb_out, aes(x = fam_div, y = Standardize_herbrate, color= Ecoregion)) +
  xlab("Diversity")+
  ylab("Herbivory rate (%/t)")+
  geom_jitter() 
##run the function to make the graph
herb.div
##check on the new dataframe
str(herb_vetted)

## herbivory rate against year published
herb.year<- ggplot(herb_vetted, aes(x= Year.pub, y= Standardize_herbrate, color= Ecoregion)) +
  geom_jitter()+
  xlab("Year Published")+
  ylab("Herbivory Rate (% loss/t)")+
  ggtitle("Herbivory Rate vs. Year Published")
herb.year 

## ecoregion against year published
eco.year<-ggplot(herb_vetted, aes(x= Ecoregion, y= Year.pub)) +
  geom_jitter()
eco.year

##biomass against herbivory rate
herb.biom<-ggplot(herb_vetted, aes(x= standard_biomass.kg.100m2., y= Standardize_herbrate, color = Ecoregion)) +
  geom_point()
herb.biom

##boxplot graph reef zone aginst herb. rate
##x,y labels
##black and white theme
herb.zone<-ggplot(herb_vetted, aes(x= Reef.zone, y= Standardize_herbrate)) +
  geom_boxplot() +
  xlab("Reef Zone")+
  ylab("% loss/hour")+
  theme_bw()
herb.zone

herb.tax<-ggplot(herb_vetted, aes(x= Prey.taxon.coarse, y= Standardize_herbrate, color = Ecoregion)) +
  geom_jitter() +
  xlab("Prey Taxon")+
  ylab("% loss/hour")+
  theme_bw()
herb.tax


##boxplot ecoregion against herb.rate, prey taxon colored 
##attempting to change legend labels, position
herb.eco<-ggplot(herb_vetted, aes(x= Ecoregion, y= Standardize_herbrate)) +
  geom_boxplot() +
  xlab("Ecoregion")+
  ylab("% loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_text("Prey Taxon")) #, labels= c("Brown", "Green", "Mixed", "Red", "Seagrass"))
herb.eco

##herb.eco with epiphytes removed dataset
herb.eco.epi<-ggplot(herb_noepi, aes(x= Ecoregion, y= Standardize_herbrate)) +
  geom_boxplot(aes(color=Prey.taxon.coarse)) +
  xlab("Ecoregion")+
  ylab("% loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")
herb.eco.epi

##overlaying another dataset 'herb_vetted'
herb.div.epi <- ggplot(herb_noepi, aes(x = Diversity, y = Standardize_herbrate)) +
  geom_jitter() +
  geom_jitter(data = herb_vetted, aes(x =Diversity, y = Standardize_herbrate, color= "red", alpha = 0.5))+
  xlab("Diversity")+
  ylab("% loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("With Epiphytes")
herb.div.epi

herb.div<-herb.div+
  ggtitle("All Treatments")
##can you add onto graphs like this or will it create a mess down the road??

##can you overlay graphs to look at change??

##using only sargassum *with location
herb.div.sarg <- ggplot(herb_sarg, aes(x = fam_div, y = Standardize_herbrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Diversity")+
  ylab("% loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("Only Sargassum")
herb.div.sarg
head(herb_GBR)
#only GBR
herb.div.GBR <- ggplot(herb_GBR, aes(x = fam_div, y = Standardize_herbrate, color= Prey.taxon.coarse)) +
  geom_jitter() +
  xlab("Diversity")+
  ylab("% loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("Only GBR")+
  scale_shape(solid = FALSE)
herb.div.GBR 

##how to change the shape of the point---- 'shape = __'


##FIGURE OUT HOW TO AVERAGE HERBIVORY RATE AND COMPARE AGAINST SPECIES
## AVERAGE HERB.RATE/BIODIVERSITY SPECIFIC SITE

##create new df. that only has biodiversity estimates !!need to put NA in ""
biodiv<-herb_out[ which(herb_out$fam_div != "NA"),]
biodiv
summary(biodiv)


##add in unique biodivID within each paper ID
#drop=TRUE gives you list of values not just random numbers

biodivID <- transform(biodiv, BioID = as.numeric(interaction(Diversity, paper.ID, drop=TRUE)))
biodivID
str(biodivID)



##create new df. with the max and average of standard herb.rate

summary(biodivID)
head(biodivID)

##attempting same task using dplyr- average and max herb.rate

herb.max.ave<- biodivID %>% group_by(BioID) %>%summarize(Mean.herb.rate=mean(Standardize_herbrate), 
                                                         Diversity= mean(fam_div), SD=sd(Standardize_herbrate), max.herb.rate=max(Standardize_herbrate)) 
#ave.biomass= mean(standard_biomass.kg.100m2.), ave.abundance=mean(Abund_Standard.indv.100m2.), sdbm=sd(standard_biomass.kg.100m2.), 
#sdabun= sd(standard_biomass.kg.100m2.), na.rm=TRUE))

herb.max.ave
head(herb.max.ave)
summary(herb.max.ave)

ave.herb.diversity<-ggplot(herb.max.ave, aes(x= Diversity, y= Mean.herb.rate)) +
  geom_point() +
  xlab("Diversity")+
  geom_errorbar(aes(ymin=Mean.herb.rate-SD, ymax=Mean.herb.rate+SD)) +
  ylab("Average % loss/hour")+
  geom_smooth(method = "lm")+
  theme_bw()+
  ggtitle("Average Biodiversity against Refined Biodiversity, outlier removed")
ave.herb.diversity

##herbivory max at each site
##using fam.div
max.herb<-ggplot(herb.max.ave, aes(x= Diversity, y= max.herb.rate)) +
  geom_jitter() +
  #geom_smooth(method = "lm") +
  xlab("Diversity")+
  ylab("Maximum % loss/hour")+
  theme_bw()
max.herb

##combine max and average
ave.max.herb.plot<-ggplot(herb.max.ave, aes(x= Diversity, y= Mean.herb.rate, color="Average Herbivory Rate")) +
  geom_point() +
  xlab("Diversity")+
  ylab("% loss/hour")+
  theme_bw()+
  geom_smooth(method="lm")+
  geom_errorbar(aes(ymin=Mean.herb.rate-SD, ymax=Mean.herb.rate+SD)) +
  geom_point(data=herb.max.ave, aes(x= Diversity, y= max.herb.rate, color="Maximum Herbivory Rate"))+
  ggtitle("Average and Maximum Herbivory Rates on Coral Reefs")+
  theme(legend.position = "bottom")+
  labs(color="Rate Measure")

ave.max.herb.plot

##creating a new df. with bioID off of herb_GBR
gbrID<-transform(herb_GBR, BioID=as.numeric(interaction(Diversity,paper.ID, drop = TRUE)))
head(gbrID)

gbr.max.ave<-gbrID %>% group_by(BioID) %>%summarize(ave.herb.rate=mean(Standardize_herbrate), Diversity= mean(fam_div), stdev.herb.rate = sd(Standardize_herbrate),max.herb.rate=max(Standardize_herbrate))
summary(gbr.max.ave)
gbr.max.herb.diversity<-ggplot(gbr.max.ave, aes(x= Diversity, y= max.herb.rate)) +
  geom_jitter() +
  xlab("Diversity")+
  ylab("Maximum % loss/hour")+
  theme_bw()+
  ggtitle("Maximum Herbivory Rate GBR")
gbr.max.herb.diversity

#############AVERAGE HERB.RATE OF JUST GBR#######
#gbr.ave<-gbrID %>% group_by(BioID) %>%summarize(ave.herb.rate=mean(Standardize_herbrate), Diversity= mean(Diversity), stdev.herb.rate = sd(Standardize_herbrate))
head(gbr.ave)

gbr.ave.plot<-ggplot(gbr.max.ave, aes(x= Diversity, y= ave.herb.rate)) +
  geom_point() +
  geom_errorbar(aes(ymin=ave.herb.rate-stdev.herb.rate, ymax=ave.herb.rate+stdev.herb.rate)) +
  xlab("Diversity")+
  ylab("Average % loss/hour")+
  geom_smooth(method = "lm")+
  theme_bw()+
  ggtitle("Herbivory Rate GBR")
gbr.ave.plot

#average overlaid with maximum herbivory rate in GBR
gbr.ave.max<-ggplot(gbr.max.ave, aes(x= Diversity, y= ave.herb.rate, color="Average Herbivory Rate")) +
  geom_point() +
  geom_errorbar(aes(ymin=ave.herb.rate-stdev.herb.rate, ymax=ave.herb.rate+stdev.herb.rate)) +
  geom_point(data=gbr.max.ave, aes(x= Diversity, y= max.herb.rate, color="Maximum Herbivory Rate"))+
  xlab("Diversity")+
  ylab("% loss/hour")+
  geom_smooth(method="lm")+
  theme_bw()+
  ggtitle("Herbivory Rate GBR")+
  theme(legend.position = "bottom")+
  labs(color="Rate Measure")
gbr.ave.max

##attempting ave biomass df
avebiomass<- biodivID[which(biodivID$standard_biomass.kg.100m2. !="NA"),]
head(avebiomass)

meanbm<-avebiomass %>% group_by(BioID) %>%summarize(ave.herb.rate=mean(Standardize_herbrate), Diversity= mean(Diversity), 
                                                    stdev.herb.rate = sd(Standardize_herbrate), ave.bm=mean(standard_biomass.kg.100m2.), 
                                                    sdbm=sd(standard_biomass.kg.100m2.), Eco=unique(Ecoregion))
head(meanbm)

meanbmplot<- ggplot(meanbm, aes(x=ave.bm, y=ave.herb.rate))+
  geom_point()+
xlab("Biomass")+
  ylab("% loss/hour")+
  theme_bw()+
  ggtitle("Ave. Herbivory Rate vs Ave. Biomass")+
  theme(legend.position = "bottom")

meanbmplot

##removing hoey and bellwood 2009
herb_out <- herb_vetted[ which(herb_vetted$paper.ID != 26),]
summary(herb_out$fam_div)
head(herb_out)
class(herb_out)

date.biodiv<-ggplot(herb_vetted, aes(x=Year.pub, y=Diversity, color = Ecoregion))+
  geom_jitter()+
  xlab("Year Published")+
  ylab("Diversity")+
  theme_bw()+
  ggtitle("Diversity against year published")

date.biodiv

eco.div<- ggplot(herb_out, aes(x=Ecoregion, y=fam_div))+
  #geom_point()+
  geom_boxplot()+
  xlab("Ecoregion")+
  ylab("Diversity")+
  theme_bw()+
  ggtitle("Diversity against Ecoregion")
eco.div

#run a linear model on the data
m1<-lm(ave.herb.rate ~ Diversity, data= gbr.max.ave)
  str(gbr.max.ave)
  summary(m1)
  m2<-lm(Mean.herb.rate ~ Diversity, data= herb.max.ave)
summary(m2)
  
str(herb.max.ave)
  #using fam_div
  ave.herb.diversity<-ggplot(herb.max.ave, aes(x= Diversity, y= Mean.herb.rate)) +
    geom_point() +
    xlab("Diversity")+
    geom_errorbar(aes(ymin=Mean.herb.rate-SD, ymax=Mean.herb.rate+SD)) +
    ylab("Average % loss/hour")+
    geom_smooth(method = "lm")+
    theme_bw()+
    ggtitle("Average Biodiversity against Refined Biodiversity, outlier removed")
  ave.herb.diversity
  
  
  
####sorting RLS data by herbivorous fish family####
  
  
rlsfam<- as.data.frame(rls[which(rls$Family== c("Acanthuridae","Scaridae","Siganidae", "Kyphosidae")),])  

#pick out families needed  
rlsAcan<- as.data.frame(rls[grep("Acanthuridae", rls$Family),])  
rlsScar<- as.data.frame(rls[grep("Scaridae", rls$Family),])
rlsSig<-  as.data.frame(rls[grep("Siganidae", rls$Family),]) 
rlsKyp<- as.data.frame(rls[grep("Kyphosidae", rls$Family),])
rlsEph<- as.data.frame(rls[grep("Ephippidae", rls$Family),])
#combine df together and cut out the things we dont need
  
rlsherb<- as.data.frame(droplevels(rbind(rlsAcan, rlsScar, rlsSig, rlsKyp, rlsEph)))
 summary(rlsherb)
class(rlsherb)
 
rls[1:100,]
unique(rlsherb$Realm)


########biodiversity by realm################
rls.realm <- group_by(rlsherb, Realm)
rlsdiv.realm <- as.data.frame(summarise(rls.realm, Taxon = n_distinct(Taxon)))
head(rlsdiv.realm)
rlsdiv.realm

##########Realm biodiv graph###########################################
coordinates(rlsherb)<-c("SiteLong", "SiteLat")
coordrls<-coordinates(rlsherb)
plot(coordrls)
class(coordrls) #matrix
class(rlsherb )

coordplot<-ggplot(rlsherb, aes(x=SiteLong, y=SiteLat, color = Realm))+
  geom_jitter()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  ggtitle("RLS sites")+
  theme(legend.position = "bottom")
coordplot

###combining both data sets 

herb_out$assign <- "Meta"
herb_out$Realm<- "NA"
herb_out$SiteCode<- "NA"
head(herb_out)
herb_out$newcolumn<- NULL

head(rlsherb)
# Rename a column in R
colnames(herb_out)[colnames(herb_out)=="Latitude"]<- "SiteLat"
colnames(herb_out)[colnames(herb_out)=="Longitude"]<- "SiteLong"

rlsherb$assign <- "RLS"
rlsherb$newcolumn<- NULL
head(rlsherb)
##create new df for both sets for combination 
dfnew1 <- herb_out[,c("SiteLat", "SiteLong", "Realm", "assign", "SiteCode")]
dfnew2 <- rlsherb[,c("SiteLat", "SiteLong", "Realm", "assign", "SiteCode")]
head(dfnew2)
combcoord<-rbind(dfnew1,dfnew2)


###plot both on top of each other
coordplot2<-ggplot(combcoord, aes(x=SiteLong, y=SiteLat, color= Realm, shape= assign))+
  geom_point()+
  #geom_point(data=herb_out, aes(x=SiteLong, y=SiteLat), colour="yellow", size=5)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_dark()+
  ggtitle("overlayed points")+
  #geom_text(data=herb_out, aes(label=paper.ID, color="black"),hjust=0, vjust=0)+
  theme(legend.position = "bottom")
  #ggmap(map)
coordplot2


coordplot3<-ggplot(combcoord, aes(x=SiteLong, y=SiteLat, color = Realm, shape=assign))+
  geom_jitter()+
  geom_point(data=herb_out, aes(x=SiteLong, y=SiteLat), colour="yellow", size=2)+
  #geom_text(data = combcoord, aes(label = SiteCode, color="black"), hjust = 0)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  ggtitle("RLS sites")+
  theme(legend.position = "bottom")

coordplot3

#adding a map to the background
library(ggmap)
library(mapproj)
map <- get_map(location = 'World')
ggmap(map)

mp <- NULL
mapWorld <- borders("world", colour="gray50") # create a layer of borders
mp <- coordplot3+   mapWorld
mp

dev.off()


head(herb_out)
head(rlsherb)

##NewRealmDataSet#########################################################

#use merge to combine the data sets
#"Taxon"= regional diversity
reg.div<- as.data.frame(merge(herb_vetted, rlsdiv.realm, by="Realm", all=TRUE))
str(reg.div)

rls.herb.div <- ggplot(reg.div, aes(x = Taxon, y = Standardize_herbrate, color= Realm)) +
  geom_boxplot() +
  xlab("Diversity")+
  ylab("% loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("RLS Regional")
rls.herb.div

#using average
rls.max.ave<- reg.div %>% group_by(paper.ID) %>%summarize(Mean.herb.rate=mean(Standardize_herbrate), 
                                                         Taxon= mean(Taxon), SD=sd(Standardize_herbrate), 
                                                         max.herb.rate=max(Standardize_herbrate)) 
head(rls.max.ave)

rls.aveherb.div <- ggplot(rls.max.ave, aes(group= Taxon, x = Taxon, y = Mean.herb.rate)) +
  geom_boxplot() +
  #geom_errorbar(aes(ymin=Mean.herb.rate-SD, ymax=Mean.herb.rate+SD)) +
  xlab("Diversity")+
  ylab("Average % loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("RLS Regional Average Herb.rate")
rls.aveherb.div

#using max
rls.maxherb.div <- ggplot(rls.max.ave, aes(group= Taxon, x = Taxon, y = max.herb.rate)) +
  geom_boxplot() +
  xlab("Diversity")+
  ylab("Maximum % loss/hour")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("RLS Regional Maximum Herb.rate")
rls.maxherb.div


#
m1 <- lm(Standardize_herbrate ~ Taxon, data = reg.div)
summary(m1)

head(rlsherb)
##average abundance of rls

rls.abun<-rlsherb %>% group_by(Realm, SiteCode) %>%summarize(abundance=sum(Total))
rls.abun

reg.ab<- as.data.frame(merge(rls.abun, rlsdiv.realm, by="Realm", all=TRUE))
head(reg.ab)

rls.ave.ab<-reg.ab %>% group_by(Realm) %>%summarize(ave.abundance=mean(abundance), stdev=sd(abundance), Taxon=mean(Taxon))
rls.ave.ab

ave.ab.rls <- ggplot(rls.ave.ab, aes(x = Taxon, y = ave.abundance, color= Realm)) +
  geom_point() +
  xlab("Diversity")+
  ylab("Average Abundance")+
  geom_errorbar(aes(ymin=ave.abundance-stdev, ymax=ave.abundance+stdev)) +
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("RLS Regional Ave. Abundance")
ave.ab.rls


dev.off()




#SJB###########
library(raster)
###get site specific estimates of diversity and abundance 
str(rlsherb)
head(rlsherb)
###round lat long
rlsherb$SiteLat <- round(rlsherb$SiteLat, 1)
rlsherb$SiteLong <- round(rlsherb$SiteLong, 1)
head(rlsherb)
str(rls.site)
###get unique estimates
#names(rlsherb)[names(rlsherb) == 'Taxon'] <- 'new.var.name'
rlsherb$UniqueLatLong <- interaction(rlsherb$SiteLat, rlsherb$SiteLong)
rls.site <- group_by(rlsherb, UniqueLatLong)
rlsdiv.site <- as.data.frame(summarise(rls.site, SiteLong = mean(SiteLong), SiteLat = mean(SiteLat), Taxon.number = n_distinct(Taxon), 
                                       Abundance = mean(Total)))
rlsdiv.site[1:200,]
str(rlsdiv.site)
###get trimmed dataset with essential info from RLS including coordinates
str(rlsherb)
rls.coords <- as.data.frame(rlsherb[c(4:6,19:22)])
rls.coords
###get only unique values 
rls.coords.unique <- as.data.frame(unique(rls.coords))
rls.coords
str(rls.coords.unique)

###merge into site based df
rls.site.coord <- as.data.frame(merge(rlsdiv.site, rls.coords.unique, by = "UniqueLatLong", all.x = TRUE))
str(rls.site.coord)
rls.site.coord

##sorting RLS data by top 3 fish species
str(rls)



#pick out sp needed  
rlsnu<- as.data.frame(rls[grep("Naso unicornis", rls$Taxon),])  
rlskv<- as.data.frame(rls[grep("Kyphosus vaigiensis", rls$Taxon),])
rlssd<-  as.data.frame(rls[grep("Siganus doliatus", rls$Taxon),]) 
rlsac<-  as.data.frame(rls[grep("Acanthurus chirurgus", rls$Taxon),]) 
rlsne<-  as.data.frame(rls[grep("Naso elegans", rls$Taxon),]) 
rlsnl<-  as.data.frame(rls[grep("Naso lituratus", rls$Taxon),]) 
rlssc<-  as.data.frame(rls[grep("Siganus canaliculatus", rls$Taxon),]) 
rlssj<-  as.data.frame(rls[grep("Siganus javus", rls$Taxon),]) 
rlssp<-  as.data.frame(rls[grep("Siganus puelloides", rls$Taxon),]) 
rlsaco<-  as.data.frame(rls[grep("Acanthurus coeruleus", rls$Taxon),]) 
rlscc<-  as.data.frame(rls[grep("Calotomus carolinus", rls$Taxon),]) 
rlscm<-  as.data.frame(rls[grep("Chlorurus microrhinos", rls$Taxon),]) 
rlslv<-  as.data.frame(rls[grep("Leptoscarus vaigiensis", rls$Taxon),]) 
rlssm<-  as.data.frame(rls[grep("Scarus margaritiferus", rls$Taxon),]) 
rlszd<-  as.data.frame(rls[grep("Zebrasoma desjardinii", rls$Taxon),]) 
rlsan<-  as.data.frame(rls[grep("Acanthurus nigrofuscus", rls$Taxon),]) 
rlsbm<-  as.data.frame(rls[grep("Bolbometopon muricatum", rls$Taxon),]) 
rlscs<-  as.data.frame(rls[grep("Chlorurus sordidus", rls$Taxon),]) 
rlskb<-  as.data.frame(rls[grep("Kyphosus bigibbus", rls$Taxon),]) 
rlssr<-  as.data.frame(rls[grep("Scarus rivulatus", rls$Taxon),]) 
rlssa<-  as.data.frame(rls[grep("Sparisoma aurofrenatum", rls$Taxon),]) 
rlsSR<-  as.data.frame(rls[grep("Sparisoma rubripinne", rls$Taxon),]) 
rlssv<-  as.data.frame(rls[grep("Sparisoma viride", rls$Taxon),]) 
rlsab<-  as.data.frame(rls[grep("Acanthurus bahianus", rls$Taxon),]) 
rlsSC<-  as.data.frame(rls[grep("Sparisoma chrysopterum", rls$Taxon),]) 
rlsss<-  as.data.frame(rls[grep("Scarus sordidas", rls$Taxon),]) 



#combine df together and cut out the things we dont need


rlssp<- as.data.frame(droplevels(rbind(rlsnu, rlskv, rlssd, rlsac,
                                       rlsne,
                                       rlsnl,
                                       rlssc,
                                       rlssj,
                                       rlssp,
                                       rlsaco,
                                       rlscc,
                                       rlscm,
                                       rlslv,
                                       rlssm,
                                       rlszd,
                                       rlsan,
                                       rlsbm,
                                       rlscs,
                                       rlskb,
                                       rlssr, 
                                       rlssa,
                                       rlssv,
                                       rlsSR,
                                       rlssv,
                                       rlsab,
                                       rlsSC,
                                       rlsss)))
head(rlssp)
str(rlssp)

##using previous code to create unique ids based off of lat. long. so that abundance can merge with the data set 


rlssp$SiteLat <- round(rlssp$SiteLat, 1)
rlssp$SiteLong <- round(rlssp$SiteLong, 1)
head(rlssp)
str(rls.site)
###get unique estimates
rlssp$UniqueLatLong <- interaction(rlssp$SiteLat, rlssp$SiteLong)
rlssp.site <- group_by(rlssp, UniqueLatLong)
rlsspdiv.site <- as.data.frame(summarise(rlssp.site, SpeciesAb= sum(Total)))

head(rlsspdiv.site)
rls.site.coord$SpeciesAb<- NULL
###merge into site based df
#rls.site.coord <- as.data.frame(merge(rlsdiv.site, rls.coords.unique, by = "UniqueLatLong", all.x = TRUE))
rls.site.coord1<- as.data.frame(merge(rls.site.coord, rlsspdiv.site, by = "UniqueLatLong", all.x = TRUE))
rls.site.coord1

#### !!!!!!!!!!!  resume here


###get coordinates from lit review
str(herb_vetted)
herb_vetted$UniqueSiteID <- interaction(herb_vetted$unique.ID, herb_vetted$Site.name)
str(herb_vetted)
head(herb_vetted)
lit.coord.factors <- as.data.frame(herb_vetted[c(1,2,6:11,19,23,25,58)])
lit.coord.factors.unique <- as.data.frame(unique(lit.coord.factors))
summary(lit.coord.factors.unique)
lit.coord.factors.unique$newID <- interaction(lit.coord.factors.unique$UniqueSiteID, lit.coord.factors.unique$micro.scale.habitat, 
                                              lit.coord.factors.unique$Prey.taxon.coarse, lit.coord.factors.unique$Duration_Assay.hrs.)
lit.coord.factor.unique <- na.omit(lit.coord.factors.unique)

herb_vetted$newID <- interaction(herb_vetted$UniqueSiteID, herb_vetted$micro.scale.habitat, 
                                 herb_vetted$Prey.taxon.coarse, herb_vetted$Duration_Assay.hrs.)

##winnowing the herb.vetted data set ie. lit review data
str(herb_vetted)
lit.coord.data <- as.data.frame(herb_vetted[c(49:54, 58, 59)])
str(lit.coord.data)
lit.coord.data.site <- group_by(lit.coord.data, newID)
lit.coord.data.site.ave.max <- as.data.frame(summarise(lit.coord.data.site, 
                                                       Herb.rate = mean(Standardize_herbrate),
                                                       Max.herb.rate = max(Standardize_herbrate),
                                                       Div.all = mean(Diversity), 
                                                       Abun = mean(Abund_Standard.indv.100m2.),
                                                       Biomass = mean(standard_biomass.kg.100m2.),
                                                       Div.herbs = mean(fam_div),
                                                       Max.day.herb = max(day.adj),
                                                       Day.herb = mean(day.adj)))
str(lit.coord.data.site.ave.max)

lit.coord.all <- as.data.frame(merge(lit.coord.factors.unique, lit.coord.data.site.ave.max, by = "newID"))
(lit.coord.all)

###get fields package for distance calculation
library(fields)

###calculate distances 
distMatrix <- rdist.earth(rls.site.coord[,c('SiteLong','SiteLat')], 
                          lit.coord.all[,c('Longitude','Latitude')], miles = FALSE)
rownames(distMatrix) <- rls.site.coord$UniqueLatLong
distMatrix


###find minimum distances and check whether each location has a reasonable nearest neighbor
##apply version -- minimum value for each column (lit review sites w unique GPS coords)
apply(distMatrix, 2, min)
###check w lit.coord.unique dataset 
lit.coord.all

min.rows <- data.frame(row = numeric(0))
for (i in 1:ncol(distMatrix))
{
  holder <- as.data.frame(which(distMatrix[,i] == min(distMatrix[,i]), arr.ind=TRUE))
  holder$names <- as.factor(rownames(holder))
  min.rows[i,1] <- holder[1,1]
  min.rows$name[i] <- as.character(holder$names[1])
}

###look at df
min.rows
###rename name column to match RLS
colnames(min.rows) <- c("Row", "UniqueLatLong")
###paste in data from lit review
min.rows.litdata <- cbind(min.rows, lit.coord.all) 
min.rows.litdata
###get diversity data fgor nearest neighbor sites from RLS
rls.site.coord
table(min.rows.litdata$Ecoregion)
min.rows.litdata$Ecoregion
min.rows.litdata$Ocean <- ifelse(min.rows.litdata$Ecoregion == "GBR", "Pacific", 
                                 ifelse(min.rows.litdata$Ecoregion == "Caribbean", "Atlantic",
                                        ifelse(min.rows.litdata$Ecoregion == "Indian Ocean", "Indian",
                                               ifelse(min.rows.litdata$Ecoregion == "Western Australia", "Indian",
                                                      ifelse(min.rows.litdata$Ecoregion == "Pacific", "Pacific",
                                                             ifelse(min.rows.litdata$Ecoregion == "South Atlantic", "Atlantic",
                                                                    ifelse(min.rows.litdata$Ecoregion == "Africa", "Indian",
                                                                           ifelse(min.rows.litdata$Ecoregion == "South Pacific", "Pacific","NA")))))))) 
(min.rows.litdata$Ocean)
###get data ready for plot
near.neigh.fish <- as.data.frame(merge(min.rows.litdata, rls.site.coord, by = "UniqueLatLong", all.x = TRUE))
summary(near.neigh.fish)
near.neigh.fish <- near.neigh.fish[order(near.neigh.fish$paper.ID),]
near.neigh.fish <- transform(near.neigh.fish, Div.merged = ifelse(!is.na(Div.herbs), Div.herbs, Taxon.y))
near.neigh.fish

summary(near.neigh.fish$SpeciesAb)
# near.neigh.fish.sarg <- as.data.frame(near.neigh.fish[which(near.neigh.fish$Prey.taxon.coarse == "BROWN"),])
near.neigh.fish.sarg <- as.data.frame(near.neigh.fish[which(near.neigh.fish$Div.merged < 30),])
near.neigh.fish.sarg

###plot data
divherb <- ggplot(near.neigh.fish, aes(x = Div.merged, y = Day.herb)) +
  geom_point(aes(size = 3)) +
  geom_smooth(method = "lm") +
  geom_text(aes(label = paper.ID, hjust = 2))
divherb

###edit data to only include data with less than 8h exposure####
min.rows.litdata1 <- as.data.frame(min.rows.litdata[which(min.rows.litdata$Duration_Assay.hrs. < 8),])
min.rows.litdata1
table(min.rows.litdata1$Ocean)
###get data ready for plot
near.neigh.fish1 <- as.data.frame(merge(min.rows.litdata1, rls.site.coord, by = "UniqueLatLong", all.x = TRUE))
near.neigh.fish1
near.neigh.fish1 <- near.neigh.fish1[order(near.neigh.fish1$paper.ID),]
near.neigh.fish1 <- transform(near.neigh.fish1, Div.merged = ifelse(!is.na(Div.herbs), Div.herbs, Taxon.y))
head(rls.site.coord)

###plot data
divherb1 <- ggplot(near.neigh.fish1, aes(x = Div.merged, y = Day.herb, color = Prey.taxon.coarse)) +
  geom_point(aes(size = 3)) +
  geom_smooth(method = "lm") +
  geom_text(aes(label = paper.ID, hjust = 2))
divherb1

##plot with ave. abundance of just top 3 sp. 
table(near.neigh.fish1$Ocean)
#the ocean is not matching the data
divherbt3 <- ggplot(near.neigh.fish, aes(x = SpeciesAb, y = Day.herb)) +
  geom_point(aes(color = Ocean))+
  geom_smooth(method = "lm")
divherbt3


###only brown algae###
near.neigh.fish.brown <- as.data.frame(near.neigh.fish1[which(near.neigh.fish1$Prey.taxon.coarse == "BROWN"),])
near.neigh.fish.brown
divherb.brown <- ggplot(near.neigh.fish.brown, aes(x = Div.merged, y = Day.herb, color = Ocean)) +
  geom_point(aes(size = 3)) +
  geom_smooth(method = "lm") +
  geom_text(aes(label = paper.ID, hjust = 2))
divherb.brown


##maximum 
divherbmax <- ggplot(near.neigh.fish1, aes(x = Div.merged, y = Max.day.herb, color = Prey.taxon.coarse)) +
  geom_point(aes(size = 3)) +
  geom_smooth(method = "lm") +
  geom_text(aes(label = paper.ID, hjust = 2))
divherbmax

#only brown algae maximum
divherb.brown.max <- ggplot(near.neigh.fish.brown, aes(x = Div.merged, y = Max.day.herb, color = Ocean)) +
  geom_point(aes(size = 3)) +
  geom_smooth(method = "lm") +
  geom_text(aes(label = paper.ID, hjust = 2))
divherb.brown.max

#models#######
head(near.neigh.fish.brown)
near.neigh.fish.brown$latabs <- abs(near.neigh.fish.brown$Latitude)

model1<- lm(Max.day.herb ~ Div.merged + Ocean + Hemi + latabs, data=near.neigh.fish.brown)
summary(model1)
plot(model1)

summary(lm(Latitude ~ Hemi, data = near.neigh.fish.brown))

model2<-lm(Max.day.herb ~ Div.merged + Ocean + Hemi + Prey.taxon.coarse, data = near.neigh.fish1)
summary(model2)
#ranked species#########
summary(herb_vetted)
table(herb_vetted$FuncID1)
table(herb_vetted$FuncID2)
table(herb_vetted$FuncID3)
herb_vetted
class(herb_vetted$FuncID3)
 






