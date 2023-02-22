#SARE on-farm data analysis for Kimber field day 2022
library(googlesheets4)
gs4_deauth()
dat<-read_sheet("https://docs.google.com/spreadsheets/d/1oV1K0cfu2KOsl_gsPard1xNV032sn7pNXudlgtHTRFQ/edit#gid=0")
#subset data to keep just a few important yield columns
twd<-getwd()
keepcols<-names(dat)[c(1:7,18,23,28,33,38,43)]
library(tidyverse)
library(nlme)
library(emmeans)
library(multcomp)
sdat<-dat %>% 
  dplyr::select(all_of(keepcols))
names(sdat)[c(3,8:13)]<-c("farm", "grain","straw","alfalfa","RC","WC","weeds")
sdat$fTrt<-factor(sdat$Treatment, labels=c("Monoculture", "Alfalfa", "Mixture"))
sdat$fTrt<-factor(sdat$fTrt, levels=c("Monoculture", "Alfalfa", "Mixture"))
sdat$farm<-plyr::revalue(sdat$farm, c("Goplen-Canby"="Goplen", "Kimber"="Kimber"))
#12/30/2022 - need to thresh Goplen 2022 grain, find soil data - calculate changes in 
#soil parameters by treatment.


#Goplen quadrat 2020 = 30"x30", row spacing = 6"
#Kimber quadrat 2020 = 30" x 30", row spacing = 6" mentions 4 rows per quad
#1 acre is 2504.5" per side
#1 pound is 453.4 grams
rowin.acre<-(2504.5/6)*2504.5
sdat$grainyld<-(sdat$grain/453.4)*(1/120)*rowin.acre
sdat$strawyld<-(sdat$straw/453.4)*(43560/6.25)
sdat$alfalfayld<-(sdat$alfalfa/453.4)*(43560/6.25)
sdat$RCyld<-(sdat$RC/453.4)*(43560/6.25)
sdat$WCyld<-(sdat$WC/453.4)*(43560/6.25)
sdat$weedyld<-(sdat$weeds/453.4)*(43560/6.25)
library(Rmisc)
library(clipr)
#saving plot theme ----
mytheme<-theme(panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),
               panel.background=element_blank(),
               strip.text=element_text(size=12),
               strip.background = element_rect(color="white"),
               #panel.border=element_rect(fill=NA),
               #legend.title=element_blank(),
               axis.line = element_line(color='black'),
               #axis.text.x=element_blank(),
               axis.title.x=element_blank(),
               axis.text.x=element_text(size=10, color='black', angle=45,vjust=1, hjust=1),
               axis.text.y=element_text(size=10, color='black'))
mtheme<-theme(rect = element_rect(fill = "transparent"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.title=element_text(size=16, color="black"),
              panel.border=element_blank(),
              strip.background = element_rect(color="black", fill="transparent"),
              strip.text=element_text(size=16, color="black"),
              panel.background=element_rect(color="black", fill="transparent", size=2),
              axis.text.x=element_text(size=16, color="black", hjust=.95, angle=45),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=16, color="black"),
              axis.title.y = element_blank())
#plots----
write_clip(summarySE(sdat, "grainyld", c("Year", "farm","fTrt"), na.rm=T))
ggplot(summarySE(sdat, "grainyld", c("Year", "farm","fTrt"), na.rm=T),
       aes(y=grainyld, x=fTrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=grainyld+se, ymin=grainyld-se), position=position_dodge(0.65), width=0.40)+
  facet_grid(Year~farm)+
  ylab("Kernza grain yield (lbs. per acre)")+
  ggtitle("On Farm Grain")+mtheme
ggsave("Sare_OF_grain.png", width=7, height=7, units="in", path=paste0(twd,"/figures"))

write_clip(summarySE(sdat, "strawyld", c("Year", "farm","fTrt"), na.rm=T))
ggplot(summarySE(sdat, "strawyld", c("Year", "farm","fTrt"), na.rm=T),
       aes(y=strawyld, x=fTrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=strawyld+se, ymin=strawyld-se), position=position_dodge(0.65), width=0.60)+
  facet_grid(Year~farm)+
  ylab("IWG straw yield (lbs. per acre)")+
  ggtitle("On Farm Straw")+mtheme
ggsave("Sare_OF_straw.png", width=7, height=7, units="in", path=paste0(twd,"/figures"))

#Weed biomass ----
write_clip(summarySE(subset(sdat, farm=="Kimber"), "weedyld", c("Year", "fTrt"), na.rm=T))
ggplot(summarySE(subset(sdat, farm=="Kimber"), "weedyld", c("Year", "fTrt"), na.rm=T),
       aes(y=weedyld, x=fTrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=weedyld+se, ymin=weedyld-se), position=position_dodge(0.65), width=0.60)+
  facet_grid(~Year)+
  ylab("Weed biomass (lbs. per acre)")+
  ggtitle("On Farm Weed Biomass")+mtheme
ggsave("Sare_OF_weeds.png", width=7, height=5, units="in", path=paste0(twd,"/figures"))

#Legume biomass ----
sdat$legyld<-rowSums(data.frame(sdat$alfalfayld,sdat$RCyld,sdat$WCyld), na.rm=T)
write_clip(summarySE(sdat, "legyld", c("Year", "farm","fTrt"), na.rm=T))
ggplot(summarySE(sdat, "legyld", c("Year", "farm","fTrt"), na.rm=T),
       aes(y=legyld, x=fTrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=legyld+se, ymin=legyld-se), position=position_dodge(0.65), width=0.60)+
  facet_grid(Year~farm)+
  ylab("IWG legume yield (lbs. per acre)")+
  ggtitle("On Farm Legumes")+mtheme
ggsave("Sare_OF_legumes2.png", width=7, height=7, units="in", path=paste0(twd,"/figures"))

#summing up biomass----
sdat$tyld<-rowSums(data.frame(sdat$strawyld,sdat$alfalfayld,sdat$RCyld,sdat$WCyld), na.rm=T)
write_clip(summarySE(sdat, "tyld", c("Year", "farm","fTrt"), na.rm=T))
ggplot(summarySE(sdat, "tyld", c("Year", "farm","fTrt"), na.rm=T),
       aes(y=tyld, x=fTrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=tyld+se, ymin=tyld-se), position=position_dodge(0.65), width=0.60)+
  facet_grid(Year~farm)+
  ylab("Total yield (lbs. per acre)")+
  ggtitle("On Farm Total Biomass")+mtheme
ggsave("Sare_OF_biomass.png", width=7, height=7, units="in", path=paste0(twd,"/figures"))


#Stats ----
sdat$Year<-as.factor(sdat$Year)
grainyld_mod<-lme(grainyld~fTrt*Year*farm, random=~1|Point, data=sdat)
anova(grainyld_mod)
cld(emmeans(grainyld_mod, ~fTrt|Year))
write_clip(cld(emmeans(grainyld_mod, ~fTrt|Year)))

strawyld_mod<-lme(strawyld~fTrt*Year*farm, random=~1|Point, data=sdat, na.action=na.omit)
anova(strawyld_mod)
cld(emmeans(strawyld_mod, ~fTrt))
write_clip(cld(emmeans(strawyld_mod, ~fTrt)))

legld_mod<-lme(legyld~fTrt*Year*farm, random=~1|Point, data=subset(sdat, fTrt!="Monoculture"), 
               na.action=na.omit)
anova(legld_mod)

tyld_mod<-lme(tyld~fTrt*Year*farm, random=~1|Point, data=sdat)
anova(tyld_mod)

#Just 2022
anova(lme(tyld~fTrt*farm, random=~1|Point, data=subset(sdat, Year=="2022")))
summary(lme(tyld~fTrt*farm, random=~1|Point, data=subset(sdat, Year=="2022")))

weedyld_mod<-lme(weedyld~fTrt*Year, random=~1|Point, data=subset(sdat, farm="Kimber"),
                 na.action=na.omit)
anova(weedyld_mod)
summary(weedyld_mod)
cld(emmeans(weedyld_mod, ~fTrt))
write_clip(cld(emmeans(weedyld_mod, ~fTrt)))
#Proportions ----
sdat$alfperc<-sdat$alfalfayld/sdat$tyld
sdat$RCperc<-sdat$RCyld/sdat$tyld
sdat$WCperc<-sdat$WCyld/sdat$tyld
sdat$Kperc<-sdat$strawyld/sdat$tyld
sdat2<- sdat %>% 
  pivot_longer(c("alfperc", "RCperc","WCperc", "Kperc"), names_to = "Plant", values_to = "Proportion")

ggplot(summarySE(sdat2, "Proportion", c("Year", "farm","fTrt", "Plant"), na.rm=T),
       aes(y=Proportion, x=fTrt, fill=Plant))+
  geom_bar(stat="identity", width=0.60)+
  #geom_errorbar(aes(ymax=Proportion+se, ymin=Proportion-se), position=position_dodge(0.65), width=0.60)+
  facet_grid(Year~farm)+
  ylab("IWG legume yield (lbs. per acre)")+
  coord_cartesian(ylim=c(0, 1))+
  ggtitle("On Farm Legumes")+mtheme
ggsave("Sare_OF_percentages.png", width=7, height=7, units="in", path=paste0(twd,"/figures"))

#Soils ----
soildat<-read_sheet("https://docs.google.com/spreadsheets/d/1oV1K0cfu2KOsl_gsPard1xNV032sn7pNXudlgtHTRFQ/edit#gid=1096911605",
                    sheet="WideSoil")
soildat2<- soildat %>% 
  pivot_longer(c("pH_dif":"C/N_dif"), names_to = "SoilComponent", values_to = "Change")
soildat3<- soildat %>%
  pivot_longer(c("OM_dif", "C_dif", "N_dif", "C/N_dif"), names_to = "SoilComponent", values_to = "Change")
soildat3$fTrt<-factor(soildat3$Trt, labels=c("Monoculture", "Alfalfa", "Mixture"))
soildat3$fTrt<-factor(soildat3$fTrt, levels=c("Monoculture", "Alfalfa", "Mixture"))
soildat3$farm<-factor(soildat3$Location, labels=c("Goplen", "Kimber"))
pps = position_dodge(width = .9)

ggplot(summarySE(soildat3, "Change", c("farm", "SoilComponent", "fTrt"), na.rm = T),
       aes(y=Change, x=fTrt))+
  geom_point(stat="identity", position=pps, size=3)+
  geom_errorbar(aes(ymax=Change+ci, ymin=Change-ci), position=pps, width=0.2)+
  geom_hline(yintercept=0, color="red")+
  facet_grid(farm~SoilComponent)+
  #coord_cartesian(ylim=c(-0.5,0.8))+
  ggtitle("Change in soil parameters")+mtheme
ggsave("Sare_OF_SoilChange.png", width=9, height=7, units="in", path=paste0(twd,"/figures"))

#alf kernza split
#group_by is not working here
sdat %>% 
         filter(Treatment=="Kernza + Alfalfa") %>% 
         mutate(alf.perc=alfalfayld/tyld,
                k.perc=strawyld/tyld) %>% 
         group_by(farm) %>% 
         summarize(m.alf=mean(alf.perc, na.rm=T),
                   m.k=mean(k.perc, na.rm=T),
                   se.alf=sd(alf.perc, na.rm=T),
                   se.k=sd(k.perc, na.rm=T))
sdat %>% 
  group_by(farm) %>%
  summarize(mean(grainyld, na.rm=T))

#Read TLI data in
library(readxl)
tliofdat<-read_excel(paste0(twd,"/data/TLI-SARE-OF_jj.xlsx"),
                    sheet="TLI_OFL")
str(tliofdat)
tliofdat<-tliofdat %>% 
  mutate(ftrt=as.factor(trt),
         fyear=as.factor(year),
         sgrain=grain*0.89,
         sstraw=straw*0.89)
#grain
ggplot(summarySE(tliofdat, "sgrain", c("fyear", "ftrt"), na.rm=T),
       aes(y=sgrain, x=ftrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=sgrain+se, ymin=sgrain-se), position=position_dodge(0.65), width=0.40)+
  facet_grid(rows=vars(fyear))+
  ylab("Kernza grain yield (lbs. per acre)")+
  ggtitle("Kansas On Farm Grain")+mtheme+
  theme(axis.title.y=element_text(size=16, color="black"))
ggsave("Sare_TIL_OF_grain.png", width=6, height=6, units="in", path=paste0(twd,"/figures"))
#straw
ggplot(summarySE(tliofdat, "sstraw", c("fyear", "ftrt"), na.rm=T),
       aes(y=sstraw, x=ftrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=sstraw+se, ymin=sstraw-se), position=position_dodge(0.65), width=0.40)+
  facet_grid(rows=vars(fyear))+
  ylab("Kernza straw yield (lbs. per acre)")+
  ggtitle("Kansas On Farm Straw")+mtheme+
  theme(axis.title.y=element_text(size=16, color="black"))
ggsave("Sare_TIL_OF_straw.png", width=6, height=6, units="in", path=paste0(twd,"/figures"))

#Read in WI data
wiofdat<-read_excel(paste0(twd,"/data/TLI-SARE-OF_jj.xlsx"),
                    sheet="WI_OFL")
str(wiofdat)
wiofdat<-wiofdat %>% 
  mutate(ftrt=as.factor(trt),
         fpart=factor(part, labels=c("Alfalfa", "Grain", "IWG straw", "White clover", "Weeds")),
         fyear=as.factor(year),
         sbiomass=biomass*0.89,
         sse=se*0.89)
ggplot(subset(wiofdat, part=="grain"),
       aes(y=biomass, x=ftrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=sbiomass+sse, ymin=sbiomass-sse), position=position_dodge(0.65), width=0.40)+
  facet_grid(rows=vars(fyear))+
  ylab("Kernza grain yield (lbs. per acre)")+
  ggtitle("Wisconsin On Farm Grain")+mtheme+
  theme(axis.title.y=element_text(size=16, color="black"))
ggsave("Sare_WI_OF_grain.png", width=6, height=6, units="in", path=paste0(twd,"/figures"))

#Straw
ggplot(subset(wiofdat, part=="straw"),
       aes(y=biomass, x=ftrt))+
  geom_bar(stat="identity", position=position_dodge(0.65), width=0.60)+
  geom_errorbar(aes(ymax=sbiomass+sse, ymin=sbiomass-sse), position=position_dodge(0.65), width=0.40)+
  facet_grid(rows=vars(fyear))+
  ylab("Kernza straw yield (lbs. per acre)")+
  ggtitle("Wisconsin On Farm Straw")+mtheme+
  theme(axis.title.y=element_text(size=16, color="black"))
ggsave("Sare_WI_OF_straw.png", width=6, height=6, units="in", path=paste0(twd,"/figures"))

ggplot(subset(wiofdat, part!="grain"),
       aes(y=biomass, x=ftrt, fill=fpart))+
  geom_bar(stat="identity", width=0.60)+
  #geom_errorbar(aes(ymax=Proportion+se, ymin=Proportion-se), position=position_dodge(0.65), width=0.60)+
  #facet_grid(Year~farm)+
  ylab("Biomass yield (lbs. per acre)")+
  #coord_cartesian(ylim=c(0, 1))+
  ggtitle("Wisconsin On Farm Biomass")+mtheme+
  theme(axis.title.y=element_text(size=16, color="black"))
ggsave("Sare_WI_OF_biomassprop.png", width=6, height=6, units="in", path=paste0(twd,"/figures"))

