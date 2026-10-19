# Run from the project root. Input directories are configurable.
yft_full_grid <- file.path("data", "yft", "grid", "full")

## hindcast result plot
## Nan looking at the results of the hindcast for YFT
library(FLR4MFCL)
library(ggplot2)
library(tidyverse)
## swift from stoc or fix

## director for retrospective 
sadir <- file.path("data", "yft", "result", "fix_rec")
model_name <-list.dirs(sadir,recursive = FALSE,full.names = FALSE)
for (model in model_name){
  print(model)
  # Read IN full frq file
  fullfrq <- read.MFCLFrq(file.path(yft_full_grid, model, "yft.frq"))
  newfrq_all<-realisations(fullfrq) %>% filter(fishery %in% c(33:37)) %>%
    mutate(cpue=catch/effort,yrqtr=year+(month+1)/12-0.125)
  newfrq_all$retro_year <- "2021"
  # read in real rep
  realrep <- read.MFCLRep(file.path(yft_full_grid, model, "plot-final.par.rep"))
  plot.sbsbf0<- as.data.frame(SBSBF0recent(realrep))
  plot.sbsbf0$retro_year <-"2021"
  # recruitment 
  plot.rec<-as.data.frame(areaSums(seasonSums(popN(realrep)[1,])))
  plot.rec$retro_year <-"2021"
  # adult biomass
  plot.adubiomass <- as.data.frame(seasonSums(popN(realrep)))
  plot.adubiomass$retro_year <-"2021"
  for (yr in c(2014:2020)){
    print(yr)
  pseudorep <- read.MFCLRep(file.path(sadir, model, paste0(yr,".rep")))
  ## sbsbf0
  data.sbsbf0 <- as.data.frame(SBSBF0recent(pseudorep))
  data.sbsbf0$retro_year <- yr 
  plot.sbsbf0 <- rbind(plot.sbsbf0,data.sbsbf0)
  ## cpue
  load(file.path(sadir, model, paste0(yr,"_pseudo.Rdata")))
  newfrq<-realisations(pseudo) %>% filter(fishery %in% c(33:37)) %>%
    mutate(cpue=catch/effort,yrqtr=year+(month+1)/12-0.125)
  newfrq$retro_year <- yr
  newfrq_all<-rbind(newfrq_all,newfrq)
  ## recruitment
  data.rec <- as.data.frame(areaSums(seasonSums(popN(pseudorep)[1,]))) 
  data.rec$retro_year <- yr
  plot.rec <- rbind(plot.rec,data.rec)
  # adult biomass
  data.adubiomass <- as.data.frame(seasonSums(popN(pseudorep)))
  data.adubiomass$retro_year <-yr
  plot.adubiomass <- rbind(plot.adubiomass,data.adubiomass)  
  }
  
  png(paste0("./fix_rec/sbsbf0recent/",model,".png"),width =1000 , height = 800)
  print(ggplot(subset(plot.sbsbf0,year>=1960& year<=plot.sbsbf0$retro_year),aes(x=year,y=data,color=retro_year))+geom_line(size=1)+
          geom_line(data=plot.sbsbf0[plot.sbsbf0$year>= plot.sbsbf0$retro_year, ], linetype="twodash",size=1.2) +
        geom_line(data=plot.sbsbf0[plot.sbsbf0$retro_year==2021, ], color="#666666",size=1)+
    scale_color_brewer(palette = "Accent")+ theme_bw()  + xlab("Year")  + ylab("Depletion\n(SBrecnet/SBF=0)") + 
    theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
          axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model)) + guides(lty="none") 
  dev.off()
  
  png(paste0("./fix_rec/cpue/",model,".png"),width =1500 , height = 1200)
  print(ggplot(subset(newfrq_all,year>=2010& year<=newfrq_all$retro_year),aes(x=yrqtr,y=cpue,color=retro_year))+geom_line(size=1.3)+
          geom_line(data=newfrq_all[newfrq_all$year>= newfrq_all$retro_year, ], linetype="twodash",size=1.2) +
          geom_line(data=newfrq_all[(newfrq_all$retro_year==2021)&(newfrq_all$year>=2010), ], color="#666666",size=1)+
          scale_color_brewer(palette = "Accent")+ facet_wrap(fishery~.,scales = "free",ncol = 2)+theme_bw()  + xlab("Year")  + ylab("CPUE") + 
    theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
          axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model)) + guides(lty="none")
  dev.off()
## not going to reported for the stoc yet
#   png(paste0("./fix_rec/recruitment/",model,".png"),width =1000 , height = 800)
#   print(ggplot(data=plot.rec, aes(x=year,y=data,color=factor(retro_year,levels=c(2014:2021)))) +geom_line(size=1)+
#     scale_color_brewer(palette = "Accent")+  theme_bw()  + xlab("Year")  + ylab("Recruitment") + 
#     theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=10),
#           axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model))
#   dev.off()
#   png(paste0("./fix_rec/adu_bio/",model,"_area3_popN.png"),width =1000 , height = 800)
#   print(ggplot(subset(plot.adubiomass,year>=1980&area==3),aes(x=year,y=data,color=retro_year))+geom_line()+
#           scale_color_brewer(palette = "Accent")+ facet_wrap(age~.,scales = "free",ncol = 5)+theme_bw()  + xlab("Year")  + ylab("adultbiomass") + 
#           theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=10),
#                 axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model)) 
# dev.off()  

}
