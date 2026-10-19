# Run from the project root. Input directories are configurable.
yft_full_grid <- file.path("data", "yft", "grid", "full")

## hindcast result plot
## Nan looking at the results of the hindcast for yft
library(FLR4MFCL)
library(ggplot2)
library(tidyverse)
## swift from stoc or fix

## director for retrospective 
sadir <- file.path("data", "yft", "result", "stoc_rec")
model_name <-gsub(".cpue_sim.all.csv","",list.files(sadir,pattern=".cpue_sim.all.csv"))
for (model in model_name){
  print(model)
  # Read IN full frq file
  fullfrq <- read.MFCLFrq(file.path(yft_full_grid, model, "yft.frq"))
  real.cpue <-realisations(fullfrq) %>% filter(fishery %in% c(33:37)) %>%
    mutate(median=catch/effort,yrqtr=year+(month+1)/12-0.125) %>%
    select(yrqtr,median,fishery)
  real.cpue$retro_year <- 2021
  real.cpue$ten<-NA
  real.cpue$nine<-NA  
  # catch 
  # real.cateff <-realisations(fullfrq) %>% filter(fishery %in% c(33:37)) %>%
  #   mutate(yrqtr=year+(month+1)/12-0.125) %>%
  #   select(yrqtr,catch,effort,fishery)
  # real.cateff$retro_year <- 2021
  # real.cateff$ten<-NA
  # real.cateff$nine<-NA
  
  # read in real rep
  realrep <- read.MFCLRep(file.path(yft_full_grid, model, "plot-final.par.rep"))
  real.sbsbf0 <- as.data.frame(SBSBF0recent(realrep)) %>%
    select(year,data) %>%
    rename(median=data)
  real.sbsbf0$retro_year <-2021
  real.sbsbf0$ten<-NA
  real.sbsbf0$nine<-NA
  ##sb
  real.sb <- as.data.frame(SB(realrep)) %>%
    select(year,data) %>%
    rename(median=data)
  real.sb$retro_year <-2021
  real.sb$ten<-NA
  real.sb$nine<-NA
  # recruitment 
  # real.rec<-as.data.frame(areaSums(seasonSums(popN(realrep)[1,]))) %>%
  #   select(year,data) %>%
  #   rename(median=data)
  # real.rec$retro_year <-2021
  # real.rec$ten.eff<-NA
  # real.rec$nine.eff<-NA
  # real.rec$ten.cat<-NA
  # real.rec$nine.cat<-NA

  # adult biomass
  sb.all <- read.csv(file.path(sadir, paste0(model,".sb.all.csv")),row.names = 1)
  sb.all <- sb.all[-1,]
  # sbsbf0
  sbsbf0.all <- read.csv(file.path(sadir, paste0(model,".sbsbf0.all.csv")),row.names = 1)
  sbsbf0.all<-sbsbf0.all[c(2:nrow(sbsbf0.all)),]
  ## cpue from frq file
  # cpue.all <- read.csv(file.path(sadir, paste0(model,".cpue.all.csv")),row.names = 1)
  # cpue.all<-cpue.all[c(2:nrow(cpue.all)),]
  ## cpue from cpue_sim file
  cpue.sim.all <- read.csv(file.path(sadir, paste0(model,".cpue_sim.all.csv")),row.names = 1)
  cpue.sim.all<-cpue.sim.all[c(2:nrow(cpue.sim.all)),]
  ## recruitment 
  # rec.all  <- read.csv(file.path(sadir, paste0(model,".rec.all.csv")),row.names = 1)
  # rec.all <- rec.all[c(2:nrow(rec.all)),]
  # sb
  plot.sb <- sb.all %>% group_by(year,retro_year) %>% 
    summarise(median=quantile(data,0.5,na.rm=TRUE),nine=quantile(data,0.9,na.rm=TRUE),ten=quantile(data,0.1,na.rm=TRUE))
  plot.sb <-rbind(plot.sb,real.sb)
  # sbsbf0
   plot.sbsbf0 <- sbsbf0.all %>% group_by(year,retro_year) %>% 
    summarise(median=quantile(data,0.5,na.rm=TRUE),nine=quantile(data,0.9,na.rm=TRUE),ten=quantile(data,0.1,na.rm=TRUE))
   plot.sbsbf0 <-rbind(plot.sbsbf0,real.sbsbf0)
  # cpue
   # plot.cpue <- cpue.all %>% group_by(yrqtr,retro_year,fishery) %>% 
   #   summarise(median=quantile(cpue,0.5,na.rm=TRUE),nine=quantile(cpue,0.9,na.rm=TRUE),ten=quantile(cpue,0.1,na.rm=TRUE))
   # plot.cpue <-rbind(plot.cpue,real.cpue)
  # cpue.sim
   plot.sim.cpue <- cpue.sim.all %>% group_by(yrqtr,retro_year,fishery) %>% 
     summarise(median=quantile(cpue,0.5,na.rm=TRUE),nine=quantile(cpue,0.9,na.rm=TRUE),ten=quantile(cpue,0.1,na.rm=TRUE))
   plot.sim.cpue <-rbind(plot.sim.cpue,real.cpue)   %>%
     mutate(region=paste0("Region ",fishery-32))
    # catch
   # plot.cateff <- cpue.all %>% 
   # ##  mutate(effort=effort*100000000000) %>%
   #   group_by(yrqtr,retro_year,fishery) %>%
   #   summarise(med.cat=quantile(catch,0.5),nine.cat=quantile(catch,0.9,na.rm=TRUE),ten.cat=quantile(catch,0.1,na.rm=TRUE),
   #             med.eff=quantile(effort,0.5),nine.eff=quantile(effort,0.9),ten.eff=quantile(effort,0.1))
   # plot.cateff <-rbind(plot.cateff,real.cateff)
   
   
  # rec
  #  plot.rec <- rec.all %>% group_by(year,retro_year) %>% 
  #    summarise(median=quantile(data,0.5,na.rm=TRUE),nine=quantile(data,0.9,na.rm=TRUE),ten=quantile(data,0.1,na.rm=TRUE))
  #  plot.rec <-rbind(plot.rec,real.rec)
  # 
   png(paste0("./stoc_rec/sbsbf0recent/",model,".png"),width =1000 , height = 800)
    print(ggplot(subset(plot.sbsbf0,year>=1960& year<=plot.sbsbf0$retro_year+1),aes(x=year,y=median))+geom_line(aes(color=factor(retro_year)),size=1,alpha=0.8)+
            geom_ribbon(data=subset(plot.sbsbf0,year>=1960),aes(ymin=ten,ymax=nine,fill=factor(retro_year)),alpha=0.4)+
            geom_line(data=plot.sbsbf0[plot.sbsbf0$year>= plot.sbsbf0$retro_year+1, ], aes(color=factor(retro_year)),linetype="twodash",size=1.2) +
            geom_line(data=plot.sbsbf0[plot.sbsbf0$retro_year==2021, ], color="#666666",size=1,alpha=0.8)+
        scale_color_brewer(palette = "Accent") + scale_fill_brewer(palette="Accent") +
              theme_bw()  + xlab("Year")  + ylab("Depletion\n(SBrecnet/SBF=0)") + 
    theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
          axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model) + guides(lty="none"))
  dev.off()
  ##sb
  ## plot sb
  png(paste0("./stoc_rec/sb/",model,".png"),width =1500 , height = 1200)
  print(ggplot(subset(plot.sb,year>=1960& year<=plot.sb$retro_year+1),aes(x=year,y=median))+geom_line(aes(color=factor(retro_year)),size=1,alpha=0.8)+
          geom_ribbon(data=subset(plot.sb,year>=1960),aes(ymin=ten,ymax=nine,fill=factor(retro_year)),alpha=0.4)+ 
          geom_line(data=plot.sb[plot.sb$year>= plot.sb$retro_year+1, ], aes(color=factor(retro_year)),linetype="twodash",size=1.2) +
          geom_line(data=plot.sb[plot.sb$retro_year==2021&plot.sb$year>=1960, ], color="#666666",size=1,alpha=0.8)+
          scale_color_brewer(palette = "Accent") + scale_fill_brewer(palette="Accent") +
          theme_bw()  + xlab("Year")  + ylab("Adult Biomass") +       
          theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
                axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model) + guides(lty="none"))
  dev.off()
  # png(paste0("./stoc_rec/cpue/",model,".png"),width =1500 , height = 1200)
  # print(ggplot(subset(plot.cpue,yrqtr>=2013),aes(x=yrqtr,y=median,color=factor(retro_year)))+
  #   geom_ribbon(aes(ymin=ten,ymax=nine,fill=factor(retro_year)),alpha=0.4)+geom_line(size=0.6,alpha=0.5)+ 
  #   scale_color_brewer(palette = "Accent")+ scale_fill_brewer(palette = "Accent") + facet_wrap(fishery~.,scales = "free",ncol = 3)+theme_bw()  + xlab("Year")  + ylab("CPUE") + 
  #   theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=10),
  #         axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model) + guides(lty="none"))
  # dev.off()
  
## cpue from cpue_sim
  ## replace fishery with region 

  
  png(paste0("./stoc_rec/cpue_sim/",model,".png"),width =1500 , height = 1200)
  print(ggplot(subset(plot.sim.cpue,yrqtr>=1960&yrqtr<=plot.sim.cpue$retro_year+0.125),aes(x=yrqtr,y=median))+geom_line(aes(color=factor(retro_year)),size=0.8,alpha=0.8)+
        geom_ribbon(data=subset(plot.sim.cpue,yrqtr>=2010),aes(ymin=ten,ymax=nine,fill=factor(retro_year)),alpha=0.4)+ 
          geom_line(data=plot.sim.cpue[plot.sim.cpue$yrqtr>= plot.sim.cpue$retro_year, ], aes(color=factor(retro_year)),linetype="twodash",size=1.2) +
           geom_line(data=plot.sim.cpue[plot.sim.cpue$retro_year==2021&plot.sim.cpue$yrqtr>=1960, ], color="#666666",size=0.8,alpha=0.8)+
          scale_color_brewer(palette = "Accent")+ scale_fill_brewer(palette = "Accent") + facet_wrap(region~.,scales = "free",ncol = 2)+theme_bw()  + xlab("Year")  + ylab("CPUE") + 
          theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
                axis.title=element_text(size=20),plot.title = element_text(size = 30),  strip.text = element_text(size = 20)) + ggtitle(model) + guides(lty="none"))
  dev.off()

## not going to reported for the stoc yet
  # png(paste0("./stoc_rec/recruitment/",model,".png"),width =1000 , height = 800)
  # 
  # print(ggplot(plot.rec,aes(x=year,y=median,color=factor(retro_year)))+
  #   geom_ribbon(aes(ymin=ten,ymax=nine,fill=factor(retro_year)),alpha=0.4)+geom_line(size=1,alpha=0.5)+ 
  #   scale_color_brewer(palette = "Accent") + scale_fill_brewer(palette="Accent") +
  #   theme_bw()  + xlab("Year")  + ylab("Recruitment") +       
  #   theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=10),
  #         axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model) + guides(lty="none"))
  # dev.off()
  # catch
  # png(paste0("./stoc_rec/catch/",model,".png"),width =1500 , height = 1200)
  # print(ggplot(subset(plot.cateff,yrqtr>=2013),aes(x=yrqtr,y=med.cat,color=factor(retro_year)))+
  #         geom_ribbon(aes(ymin=ten.cat,ymax=nine.cat,fill=factor(retro_year)),alpha=0.4)+geom_line(size=0.6,alpha=0.5)+
  #         scale_color_brewer(palette = "Accent")+ scale_fill_brewer(palette = "Accent") + facet_wrap(fishery~.,scales = "free",ncol = 3)+theme_bw()  + xlab("Year")  + ylab("CPUE") + 
  #         theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=10),
  #               axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model) + guides(lty="none"))
  # dev.off()
  # 
  # # effort
  # png(paste0("./stoc_rec/effort/",model,".png"),width =1500 , height = 1200)
  # print(ggplot(subset(plot.cateff,yrqtr>=2013),aes(x=yrqtr,y=med.eff,color=factor(retro_year)))+
  #         geom_ribbon(aes(ymin=ten.eff,ymax=nine.eff,fill=factor(retro_year)),alpha=0.4)+geom_line(size=0.6,alpha=0.5)+
  #         scale_color_brewer(palette = "Accent")+ scale_fill_brewer(palette = "Accent") + facet_wrap(fishery~.,scales = "free",ncol = 3)+theme_bw()  + xlab("Year")  + ylab("CPUE") + 
  #         theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=10),
  #               axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(model) + guides(lty="none"))
  # dev.off()
  
  
}
