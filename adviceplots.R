library(ggplot2)
library(tidyverse)
library(data.table)
### Input data ##
rateseverythingwithCI<-read.csv("ratesmammalsbirdsreptilefishwithCI.csv")

df<-rateseverythingwithCI %>% filter(das>49)

df<-df %>% group_by(Ecoregion,Metier4,class,Species) %>% summarize(das=sum(das),animals=sum(animals),rate=sum(rate),CI5=sum(CI5),CI95=sum(CI95))

### Birds

df50<-df %>% filter(class=="b")
### Mammals

dfm50<-df %>% filter(class=="m")
### Reptiles

dfr50<-df %>% filter(class=="r")

### Fish

dff50<-df %>% filter(class=="f")

## Plot Birds ##
colourCount = length(unique(df50$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdf50<-ggplot(df50,aes(Metier4,y=rate,fill=Species))+
  geom_bar(stat="identity",position='stack') +
  facet_wrap(~Ecoregion,scales = "free_x",nrow=2)+
  scale_fill_manual(name="",values = getPalette(colourCount))+
  #geom_text(aes(label=Species))+#, position=position_dodge(width=0.9),vjust=-0.1, hjust=-0.15)+
  #scale_fill_grey(start = 0.05, end = 0.5)+ 
  #coord_cartesian(ylim = c(0, 0.60))+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ylab("Reported seabird bycatch per day at sea")+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=14, face="bold"), 
        legend.text=element_text(size=14),
        axis.text.x=element_text(angle=-90, hjust=1, size=14),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size = 14, face = "bold"),
        axis.title.y=element_text(size = 14, face = "bold"),
        strip.text.x=element_text(size = 14),
        strip.text.y=element_text(size = 14))+
  theme(legend.background = element_rect(fill="white",colour="white"))+
  theme(legend.position="top")+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(fill=NA,linetype = "dashed", colour = "black")
    #, legend.position="none"
  )+
  theme(axis.line = element_line(color = 'black'))

plotdf50

## Plot mammals ##
colourCount = length(unique(dfm50$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdfm50<-ggplot(dfm50,aes(Metier4,y=rate,fill=Species))+
  geom_bar(stat="identity",position='stack') +
  facet_wrap(~Ecoregion,scales = "free_x",nrow = 2)+
  scale_fill_manual(name="",values = getPalette(colourCount))+
  #geom_text(aes(label=Species))+#, position=position_dodge(width=0.9),vjust=-0.1, hjust=-0.15)+
  #scale_fill_grey(start = 0.05, end = 0.5)+ 
  #coord_cartesian(ylim = c(0, 0.60))+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ylab("Reported marine mammal bycatch per day at sea")+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=14, face="bold"), 
        legend.text=element_text(size=14),
        axis.text.x=element_text(angle=-90, hjust=1, size=14),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size = 14, face = "bold"),
        axis.title.y=element_text(size = 14, face = "bold"),
        strip.text.x=element_text(size = 14),
        strip.text.y=element_text(size = 14))+
  theme(legend.background = element_rect(fill="white",colour="white"))+
  theme(legend.position="top")+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(fill=NA,linetype = "dashed", colour = "black")
    #, legend.position="none"
  )+
  theme(axis.line = element_line(color = 'black'))
plotdfm50
## Plot reptiles
colourCount = length(unique(dfr50$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdfr50<-ggplot(dfr50,aes(Metier4,y=rate,fill=Species))+
  geom_bar(stat="identity",position='stack') +
  facet_wrap(~Ecoregion,scales = "free_x",nrow=2)+
  scale_fill_manual(name="",values = getPalette(colourCount))+
  #geom_text(aes(label=Species))+#, position=position_dodge(width=0.9),vjust=-0.1, hjust=-0.15)+
  #scale_fill_grey(start = 0.05, end = 0.5)+ 
  #coord_cartesian(ylim = c(0, 0.60))+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ylab("Reported turtle bycatch per day at sea")+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=14, face="bold"), 
        legend.text=element_text(size=14),
        axis.text.x=element_text(angle=-90, hjust=1, size=14),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size = 14, face = "bold"),
        axis.title.y=element_text(size = 14, face = "bold"),
        strip.text.x=element_text(size = 14),
        strip.text.y=element_text(size = 14))+
  theme(legend.background = element_rect(fill="white",colour="white"))+
  theme(legend.position="top")+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(fill=NA,linetype = "dashed", colour = "black")
    #, legend.position="none"
  )+
  theme(axis.line = element_line(color = 'black'))
plotdfr50


## Plot fish
colourCount = length(unique(dff50$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdff50<-ggplot(dff50,aes(Metier4,y=rate,fill=Species))+
  geom_bar(stat="identity",position='stack') +
  facet_wrap(~Ecoregion,scales = "free_x",nrow=2)+
  scale_fill_manual(name="",values = getPalette(colourCount))+
  #geom_text(aes(label=Species))+#, position=position_dodge(width=0.9),vjust=-0.1, hjust=-0.15)+
  #scale_fill_grey(start = 0.05, end = 0.5)+ 
  #coord_cartesian(ylim = c(0, 0.60))+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ylab("Reported fish bycatch per day at sea")+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=14, face="bold"), 
        legend.text=element_text(size=14),
        axis.text.x=element_text(angle=-90, hjust=1, size=14),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size = 14, face = "bold"),
        axis.title.y=element_text(size = 14, face = "bold"),
        strip.text.x=element_text(size = 14),
        strip.text.y=element_text(size = 14))+
  theme(legend.background = element_rect(fill="white",colour="white"))+
  theme(legend.position="top")+
  theme(
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(fill=NA,linetype = "dashed", colour = "black")
    #, legend.position="none"
  )+
  theme(axis.line = element_line(color = 'black'))
plotdff50
