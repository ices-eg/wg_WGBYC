library(ggplot2)
library(tidyverse)
library(data.table)
### Input data ##
df<-rateseverythingwithCI %>% filter(das>49, Ecoregion=="Celtic Seas")
### Birds

df5<-df %>% filter(Species%in%c("Uria aalge","Morus bassanus","Fulmarus glacialis","Larus argentatus","Phalacrocorax carbo"))
### Mammals

dfm5<-df %>% filter(Species %in%c("Halichoerus gryphus","Phoca vitulina","Delphinus delphis","Phocoena phocoena","Globicephala melas"))
### sharks
dfs5<-df %>% filter(Species %in%c("Amblyraja radiata","Torpedo marmorata","Etmopterus spinax","Deania calceus","Hexanchus griseus"))
##fish
dff5<-df %>% filter(Species %in%c("Helicolenus dactylopterus","Zeus faber","Chelidonichthys lucerna","Chimaera monstrosa","Scophthalmus maximus"))

## Plot Birds ##
colourCount = length(unique(df5$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdf5<-ggplot(df5,aes(Metier4,y=rate,fill=Species))+
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

plotdf5

## Plot mammals ##
colourCount = length(unique(dfm5$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdfm5<-ggplot(dfm5,aes(Metier4,y=rate,fill=Species))+
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
plotdfm5
## Plot sharks and rays
colourCount = length(unique(dfs5$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdfs5<-ggplot(dfs5,aes(Metier4,y=rate,fill=Species))+
  geom_bar(stat="identity",position='stack') +
  facet_wrap(~Ecoregion,scales = "free_x",nrow=2)+
  scale_fill_manual(name="",values = getPalette(colourCount))+
  #geom_text(aes(label=Species))+#, position=position_dodge(width=0.9),vjust=-0.1, hjust=-0.15)+
  #scale_fill_grey(start = 0.05, end = 0.5)+ 
  #coord_cartesian(ylim = c(0, 0.60))+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ylab("Reported elasmobranch bycatch per day at sea")+
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
plotdfs5

## Plot fish
colourCount = length(unique(dff5$Species))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
plotdff5<-ggplot(dff5,aes(Metier4,y=rate,fill=Species))+
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
plotdff5
