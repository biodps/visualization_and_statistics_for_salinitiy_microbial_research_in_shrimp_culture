
library(ggplot2)
library(ggExtra)
library(vegan)
library(ggthemes)

data <- read.table("otu.txt", header = TRUE, sep = "\t", row.names = 1)
dune.div <- adonis2(data ~ group, data = group, permutations = 999, method="bray")
dune.div
dune_adonis <- paste0("adonis R2: ",round(dune.div$R2,2), "; P-value: ", dune.div$`Pr(>F)`)
dune_adonis
####fig2A#######
p = ggplot(pcoa_result, aes(x=PCoA1, y=PCoA2, color=group)) +
  geom_point(aes(color=group),size=3)+
  labs(x = paste0("PCO1 (",pco1,"%)"),
       y = paste0("PCO2 (",pco2,"%)"),
       subtitle = dune_adonis)+
  scale_colour_manual(values = colour)+
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.grid = element_blank(),plot.title=element_text(hjust=0.5),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        axis.text = element_text(color = "black",size=10))+
  geom_hline(aes(yintercept=0), colour="#BEBEBE", linetype="dashed")+
  geom_vline(aes(xintercept=0), colour="#BEBEBE", linetype="dashed")
p
#####fig2B#######
plotdata<-read.csv("env.csv",row.names = 1)
p1 = ggplot(plotdata,aes(x=CCA1,y=CCA2,color=group))+  
  geom_point(size=6,position = position_jitter(width = 0.1, height = 0.1))+  
  geom_segment(data=df_rda_env,aes(x=0,y=0,xend=df_rda_env[,1]*1.4,yend=df_rda_env[,2]*1.4), 
               arrow = arrow(length = unit(0.01, 'npc')),color="gray2",size=0.68)+  
  geom_text(data=df_rda_env,aes(x=df_rda_env[,1]*1.7,
                                y=df_rda_env[,2]*1.7,
                                label=rownames(df_rda_env)),
            size=5.5,color="gray2")+  
  labs(x=paste0("CCA1 (",CCA1,"%)"),
       y=paste0("CCA2 (",CCA2,"%)"))+  
  theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+  
  scale_color_manual(values=colour)+  
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  mytheme+
  geom_hline(aes(yintercept=0), colour="gray45",size=0.8, linetype="dashed")+ 
  geom_vline(aes(xintercept=0), colour="gray45",size=0.8, linetype="dashed")