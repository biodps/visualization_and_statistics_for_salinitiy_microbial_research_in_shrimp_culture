#########RA Genetic testing##########
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(ggalluvial)
library(vegan)
library(patchwork)
df<-read.csv("qpcr-RA.csv",header = 1)

library(agricolae)
MC1 <- LSD.test(result1, 'Group', p.adj = 'bonferroni')
MC2 <- LSD.test(result2, 'Group', p.adj = 'bonferroni')
print(MC1$groups)
print(MC2$groups)
colour<-c("#014f9c","#bde3f9","#c65323","#fab590","#2f9969","#d4f9a9","#bc98d9","#e4b9d9")
p1<-ggplot(data=df,aes(x=group,y=pit,colour = group))+ 
  geom_violin(alpha = 0.6,
              scale = 'width',
              linewidth = 0.2, 
              trim = TRUE)+ 
  geom_boxplot(mapping=aes(x=group,y=pit,colour=group,fill=group), 
               alpha = 0.6, size=1, width = 0.3)+ 
  geom_jitter(mapping=aes(x=group,y=pit,colour = group), 
              alpha = 0.3,size=3)+ 
  scale_fill_manual(
    values =colour)+ 
  scale_color_manual(
    values=colour)+ 
  facet_wrap(~ type, scales = "free_x",ncol=2) +
  theme_bw()+
  theme(legend.position ="none", panel.border = element_rect(fill=NA,color="black", linewidth = 1, linetype="solid"),#设置边框 
        strip.text = element_text(size = 14, face = "bold", colour = "black"),
        axis.text = element_text(size=17), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=17))+
  labs(x="pit", y=NULL) 

cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, nrow= 4 )