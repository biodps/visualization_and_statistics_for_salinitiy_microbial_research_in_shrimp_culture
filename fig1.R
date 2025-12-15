library(vegan)
library(picante)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggprism)
library(ggsignif)
df2 <- read.delim("index.txt",row.names = 1, sep = '\t', stringsAsFactors = F, check.names = F)
colour<-c("#014f9c","#bde3f9","#c65323","#fab590","#2f9969","#d4f9a9","#bc98d9","#e4b9d9")
#fig1A########
A <- ggplot(data=shannon,aes(x=group,y=Shannon,colour = group))+ 
  geom_violin(alpha = 0.6,
              scale = 'width',
              linewidth = 1, 
              trim = TRUE)+
  geom_boxplot(mapping=aes(x=group,y=Shannon,colour=group,fill=group), 
               alpha = 0.6, size=1, width = 0.3)+ 
  geom_jitter(mapping=aes(x=group,y=Shannon,colour = group), 
              alpha = 0.3,size=3)+ 
  scale_fill_manual(
    values =colour)+ 
  scale_color_manual(
    values=colour)+ 
  stat_compare_means(size = 8,
    label.y = 8,
    label.x = 2,
    tip.length = 4)+
  theme_bw()+
  theme(legend.position ="none", panel.border = element_rect(fill=NA,color="black", linewidth = 1, linetype="solid"),#设置边框 
        axis.text = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20))+
  labs(x=NULL, y="Shannon") 
A
#fig1B####
B <- ggplot(data=Richness,aes(x=group,y=Richness,colour = group))+ 
  geom_violin(alpha = 0.6,
              scale = 'width', 
              linewidth = 1, 
              trim = TRUE)+ 
  geom_boxplot(mapping=aes(x=group,y=Richness,colour=group,fill=group), 
               alpha = 0.6, size=1, width = 0.3)+ 
  geom_jitter(mapping=aes(x=group,y=Richness,colour = group), 
              alpha = 0.3,size=3)+ 
  scale_fill_manual(
    values =colour)+ 
  scale_color_manual(
    values=colour)+ 
  stat_compare_means(
    size = 8,
    label.y = 1800,
    label.x = 2,
    tip.length = 4)+ theme_bw() + 
  theme(legend.position = "none", 
        panel.border = element_rect(fill = NA, color = "black", 
                                    linewidth = 1, linetype = "solid"), 
        axis.text = element_text(size = 20), 
        axis.title.x = element_text(size = 20,), 
        axis.title.y = element_text(size = 20,))+
  labs(x = NULL, y = "Richness")
B

#fig1C######
C <- ggplot(data=PD_whole_tree,aes(x=group,y=PD_whole_tree,colour = group))+ 
  geom_violin(alpha = 0.6,
              scale = 'width',
              linewidth = 1, 
              trim = TRUE)+
  geom_boxplot(mapping=aes(x=group,y=PD_whole_tree,colour=group,fill=group), 
               alpha = 0.6, size=1, width = 0.3)+ 
  geom_jitter(mapping=aes(x=group,y=PD_whole_tree,colour = group), 
              alpha = 0.3,size=3)+ 
  scale_fill_manual(
    values =colour)+ 
  scale_color_manual(
    values=colour)+ 
  stat_compare_means(
    size = 8,
    label.y = 180,
    label.x = 2,
    tip.length = 4)+
  theme_bw() + 
  theme(legend.position = "none", 
        panel.border = element_rect(fill = NA, color = "black", 
                                    linewidth = 1, linetype = "solid"), 
        axis.text = element_text(size = 20), 
        axis.title.x = element_text(size = 20,), 
        axis.title.y = element_text(size = 20,))+
  labs(x=NULL, y="PD_whole_tree")
C
###fig1D#####
D <- ggplot(data=Evenness,aes(x=group,y=Evenness,colour = group))+ 
  geom_violin(alpha = 0.6,
              scale = 'width',
              linewidth = 1, 
              trim = TRUE)+
  geom_boxplot(mapping=aes(x=group,y=Evenness,colour=group,fill=group), 
               alpha = 0.6, size=1, width = 0.3)+ 
  geom_jitter(mapping=aes(x=group,y=Evenness,colour = group),
              alpha = 0.3,size=3)+ 
  scale_fill_manual(
    values =colour)+ 
  scale_color_manual(
    values=colour)+ 
  stat_compare_means(
    size = 8,
    label.y = 1,
    label.x = 2,
    tip.length = 4)+ theme_bw() + 
  theme(legend.position = "none", 
        panel.border = element_rect(fill = NA, color = "black", 
                                    linewidth = 1, linetype = "solid"), 
        axis.text = element_text(size = 20), 
        axis.title.x = element_text(size = 20,), 
        axis.title.y = element_text(size = 20,))+
  labs(x=NULL, y="Evenness")
D