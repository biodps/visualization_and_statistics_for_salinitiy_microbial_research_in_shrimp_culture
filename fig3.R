library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(ggalluvial)
library(vegan)
library(patchwork)
rm(list = ls())
####fig3A#######
data<-read.table("AR-otu.txt", header=T, sep="\t",  stringsAsFactors=F,row.names = 1)

group <- read.csv('group.csv',header = 1)
mycolors=c(brewer.pal(7,"Set1"),brewer.pal(8, "Accent"))
ggplot(bb , aes(x =variable, y = value,                 
                    fill =Taxonomy,stratum =Taxonomy, alluvium =Taxonomy)) +  
  geom_stratum(width = 0.8,alpha=1,color="white") +
  geom_flow(alpha = 0.01) +   
  scale_fill_manual(values=mycolors)+  
  theme(axis.text=element_text(colour='black',size=9))+  
  labs(x = '', y = 'Relative abundance',fill=" ")+
  theme_bw()+  
  theme(axis.text.y =element_text(colour='black',size=9),
        axis.text.x =element_text(colour='black',size=11, angle=90) )+  
  theme(strip.text = element_text(size = 13,face = 'bold',colour = "gray2"))+  
  facet_grid(~group, drop=TRUE,scale="free",space="free_x")

####fig3B#######
data<-read.table("AR-otu.txt", header=T, sep="\t",  stringsAsFactors=F,row.names = 1)

group <- read.csv('group.csv',header = 1)
mycolors=c(brewer.pal(7,"Set1"),brewer.pal(8, "Accent"))
ggplot(bb , aes(x =variable, y = value,                 
                fill =Taxonomy,stratum =Taxonomy, alluvium =Taxonomy)) +  
  geom_stratum(width = 0.8,alpha=1,color="white") +
  geom_flow(alpha = 0.01) +   
  scale_fill_manual(values=mycolors)+  
  theme(axis.text=element_text(colour='black',size=9))+  
  labs(x = '', y = 'Relative abundance',fill=" ")+
  theme_bw()+  
  theme(axis.text.y =element_text(colour='black',size=9),
        axis.text.x =element_text(colour='black',size=11, angle=90) )+  
  theme(strip.text = element_text(size = 13,face = 'bold',colour = "gray2"))+  
  facet_grid(~group, drop=TRUE,scale="free",space="free_x")


######fig3C######
library(tidyverse)
library(microeco)
library(magrittr)

otu <-  read.csv(file.choose(), row.names = 1)
group <-  read.csv(file.choose(), row.names = 1)
tax <-  read.csv(file.choose(), row.names = 1)

dataset <- microtable$new(sample_table = sample,
                          otu_table = OTU, 
                          tax_table =Tax)
#开始LEfse分析
lefse <- trans_diff$new(dataset = dataset, 
                        method = "lefse", 
                        group = "group")


col=c("#014f9c","#bde3f9","#c65323","#fab590","#2f9969","#d4f9a9")
col<-c("#6fb2e3",  "#eee461","#dca237", "#469c76")
p1<-lefse$plot_diff_bar(use_number = 1:30, 
                        color_values = col,
                        axis_text_y=17,
                        width = 0.8,
                        threshold=4.5,
                        keep_prefix = T)
####fig3D######
library(RColorBrewer)

p2<-lefse$plot_diff_cladogram(color = col,
                              use_taxa_num = 50, 
                              use_feature_num = 50,
                              clade_label_size = 0
)