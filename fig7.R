#####fig7A########
df<-read.csv("plspm.csv",header = 1,row.names = 1)
library(plspm)
str(df)
path_matrix <- matrix(
  c(0, 0, 0, 0, 0,0,  
    1, 0, 0, 0, 0,0,    
    1, 1, 0, 0, 0,0,    
    1, 1, 1, 0, 0,0,
    0, 0, 0, 1, 0,0,
    0, 0, 0, 0, 1,0),   
  nrow = 6, byrow = TRUE,
  dimnames = list(c("sal", "w_bac", "b_bac","Community", "Function","factor"),
                  c("sal", "w_bac", "b_bac","Community", "Function","factor"))
)

blocks <- list(
  sal = "salinity",
  w_bac = c("Shannonw","Simpsonw","PC1w"),
  b_bac = c("Shannonb","Simpsonb","PC1b"),
  # N_cycling=c("Ammonia.Assimilation","Ammonia.Oxidation", "Nitrification","Denitrification","Nitrogen.Fixation"),
  Community = c("Shannon", "Simpson","PC1"),
  # P_cycling = c("Phosphorus.activation","Phosphorus.uptake","P.starvation.response.regulation")
  Function=c("Ammonia.Assimilation","Ammonia.Oxidation", "Nitrification","Denitrification","Nitrogen.Fixation","Phosphorus.activation","Phosphorus.uptake","P.starvation.response.regulation"),
  factor=c("NH4","NO2","NO3","TP","pH")
)

pls_result <- plspm(
  Data = df,
  path_matrix = path_matrix,
  blocks = blocks,
  modes = c("A", "B", "B", "A", "B", "B")  
)
pls_result$effects[, 2:4] <- pls_result$effects[, 2:4] / 1.2
p <- innerplot(pls_result, colpos = 'red', colneg = 'blue', 
               show.values = TRUE, lcol = 'gray', box.lwd = 0)

######fig7B######
p<-ggplot(data=aa,aes(x=Type,y=value,fill=Type))+
  geom_bar(stat = 'identity', 
           position=position_dodge(width=0.9),width = 0.8,     
           color='black')+        
  labs(x=NULL,y=NULL)+ 
  theme_bw(base_size = 18)+ 
  facet_wrap( ~ relationships, scales = "free_x", nrow = 1)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1))+
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set2")) 
p