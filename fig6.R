######fig6A,B#####
data <- read.csv('Test.ProcessImportance_EachGroup.csv')
ggplot(data, aes(x=Group,y= y,fill =  sub)) + 
  geom_bar(position = 'stack', stat = 'identity', color = 'black', width = 0.95) +
  labs(y =expression('Relative Abundance'), x = expression(''), title = "") +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(title = ''),direction = 'horizontal') +
  scale_fill_manual(values = c("#1072BD","#77AE43","#EDB021","#D7592C","#7F318D")) + 
  theme_classic() +
 theme(plot.margin=unit(rep(1,4),'lines')) +
  theme(text= element_text(size = 16,
                           color = 'black')) +
  theme(legend.background = element_blank(),
        legend.text = element_text(size = 12)) +
  theme(legend.key = element_blank()) +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line(color = 'black', lineend = 'square'),
        axis.text.x = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), 'cm'),size = 16,colour = 'black'),
        axis.text.y = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), 'cm'),size = 16,colour = 'black'),
        axis.title = element_text())
######fig6C,D####
library(ggvenn)
df <-  read.delim("otu.txt",row.names = 1, sep = '\t', stringsAsFactors = F, check.names = F)
ggvenn(df,fill_color= brewer.pal(5, "Dark2")[1:2],
       fill_alpha =  .7,stroke_linetype = "longdash",set_name_size = 8,
       text_size=5) 
ggsave("Venn.pdf",width = 4, height = 4)
