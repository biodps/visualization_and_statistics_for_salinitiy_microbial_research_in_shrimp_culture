library(linkET)
library(ggplot2)
library(dplyr)
##############fig4######
varechem<-read.csv("varechem.csv",row.names = 1)
varespec<-read.csv("varespec.csv",row.names = 1)
#mantel
mantel <- mantel_test(varechem,varespec,
                      spec_select = list(S0=1:6,
                                         S15=7:12,
                                         S30=13:18))

mantel <- mutate(mantel, rd = cut(abs(r), breaks = c(-Inf, 0.2, 0.4, Inf),
                                  labels = c("<0.2", "0.2-0.4", ">= 0.4")), 
                 pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                          labels = c("< 0.01",  "0.01 - 0.05", ">= 0.05")))

qcorrplot(correlate(varespec),type = "lower", diag = FALSE) + 
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), 
              data = mantel, curvature = nice_curvature()) + 
  scale_fill_gradientn(colours =RColorBrewer::brewer.pal(11, "RdBu")) + 
  scale_size_manual(values = c(0.5, 1, 2)) + 
  scale_colour_manual(values = color_pal(3)) + 
  guides(size = guide_legend(title ="Mantel's r", 
                             override.aes =list(colour = "grey23"), order = 2), 
         colour = guide_legend(title ="Mantel's p", override.aes =list(size = 3), order = 1), 
         fill = guide_colorbar(title ="Pearson's r", order = 3))
