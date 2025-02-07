library(gridExtra)
library(ggplot2)
library(grid)
source("H_opt.R")

set.seed(1)

a<-seq(2,10,0.5)
b<-seq(2,10,0.5)

par(mfrow = c(1, 2))

H_a<-hopt_versus_a(10^6,a,7,5)


dataset_a<-data.frame(
  values_a=a,
  h_ott_a=H_a
)

H_b<-hopt_versus_b(10^6,5,b,5)

dataset_b<-data.frame(
  values_b=b,
  h_ott_b=H_b
)

p1<-ggplot(dataset_a, aes(x = values_a, y = h_ott_a)) +
  geom_point(color='#21636A',size=2.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,color='#27D2C0') +  
  labs(title = NULL, x = expression(alpha), y = "h optimal")+
  ylim(c(min(min(H_a),min(H_b))-0.0002,max(max(H_a),max(H_b))+0.0002))+
  theme_minimal() +  
  theme(
    #panel.grid = element_blank(),  
    panel.border = element_rect(colour = "black", fill = NA),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    )
  ) +
  annotate("text", x = 8.2, y = min(H_a)+0.0008, label = expression(beta == 7), hjust = 0,family = "sans",size=3)  

p2<-ggplot(dataset_b, aes(x = values_b, y = h_ott_b)) +
  geom_point(color='#21636A',size=2.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE,color='#27D2C0') +  
  labs(title = NULL, x = expression(beta), y = NULL)+
  ylim(c(min(min(H_a),min(H_b))-0.0002,max(max(H_a),max(H_b))+0.0002))+
  theme_minimal() +  
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    )
  ) +
  annotate("text", x = 8.4, y = min(H_a)+0.0008, label = expression(alpha == 5), hjust = 0,family = "sans",size=3)  


caption <- textGrob("n = 10^6\nN = 5", gp = gpar(fontsize = 11, fontfamily = "sans"), just = "center",
                    y = unit(0.6, "npc")) 

grid.arrange(p1, p2, ncol = 2, right = caption)

